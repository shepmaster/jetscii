#![feature(asm)]
#![feature(const_fn)]
#![feature(pattern)]
#![cfg_attr(test, feature(test))]

//!
//! A tiny library to efficiently search strings for ASCII characters.
//!
//! ## Example
//! ```
//! use jetscii::AsciiChars;
//! let mut search = AsciiChars::new();
//! search.push(b'-');
//! search.push(b':');
//! let part_number = "86-J52:rev1";
//! let parts: Vec<_> = part_number.split(search.with_fallback(|c| {
//!     c == b'-' || c == b':'
//! })).collect();
//! assert_eq!(&parts, &["86", "J52", "rev1"]);
//! ```
//!
//! For maximum performance, you can create the searcher as a constant
//! item. Print an existing `AsciiChars` with the debug formatter to
//! get the appropriate invocation.
//!
//! ```
//! use jetscii::AsciiChars;
//! let search = AsciiChars::from_words(0x0000000000002d3a, 0, 2);
//! let part_number = "86-J52:rev1";
//! let parts: Vec<_> = part_number.split(search.with_fallback(|c| {
//!     c == b'-' || c == b':'
//! })).collect();
//! assert_eq!(&parts, &["86", "J52", "rev1"]);
//! ```

use std::fmt;
use std::str::pattern::{Pattern,Searcher,SearchStep};

const MAXBYTES: u8 = 16;

/// 8 ascii-only bytes
const ASCII_WORD_MASK: u64 = 0x7f7f7f7f7f7f7f7f;

/// Searches a string for a set of ASCII characters. Up to 16
/// characters may be used.
#[derive(Copy,Clone)]
pub struct AsciiChars {
    needle: u64,
    needle_hi: u64,
    count: u8,
}

#[cfg(all(feature = "unstable", target_arch = "x86_64"))]
enum InitialMatch {
    Complete(Option<usize>),
    Incomplete(usize),
}

trait PackedCompareOperation {
    // Returns a mask
    unsafe fn initial(&self, ptr: *const u8, offset: usize, len: usize) -> u64;
    // Returns an index
    unsafe fn body(&self, ptr: *const u8, offset: usize, len: usize) -> u32;
}

#[cfg(all(feature = "unstable", target_arch = "x86_64"))]
struct UnalignedByteSliceHandler<T> {
    operation: T,
}

#[cfg(all(feature = "unstable", target_arch = "x86_64"))]
impl<T> UnalignedByteSliceHandler<T>
    where T: PackedCompareOperation
{
    #[inline]
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn find(&self, haystack: &[u8]) -> Option<usize> {
        let mut len = haystack.len();

        if len == 0 { return None }

        // The PCMPxSTRx instructions always read 16 bytes worth of
        // data. To avoid walking off the end of a page (and
        // potentially into a protected area), we read in 16-byte
        // chunks aligned to the *end* of the byte slice. The
        // instructions handle truly unaligned access just fine; the
        // trick lies in searching the leftover bytes at the beginning
        // of the byte slice.

        let true_ptr = haystack.as_ptr();

        // Start at the 16-byte-aligned block *before* the byte slice
        // starts
        let ptr = (true_ptr as usize & !0xF) as *const u8;

        // Find where the byte slice really starts
        let initial_offset = true_ptr as usize & 0xF;
        let mut offset = initial_offset;

        // If the byte slice is magically aligned, skip this extra work
        if offset != 0 {
            match self.initial_unaligned_byte_slice(ptr, offset, len) {
                InitialMatch::Complete(result) => return result,
                InitialMatch::Incomplete(length_of_leading_slice) => {
                    offset = 16;
                    len -= length_of_leading_slice;
                }
            }
        }

        while len != 0 {
            let res: u32;

            res = unsafe { self.operation.body(ptr, offset, len) };

            // We know if it matched if the zero flag is set (or
            // unset?), we shouldn't need to test res...
            if res == 16 {
                offset += 16;
                len = len.saturating_sub(16);
            } else {
                return Some(res as usize + offset - initial_offset);
            }
        }

        None
    }

    #[inline]
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn initial_unaligned_byte_slice(&self, ptr: *const u8, offset: usize, len: usize) -> InitialMatch {
        // We use the PCMPESTRM instruction on the 16-byte-aligned
        // block that contains the *start* of the byte slice. This
        // returns a mask of all the matching bytes. We can can then
        // ignore unrelated leading bits to find the index of the
        // first related byte (if any).

        let mut matching_bytes = unsafe { self.operation.initial(ptr, offset, len) };

        // Ignore matches that occurred before our byte slice began
        matching_bytes >>= offset;

        if matching_bytes != 0 {
            // Matched somewhere in there, find the least significant bit
            let index = matching_bytes.trailing_zeros() as usize;
            return InitialMatch::Complete(Some(index));
        }

        let length_of_leading_slice = 16 - offset;

        if len < length_of_leading_slice {
            // We've searched the entire byte slice
            InitialMatch::Complete(None)
        } else {
            InitialMatch::Incomplete(length_of_leading_slice)
        }
    }
}

impl AsciiChars {
    #[inline]
    /// Create an empty AsciiChars
    pub const fn new() -> AsciiChars {
        Self::from_words(0, 0, 0)
    }

    #[inline]
    /// Create an AsciiChars with ascii bytes from `lo`, `hi`,
    /// with `count` bytes being used.
    pub const fn from_words(lo: u64, hi: u64, count: usize) -> AsciiChars {
        // this is memory safe even if the user may specify a count > 16 here
        // (because the pcmpestri instruction will saturate it at 16)
        //
        // However, specifying non-ascii bytes will result in non-ascii
        // indices being matched to, so we have to avoid this.
        AsciiChars {
            needle: lo & ASCII_WORD_MASK,
            needle_hi: hi & ASCII_WORD_MASK,
            count: count as u8,
        }
    }

    /// Add a new ASCII character to the set to search for.
    ///
    /// ### Panics
    ///
    /// - If you add more than 16 characters.
    /// - If you add a non-ASCII byte.
    pub fn push(&mut self, byte: u8) {
        assert!(byte < 128);
        assert!(self.count < MAXBYTES);
        self.needle_hi <<= 8;
        self.needle_hi |= self.needle >> (64 - 8);
        self.needle <<= 8;
        self.needle |= byte as u64;
        self.count += 1;
    }

    /// Builds a searcher with a fallback implementation for when the
    /// optimized version is not available. The fallback should search
    /// for the **exact** same set of characters.
    pub fn with_fallback<F>(self, fallback: F) -> AsciiCharsWithFallback<F>
        where F: Fn(u8) -> bool
    {
        AsciiCharsWithFallback { inner: self, fallback: fallback }
    }

    /// Find the index of the first character in the set.
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    #[inline]
    pub fn find(self, haystack: &str) -> Option<usize> {
        UnalignedByteSliceHandler { operation: self }.find(haystack.as_bytes())
    }
}

#[cfg(all(feature = "unstable", target_arch = "x86_64"))]
impl PackedCompareOperation for AsciiChars {
    unsafe fn initial(&self, ptr: *const u8, offset: usize, len: usize) -> u64 {
        let matching_bytes;

        asm!("movlhps $2, $1
              pcmpestrm $$0, ($3), $1"
             : // output operands
             "={xmm0}"(matching_bytes)
             : // input operands
             "x"(self.needle),
             "x"(self.needle_hi),
             "r"(ptr),
             "{rdx}"(offset + len), // saturates at 16
             "{rax}"(self.count as u64)
             : // clobbers
             "cc"
             : // options
        );

        matching_bytes
    }

    unsafe fn body(&self, ptr: *const u8, offset: usize, len: usize) -> u32 {
        let res;

        asm!("# Move low word of $2 to high word of $1
              movlhps $2, $1
              pcmpestri $$0, ($3, $4), $1"
             : // output operands
             "={ecx}"(res)
             : // input operands
             "x"(self.needle),
             "x"(self.needle_hi),
             "r"(ptr),
             "r"(offset)
             "{rdx}"(len),              // haystack length
             "{rax}"(self.count as u64) // needle length
             : // clobbers
             "cc"
             : // options
         );

        res
    }
}

impl fmt::Debug for AsciiChars {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "AsciiChars {{ needle: 0x{:016x}, count: {} }}", self.needle, self.count)
    }
}

/// Provides a hook for a user-supplied fallback implementation, used
/// when the optimized instructions are not available.
///
/// Although this implementation is a bit ungainly, Rust's closure
/// inlining is top-notch and provides the best speed.
#[derive(Debug,Copy,Clone)]
pub struct AsciiCharsWithFallback<F> {
    inner: AsciiChars,
    fallback: F,
}

impl<'a, F> Pattern<'a> for AsciiCharsWithFallback<F>
    where F: Fn(u8) -> bool
{
    type Searcher = AsciiCharsSearcher<'a, F>;

    fn into_searcher(self, haystack: &'a str) -> AsciiCharsSearcher<'a, F> {
        AsciiCharsSearcher { haystack: haystack, offset: 0, needle: self }
    }
}

/// An implementation of `Searcher` using `AsciiChars`
#[derive(Debug,Copy,Clone)]
pub struct AsciiCharsSearcher<'a, F> {
    haystack: &'a str,
    offset: usize,
    needle: AsciiCharsWithFallback<F>,
}

impl<'a, F> AsciiCharsSearcher<'a, F>
    where F: Fn(u8) -> bool
{
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn next_idx(&self, haystack: &str) -> Option<usize> {
        self.needle.inner.find(haystack)
    }

    #[cfg(not(all(feature = "unstable", target_arch = "x86_64")))]
    fn next_idx(&self, haystack: &str) -> Option<usize> {
        haystack.as_bytes().iter().cloned().position(&self.needle.fallback)
    }
}

unsafe impl<'a, F> Searcher<'a> for AsciiCharsSearcher<'a, F>
    where F: Fn(u8) -> bool
{
    fn haystack(&self) -> &'a str { self.haystack }

    #[inline]
    fn next(&mut self) -> SearchStep {
        // Assert that we are searching for only ascii
        let inner = &self.needle.inner;
        debug_assert!(inner.needle & !ASCII_WORD_MASK == 0);
        debug_assert!(inner.needle_hi & !ASCII_WORD_MASK == 0);

        if self.offset >= self.haystack.len() { return SearchStep::Done }

        let left_to_search = &self.haystack[self.offset..]; // TODO: unchecked_slice?
        let idx = self.next_idx(left_to_search);

        // If there's no match, then the rest of the string should be
        // returned.
        let idx = idx.unwrap_or(self.haystack.len());

        let (res, next_offset) = if idx == 0 {
            // A match occurs at the beginning of the string
            let next = self.offset + 1;
            (SearchStep::Match(self.offset, next), next)
        } else {
            // A match occurs somewhere further in the string
            let next = self.offset + idx;
            (SearchStep::Reject(self.offset, next), next)
        };

        self.offset = next_offset;
        res
    }
}

#[cfg(test)]
mod test {
    extern crate quickcheck;
    extern crate libc;
    extern crate rand;

    use super::AsciiChars;
    use self::rand::Rng;
    use self::quickcheck::{quickcheck,Arbitrary,Gen};
    use std::str::pattern::{Pattern,Searcher,SearchStep};
    use std::cmp;
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    use std::{slice,str,ptr};

    pub const SPACE: AsciiChars       = AsciiChars::from_words(0x0000000000000020, 0, 1);
    // < > &
    pub const XML_DELIM_3: AsciiChars = AsciiChars::from_words(0x00000000003c3e26, 0, 3);
    // < > & ' "
    pub const XML_DELIM_5: AsciiChars = AsciiChars::from_words(0x0000003c3e262722, 0, 5);

    #[derive(Debug,Copy,Clone)]
    struct AsciiChar(u8);

    impl Arbitrary for AsciiChar {
        fn arbitrary<G>(g: &mut G) -> AsciiChar
            where G: Gen
        {
            AsciiChar(g.gen_range(0, 128))
        }
    }

    #[test]
    fn works_as_find_does_for_single_characters() {
        fn prop(s: String, c: AsciiChar) -> bool {
            let mut searcher = AsciiChars::new();
            searcher.push(c.0);
            s.find(searcher.with_fallback(|b| b == c.0)) == s.find(c.0 as char)
        }
        quickcheck(prop as fn(String, AsciiChar) -> bool);
    }

    #[test]
    fn works_as_find_does_for_multiple_characters() {
        fn prop(s: String, (c1, c2, c3, c4): (AsciiChar, AsciiChar, AsciiChar, AsciiChar)) -> bool {
            let mut searcher = AsciiChars::new();
            searcher.push(c1.0);
            searcher.push(c2.0);
            searcher.push(c3.0);
            searcher.push(c4.0);
            s.find(searcher.with_fallback(|b| b == c1.0 || b == c2.0 || b == c3.0 || b == c4.0)) == s.find(&[c1.0 as char, c2.0 as char, c3.0 as char, c4.0 as char][..])
        }
        quickcheck(prop as fn(String, (AsciiChar, AsciiChar, AsciiChar, AsciiChar)) -> bool);
    }

    #[test]
    fn works_as_find_does_for_many_characters() {
        // test up to 16 ascii characters
        fn prop(s: String, v: Vec<AsciiChar>) -> bool {
            let n = cmp::min(super::MAXBYTES as usize, v.len());
            let mut searcher = AsciiChars::new();
            let mut chars = ['\0'; 16];
            for (index, &c) in v.iter().take(n).enumerate() {
                searcher.push(c.0);
                chars[index] = c.0 as char;
            }
            s.find(searcher.with_fallback(|b| chars[..n].iter().position(|&c| c == b as char).is_some())) == s.find(&chars[..n])
        }
        quickcheck(prop as fn(String, Vec<AsciiChar>) -> bool);
    }

    #[test]
    fn can_search_for_nul_bytes() {
        let mut s = AsciiChars::new();
        s.push(b'\0');
        assert_eq!(Some(1), "a\0".find(s.with_fallback(|b| b == b'\0')));
        assert_eq!(Some(0), "\0".find(s.with_fallback(|b| b == b'\0')));
        assert_eq!(None, "".find(s.with_fallback(|b| b == b'\0')));
    }

    #[test]
    fn can_search_in_nul_bytes() {
        let mut s = AsciiChars::new();
        s.push(b'a');
        assert_eq!(Some(1), "\0a".find(s.with_fallback(|b| b == b'a')));
        assert_eq!(None, "\0".find(s.with_fallback(|b| b == b'a')));
    }

    #[test]
    fn pattern_does_not_backtrack_after_first() {
        let mut searcher = SPACE.with_fallback(|b| b == b' ').into_searcher("hello w ");
        assert_eq!(SearchStep::Reject(0,5), searcher.next());
        assert_eq!(SearchStep::Match(5,6), searcher.next());
        assert_eq!(SearchStep::Reject(6,7), searcher.next());
        assert_eq!(SearchStep::Match(7,8), searcher.next());
        assert_eq!(SearchStep::Done, searcher.next());
    }

    #[test]
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn space_is_found() {
        // Since the algorithm operates on 16-byte chunks, it's
        // important to cover tests around that boundary. Since 16
        // isn't that big of a number, we might as well do all of
        // them.

        assert_eq!(Some(0),  SPACE.find(" "));
        assert_eq!(Some(1),  SPACE.find("0 "));
        assert_eq!(Some(2),  SPACE.find("01 "));
        assert_eq!(Some(3),  SPACE.find("012 "));
        assert_eq!(Some(4),  SPACE.find("0123 "));
        assert_eq!(Some(5),  SPACE.find("01234 "));
        assert_eq!(Some(6),  SPACE.find("012345 "));
        assert_eq!(Some(7),  SPACE.find("0123456 "));
        assert_eq!(Some(8),  SPACE.find("01234567 "));
        assert_eq!(Some(9),  SPACE.find("012345678 "));
        assert_eq!(Some(10), SPACE.find("0123456789 "));
        assert_eq!(Some(11), SPACE.find("0123456789A "));
        assert_eq!(Some(12), SPACE.find("0123456789AB "));
        assert_eq!(Some(13), SPACE.find("0123456789ABC "));
        assert_eq!(Some(14), SPACE.find("0123456789ABCD "));
        assert_eq!(Some(15), SPACE.find("0123456789ABCDE "));
        assert_eq!(Some(16), SPACE.find("0123456789ABCDEF "));
        assert_eq!(Some(17), SPACE.find("0123456789ABCDEFG "));
    }

    #[test]
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn space_not_found() {
        // Since the algorithm operates on 16-byte chunks, it's
        // important to cover tests around that boundary. Since 16
        // isn't that big of a number, we might as well do all of
        // them.

        assert_eq!(None, SPACE.find(""));
        assert_eq!(None, SPACE.find("0"));
        assert_eq!(None, SPACE.find("01"));
        assert_eq!(None, SPACE.find("012"));
        assert_eq!(None, SPACE.find("0123"));
        assert_eq!(None, SPACE.find("01234"));
        assert_eq!(None, SPACE.find("012345"));
        assert_eq!(None, SPACE.find("0123456"));
        assert_eq!(None, SPACE.find("01234567"));
        assert_eq!(None, SPACE.find("012345678"));
        assert_eq!(None, SPACE.find("0123456789"));
        assert_eq!(None, SPACE.find("0123456789A"));
        assert_eq!(None, SPACE.find("0123456789AB"));
        assert_eq!(None, SPACE.find("0123456789ABC"));
        assert_eq!(None, SPACE.find("0123456789ABCD"));
        assert_eq!(None, SPACE.find("0123456789ABCDE"));
        assert_eq!(None, SPACE.find("0123456789ABCDEF"));
        assert_eq!(None, SPACE.find("0123456789ABCDEFG"));
    }

    #[test]
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn works_on_nonaligned_beginnings() {
        // We have special code for strings that don't lie on 16-byte
        // boundaries. Since allocation seems to happen on these
        // boundaries by default, let's walk around a bit.

        let s = "0123456789ABCDEF ".to_string();

        assert_eq!(Some(16), SPACE.find(&s[ 0..]));
        assert_eq!(Some(15), SPACE.find(&s[ 1..]));
        assert_eq!(Some(14), SPACE.find(&s[ 2..]));
        assert_eq!(Some(13), SPACE.find(&s[ 3..]));
        assert_eq!(Some(12), SPACE.find(&s[ 4..]));
        assert_eq!(Some(11), SPACE.find(&s[ 5..]));
        assert_eq!(Some(10), SPACE.find(&s[ 6..]));
        assert_eq!(Some(9),  SPACE.find(&s[ 7..]));
        assert_eq!(Some(8),  SPACE.find(&s[ 8..]));
        assert_eq!(Some(7),  SPACE.find(&s[ 9..]));
        assert_eq!(Some(6),  SPACE.find(&s[10..]));
        assert_eq!(Some(5),  SPACE.find(&s[11..]));
        assert_eq!(Some(4),  SPACE.find(&s[12..]));
        assert_eq!(Some(3),  SPACE.find(&s[13..]));
        assert_eq!(Some(2),  SPACE.find(&s[14..]));
        assert_eq!(Some(1),  SPACE.find(&s[15..]));
        assert_eq!(Some(0),  SPACE.find(&s[16..]));
        assert_eq!(None,     SPACE.find(&s[17..]));
    }

    #[test]
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn xml_delim_3_is_found() {
        assert_eq!(Some(0), XML_DELIM_3.find("<"));
        assert_eq!(Some(0), XML_DELIM_3.find(">"));
        assert_eq!(Some(0), XML_DELIM_3.find("&"));
        assert_eq!(None,    XML_DELIM_3.find(""));
    }

    #[test]
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn xml_delim_5_is_found() {
        assert_eq!(Some(0), XML_DELIM_5.find("<"));
        assert_eq!(Some(0), XML_DELIM_5.find(">"));
        assert_eq!(Some(0), XML_DELIM_5.find("&"));
        assert_eq!(Some(0), XML_DELIM_5.find("'"));
        assert_eq!(Some(0), XML_DELIM_5.find("\""));
        assert_eq!(None,    XML_DELIM_5.find(""));
    }

    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    #[cfg(target_os = "macos")]
    const MAP_ANONYMOUS: libc::int32_t = libc::MAP_ANON;
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    #[cfg(not(target_os = "macos"))]
    const MAP_ANONYMOUS: libc::int32_t = libc::MAP_ANONYMOUS;

    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn alloc_guarded_string(value: &str, protect: bool) -> &'static str {
        // Allocate a string that ends directly before a
        // read-protected page.
        //
        // This function leaks two pages of memory per call, which is
        // acceptable for use in tests only.

        const PAGE_SIZE: usize = 4096;
        assert!(value.len() <= PAGE_SIZE);

        unsafe {
            // Map two rw-accessible pages of anonymous memory
            let first_page = libc::mmap(
                /* addr   = */ 0 as *mut libc::c_void,
                /* length = */ 2 * PAGE_SIZE as libc::size_t,
                /* prot   = */ libc::PROT_READ | libc::PROT_WRITE,
                /* flags  = */ libc::MAP_PRIVATE | MAP_ANONYMOUS,
                /* fd     = */ -1,
                /* offset = */ 0,
                );
            assert!(!first_page.is_null());

            let second_page = first_page.offset(PAGE_SIZE as isize);

            if protect {
                // Prohibit any access to the second page, so that any attempt
                // to read or write it would trigger a segfault
                let mprotect_retval = libc::mprotect(
                    /* addr   = */ second_page,
                    /* length = */ PAGE_SIZE as libc::size_t,
                    /* prot   = */ libc::PROT_NONE,
                    );
                assert_eq!(0, mprotect_retval);
            }

            // Copy bytes to the end of the first page
            let start = second_page.offset(-(value.len() as isize)) as *mut u8;
            ptr::copy_nonoverlapping(value.as_ptr(), start, value.len());
            str::from_utf8_unchecked(slice::from_raw_parts(start, value.len()))
        }
    }

    #[test]
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn works_at_page_boundary() {
        // PCMP*STR* instructions are known to read 16 bytes at a time.
        // This behaviour may cause accidental segfaults by reading
        // past the page boundary.
        //
        // For now, this test crashes the whole test suite.
        // This could be fixed by setting a custom signal handlers,
        // though Rust lacks such facilities at the moment.

        // Allocate a 16-byte string at page boundary.
        // To verify this test, set protect=false to prevent segfaults.
        let text = alloc_guarded_string("0123456789abcdef", true);

        // Will search for the last char
        let mut needle = AsciiChars::new();
        needle.push(b'f');

        // Check all suffixes of our 16-byte string
        for offset in 0..text.len() {
            let tail = &text[offset..];
            assert_eq!(Some(tail.len() - 1), needle.find(tail));
        }
    }
}

#[cfg(test)]
mod bench {
    extern crate test;

    use super::test::{SPACE,XML_DELIM_3,XML_DELIM_5};
    use std::iter;

    fn prefix_string() -> String {
        iter::repeat("a").take(5 * 1024 * 1024).collect()
    }

    fn bench_space<F>(b: &mut test::Bencher, f: F)
        where F: Fn(&str) -> Option<usize>
    {
        let mut haystack = prefix_string();
        haystack.push(' ');

        b.iter(|| test::black_box(f(&haystack)));
        b.bytes = haystack.len() as u64;
    }

    #[bench]
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn space_asciichars(b: &mut test::Bencher) {
        bench_space(b, |hs| SPACE.find(hs))
    }

    #[bench]
    fn space_asciichars_as_pattern(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.find(SPACE.with_fallback(|b| b == b' ')))
    }

    #[bench]
    fn space_find_string(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.find(" "))
    }

    #[bench]
    fn space_find_char(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.find(' '))
    }

    #[bench]
    fn space_find_char_set(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.find(&[' '][..]))
    }

    #[bench]
    fn space_find_byte(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.as_bytes().iter().position(|&v| v == b' '))
    }

    #[bench]
    fn space_find_closure(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.find(|c| c == ' '))
    }

    fn bench_xml_delim_3<F>(b: &mut test::Bencher, f: F)
        where F: Fn(&str) -> Option<usize>
    {
        let mut haystack = prefix_string();
        haystack.push('&');

        b.iter(|| test::black_box(f(&haystack)));
        b.bytes = haystack.len() as u64;
    }

    #[bench]
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn xml_delim_3_asciichars(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| XML_DELIM_3.find(hs))
    }

    #[bench]
    fn xml_delim_3_asciichars_as_pattern(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| hs.find(XML_DELIM_3.with_fallback(|c| {
            c == b'<' || c == b'>' || c == b'&'
        })))
    }

    #[bench]
    fn xml_delim_3_find_byte_closure(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| hs.as_bytes().iter().position(|&c| {
            c == b'<' || c == b'>' || c == b'&'
        }))
    }

    #[bench]
    fn xml_delim_3_find_char_set(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| hs.find(&['<', '>', '&'][..]))
    }

    #[bench]
    fn xml_delim_3_find_char_closure(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| hs.find(|c| {
            c == '<' || c == '>' || c == '&'
        }))
    }

    fn bench_xml_delim_5<F>(b: &mut test::Bencher, f: F)
        where F: Fn(&str) -> Option<usize>
    {
        let mut haystack = prefix_string();
        haystack.push('"');

        b.iter(|| test::black_box(f(&haystack)));
        b.bytes = haystack.len() as u64;
    }

    #[bench]
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn xml_delim_5_asciichars(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| XML_DELIM_5.find(hs))
    }

    #[bench]
    fn xml_delim_5_asciichars_as_pattern(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| hs.find(XML_DELIM_5.with_fallback(|c| {
            c == b'<' || c == b'>' || c == b'&' || c == b'\'' || c == b'"'
        })))
    }

    #[bench]
    fn xml_delim_5_find_byte_closure(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| hs.as_bytes().iter().position(|&c| {
            c == b'<' || c == b'>' || c == b'&' || c == b'\'' || c == b'"'
        }))
    }

    #[bench]
    fn xml_delim_5_find_char_set(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| hs.find(&['<', '>', '&', '\'', '"'][..]))
    }

    #[bench]
    fn xml_delim_5_find_char_closure(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| hs.find(|c| {
            c == '<' || c == '>' || c == '&' || c == '\'' || c == '"'
        }))
    }
}
