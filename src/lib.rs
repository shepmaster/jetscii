#![feature(asm)]
#![feature(const_fn)]
#![feature(pattern)]
#![cfg_attr(test, feature(test))]

//! A tiny library to efficiently search strings for substrings or
//! sets of ASCII characters.
//!
//! ## Examples
//!
//! ### Searching for a set of ASCII characters
//! ```
//! #[macro_use]
//! extern crate jetscii;
//!
//! fn main() {
//!     let part_number = "86-J52:rev1";
//!     let parts: Vec<_> = part_number.split(ascii_chars!('-', ':')).collect();
//!     assert_eq!(&parts, &["86", "J52", "rev1"]);
//! }
//! ```
//!
//! ### Searching for a substring
//! ```
//! use jetscii::Substring;
//!
//! let colors: Vec<_> = "red, blue, green".split(Substring::new(", ")).collect();
//! assert_eq!(&colors, &["red", "blue", "green"]);
//! ```

#[cfg(test)]
#[macro_use]
extern crate lazy_static;

use std::cmp::min;
use std::fmt;
use std::str::pattern::{Pattern, SearchStep, Searcher};

mod v2;

pub use v2::Bytes as BytesV2;

trait PackedCompareOperation {
    // Returns a mask
    unsafe fn initial(&self, ptr: *const u8, offset: usize, len: usize) -> u64;
    // Returns an index
    unsafe fn body(&self, ptr: *const u8, offset: usize, len: usize) -> u32;
}

#[cfg(all(feature = "unstable", target_arch = "x86_64"))]
enum InitialMatch {
    Complete(Option<usize>),
    Incomplete(usize),
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

        if len == 0 {
            return None;
        }

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
    fn initial_unaligned_byte_slice(&self,
                                    ptr: *const u8,
                                    offset: usize,
                                    len: usize)
                                    -> InitialMatch {
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

#[doc(hidden)]
#[macro_export]
macro_rules! ascii_chars_inner {
    ($cnt:expr,
     $c0:expr, $c1:expr, $c2:expr, $c3:expr,
     $c4:expr, $c5:expr, $c6:expr, $c7:expr) => ({
         let lo =
             ($c0 as u64) <<  0 | ($c1 as u64) <<  8 | ($c2 as u64) << 16 | ($c3 as u64) << 24 |
             ($c4 as u64) << 32 | ($c5 as u64) << 40 | ($c6 as u64) << 48 | ($c7 as u64) << 56;
         let ac = $crate::AsciiChars::from_words(lo, 0, $cnt);
         ac.with_fallback(move |c| {
             c == $c0 as u8 || c == $c1 as u8 || c == $c2 as u8 || c == $c3 as u8 ||
             c == $c4 as u8 || c == $c5 as u8 || c == $c6 as u8 || c == $c7 as u8
         })
     });
    ($cnt:expr,
     $c0:expr,   $c1:expr,  $c2:expr,  $c3:expr,
     $c4:expr,   $c5:expr,  $c6:expr,  $c7:expr,
     $c8:expr,   $c9:expr, $c10:expr, $c11:expr,
     $c12:expr, $c13:expr, $c14:expr, $c15:expr) => ({
         let lo =
             ($c0 as u64) <<  0 | ($c1 as u64) <<  8 | ($c2 as u64) << 16 | ($c3 as u64) << 24 |
             ($c4 as u64) << 32 | ($c5 as u64) << 40 | ($c6 as u64) << 48 | ($c7 as u64) << 56;
         let hi =
             ( $c8 as u64) <<  0 | ( $c9 as u64) <<  8 | ($c10 as u64) << 16 | ($c11 as u64) << 24 |
             ($c12 as u64) << 32 | ($c13 as u64) << 40 | ($c14 as u64) << 48 | ($c15 as u64) << 56;
         let ac = $crate::AsciiChars::from_words(lo, hi, $cnt);
         ac.with_fallback(move |c| {
             c ==  $c0 as u8 || c ==  $c1 as u8 || c ==  $c2 as u8 || c ==  $c3 as u8 ||
             c ==  $c4 as u8 || c ==  $c5 as u8 || c ==  $c6 as u8 || c ==  $c7 as u8 ||
             c ==  $c8 as u8 || c ==  $c9 as u8 || c == $c10 as u8 || c == $c11 as u8 ||
             c == $c12 as u8 || c == $c13 as u8 || c == $c14 as u8 || c == $c15 as u8
         })
     });
}

/// A convenience constructor for an AsciiChars that automatically
/// implements a fallback. Provide 1 to 16 characters.
// Pad out undefined variables with duplicate values, hope that the
// optimizer deduplicates them.
#[macro_export]
macro_rules! ascii_chars {
    ($c0:expr) =>
        (ascii_chars_inner!(1, $c0, $c0, $c0, $c0, $c0, $c0, $c0, $c0));
    ($c0:expr, $c1:expr) =>
        (ascii_chars_inner!(2, $c0, $c1, $c0, $c0, $c0, $c0, $c0, $c0));
    ($c0:expr, $c1:expr, $c2:expr) =>
        (ascii_chars_inner!(3, $c0, $c1, $c2, $c0, $c0, $c0, $c0, $c0));
    ($c0:expr, $c1:expr, $c2:expr, $c3:expr) =>
        (ascii_chars_inner!(4, $c0, $c1, $c2, $c3, $c0, $c0, $c0, $c0));
    ($c0:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr) =>
        (ascii_chars_inner!(5, $c0, $c1, $c2, $c3, $c4, $c0, $c0, $c0));
    ($c0:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr, $c5:expr) =>
        (ascii_chars_inner!(6, $c0, $c1, $c2, $c3, $c4, $c5, $c0, $c0));
    ($c0:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr, $c5:expr, $c6:expr) =>
        (ascii_chars_inner!(7, $c0, $c1, $c2, $c3, $c4, $c5, $c6, $c0));
    ($c0:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr, $c5:expr, $c6:expr, $c7:expr) =>
        (ascii_chars_inner!(8, $c0, $c1, $c2, $c3, $c4, $c5, $c6, $c7));

    ($c0:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr, $c5:expr, $c6:expr, $c7:expr,
     $c8:expr) =>
        (ascii_chars_inner!(9, $c0, $c1, $c2, $c3, $c4, $c5, $c6, $c7,
                       $c8, $c0, $c0, $c0, $c0, $c0, $c0, $c0));

    ($c0:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr, $c5:expr, $c6:expr, $c7:expr,
     $c8:expr, $c9:expr) =>
        (ascii_chars_inner!(10, $c0, $c1, $c2, $c3, $c4, $c5, $c6, $c7,
                        $c8, $c9, $c0, $c0, $c0, $c0, $c0, $c0));

    ($c0:expr, $c1:expr,  $c2:expr, $c3:expr, $c4:expr, $c5:expr, $c6:expr, $c7:expr,
     $c8:expr, $c9:expr, $c10:expr) =>
        (ascii_chars_inner!(11, $c0, $c1,  $c2, $c3, $c4, $c5, $c6,$c7,
                        $c8, $c9, $c10, $c0, $c0, $c0, $c0, $c0));

    ($c0:expr, $c1:expr,  $c2:expr,  $c3:expr, $c4:expr, $c5:expr, $c6:expr, $c7:expr,
     $c8:expr, $c9:expr, $c10:expr, $c11:expr) =>
        (ascii_chars_inner!(12, $c0, $c1,  $c2,  $c3, $c4, $c5, $c6, $c7,
                        $c8, $c9, $c10, $c11, $c0, $c0, $c0, $c0));

    ($c0:expr, $c1:expr,  $c2:expr,  $c3:expr,  $c4:expr, $c5:expr, $c6:expr, $c7:expr,
     $c8:expr, $c9:expr, $c10:expr, $c11:expr, $c12:expr) =>
        (ascii_chars_inner!(13, $c0, $c1,  $c2,  $c3,  $c4, $c5, $c6, $c7,
                        $c8, $c9, $c10, $c11, $c12, $c0, $c0, $c0));

    ($c0:expr, $c1:expr,  $c2:expr,  $c3:expr,  $c4:expr,  $c5:expr, $c6:expr, $c7:expr,
     $c8:expr, $c9:expr, $c10:expr, $c11:expr, $c12:expr, $c13:expr) =>
        (ascii_chars_inner!(14, $c0, $c1,  $c2,  $c3,  $c4,  $c5, $c6, $c7,
                        $c8, $c9, $c10, $c11, $c12, $c13, $c0, $c0));

    ($c0:expr, $c1:expr,  $c2:expr,  $c3:expr,  $c4:expr,  $c5:expr,  $c6:expr, $c7:expr,
     $c8:expr, $c9:expr, $c10:expr, $c11:expr, $c12:expr, $c13:expr, $c14:expr) =>
        (ascii_chars_inner!(15, $c0, $c1,  $c2,  $c3,  $c4,  $c5,  $c6, $c7,
                        $c8, $c9, $c10, $c11, $c12, $c13, $c14, $c0));

    ($c0:expr, $c1:expr,  $c2:expr,  $c3:expr,  $c4:expr,  $c5:expr,  $c6:expr,  $c7:expr,
     $c8:expr, $c9:expr, $c10:expr, $c11:expr, $c12:expr, $c13:expr, $c14:expr, $c15:expr) =>
        (ascii_chars_inner!(16, $c0, $c1,  $c2,  $c3,  $c4,  $c5,  $c6,  $c7,
                        $c8, $c9, $c10, $c11, $c12, $c13, $c14, $c15));
}

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
        AsciiCharsWithFallback {
            inner: self,
            fallback: fallback,
        }
    }

    /// Find the index of the first character in the set.
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    #[inline]
    pub fn find(self, haystack: &str) -> Option<usize> {
        UnalignedByteSliceHandler { operation: self }.find(haystack.as_bytes())
    }
}

impl fmt::Debug for AsciiChars {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "AsciiChars {{ lo: 0x{:016x}, hi: 0x{:016x}, count: {} }}",
               self.needle,
               self.needle_hi,
               self.count)
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

unsafe impl<F> DirectSearch for AsciiCharsWithFallback<F>
    where F: Fn(u8) -> bool
{
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn find(&self, haystack: &str) -> Option<usize> {
        self.inner.find(haystack)
    }

    #[cfg(not(all(feature = "unstable", target_arch = "x86_64")))]
    fn find(&self, haystack: &str) -> Option<usize> {
        haystack.as_bytes().iter().cloned().position(&self.fallback)
    }

    fn len(&self) -> usize {
        1
    }
}

impl<'a, F> Pattern<'a> for AsciiCharsWithFallback<F>
    where F: Fn(u8) -> bool
{
    type Searcher = DirectSearcher<'a, AsciiCharsWithFallback<F>>;

    fn into_searcher(self, haystack: &'a str) -> DirectSearcher<'a, AsciiCharsWithFallback<F>> {
        // Assert that we are searching for only ascii
        debug_assert!(self.inner.needle & !ASCII_WORD_MASK == 0);
        debug_assert!(self.inner.needle_hi & !ASCII_WORD_MASK == 0);

        DirectSearcher {
            haystack: haystack,
            offset: 0,
            direct_search: self,
        }
    }
}

/// Search a string for a substring.
#[derive(Debug,Copy,Clone)]
pub struct Substring<'a> {
    raw: &'a str,
    needle_lo: u64,
    needle_hi: u64,
    needle_len: u8,
}

impl<'a> Substring<'a> {
    pub fn new(needle: &'a str) -> Substring<'a> {
        fn pack_needle_bytes(bytes: &[u8]) -> u64 {
            let mut needle = 0;
            for &b in bytes.iter().rev() {
                needle <<= 8;
                needle |= b as u64;
            }
            needle
        }

        let mut bytes = needle.as_bytes().chunks(8);
        let needle_lo = bytes.next().map(pack_needle_bytes).unwrap_or(0);
        let needle_hi = bytes.next().map(pack_needle_bytes).unwrap_or(0);

        Substring {
            raw: needle,
            needle_lo: needle_lo,
            needle_hi: needle_hi,
            needle_len: min(needle.len(), 16) as u8,
        }
    }
}

impl<'a> PackedCompareOperation for Substring<'a> {
    unsafe fn initial(&self, haystack: *const u8, offset: usize, len: usize) -> u64 {
        let matching_bytes;

        asm!("movlhps $2, $1
              pcmpestrm $$0b00001100, ($3), $1"
             : // output operands
             "={xmm0}"(matching_bytes)
             : // input operands
             "x"(self.needle_lo),
             "x"(self.needle_hi),
             "r"(haystack),
             "{rax}"(self.needle_len as u64),
             "{rdx}"(offset + len)
             : // clobbers
             "cc"
             : // options
        );

        matching_bytes
    }

    unsafe fn body(&self, haystack: *const u8, offset: usize, len: usize) -> u32 {
        let matching_idx;

        asm!("movlhps $2, $1
              pcmpestri $$0b00001100, ($3, $4), $1"
             : // output operands
             "={ecx}"(matching_idx)
             : // input operands
             "x"(self.needle_lo),
             "x"(self.needle_hi),
             "r"(haystack),
             "r"(offset),
             "{rax}"(self.needle_len as u64),
             "{rdx}"(len)
             : // clobbers
             "cc"
             : // options
        );

        matching_idx
    }
}

unsafe impl<'a> DirectSearch for Substring<'a> {
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    fn find(&self, haystack: &str) -> Option<usize> {
        // It's ok to treat the haystack as a bag of bytes because the
        // needle is guaranteed to only match complete UTF-8
        // characters. Whenever a match is found, we double-check the
        // match position with the complete needle.

        let needle = self.raw.as_bytes();
        let haystack = haystack.as_bytes();

        if needle.len() == 0 && haystack.len() == 0 {
            return Some(0);
        }

        let searcher = UnalignedByteSliceHandler { operation: *self };
        let mut offset = 0;

        while let Some(pos) = searcher.find(&haystack[offset..]) {
            // Found a match, but is it really?
            if haystack[pos + offset..].starts_with(needle) {
                return Some(offset + pos);
            }

            // Skip past this false positive
            offset += pos + 1;
        }
        None
    }

    #[cfg(not(all(feature = "unstable", target_arch = "x86_64")))]
    fn find(&self, haystack: &str) -> Option<usize> {
        haystack.find(self.raw)
    }

    fn len(&self) -> usize {
        self.raw.len()
    }
}

impl<'a> Pattern<'a> for Substring<'a> {
    type Searcher = DirectSearcher<'a, Substring<'a>>;

    fn into_searcher(self, haystack: &'a str) -> DirectSearcher<'a, Substring<'a>> {
        DirectSearcher {
            haystack: haystack,
            offset: 0,
            direct_search: self,
        }
    }
}

/// Types that return the index of the next match.
// Do we really want to expose the trait like this?
pub unsafe trait DirectSearch {
    fn find(&self, haystack: &str) -> Option<usize>;
    fn len(&self) -> usize;
}

/// A searcher implementation for DirectSearch types.
#[derive(Debug,Copy,Clone)]
pub struct DirectSearcher<'a, D> {
    haystack: &'a str,
    offset: usize,
    direct_search: D,
}

unsafe impl<'a, D> Searcher<'a> for DirectSearcher<'a, D>
    where D: DirectSearch
{
    fn haystack(&self) -> &'a str {
        self.haystack
    }

    #[inline]
    fn next(&mut self) -> SearchStep {
        if self.offset >= self.haystack.len() {
            return SearchStep::Done;
        }

        let left_to_search = &self.haystack[self.offset..]; // TODO: unchecked_slice?
        let idx = self.direct_search.find(left_to_search);

        // If there's no match, then the rest of the string should be
        // returned.
        let idx = idx.unwrap_or(self.haystack.len());

        let (res, next_offset) = if idx == 0 {
            // A match occurs at the beginning of the string
            let next = self.offset + self.direct_search.len();
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

    use super::{AsciiChars, Substring, DirectSearch};
    use self::quickcheck::{quickcheck, Arbitrary, Gen};
    use std::str::pattern::{Pattern, Searcher, SearchStep};
    use std::cmp;
    #[cfg(all(feature = "unstable", target_arch = "x86_64"))]
    use std::{slice, str, ptr};

    pub const SPACE: AsciiChars = AsciiChars::from_words(0x0000000000000020, 0, 1);
    // < > &
    pub const XML_DELIM_3: AsciiChars = AsciiChars::from_words(0x00000000003c3e26, 0, 3);
    // < > & ' "
    pub const XML_DELIM_5: AsciiChars = AsciiChars::from_words(0x0000003c3e262722, 0, 5);

    #[derive(Debug,Copy,Clone)]
    struct AsciiChar(char);

    impl Arbitrary for AsciiChar {
        fn arbitrary<G>(g: &mut G) -> AsciiChar
            where G: Gen
        {
            AsciiChar(g.gen_range::<u8>(0, 128) as char)
        }
    }

    #[test]
    fn works_as_find_does_for_single_characters() {
        fn prop(s: String, c: AsciiChar) -> bool {
            s.find(ascii_chars!(c.0)) == s.find(c.0)
        }
        quickcheck(prop as fn(String, AsciiChar) -> bool);
    }

    #[test]
    fn works_as_find_does_for_multiple_characters() {
        fn prop(s: String, (c1, c2, c3, c4): (AsciiChar, AsciiChar, AsciiChar, AsciiChar)) -> bool {
            s.find(ascii_chars!(c1.0, c2.0, c3.0, c4.0)) == s.find(&[c1.0, c2.0, c3.0, c4.0][..])
        }
        quickcheck(prop as fn(String, (AsciiChar, AsciiChar, AsciiChar, AsciiChar)) -> bool);
    }

    #[test]
    fn works_as_find_does_for_up_to_16_characters() {
        fn prop(s: String, v: Vec<AsciiChar>) -> bool {
            let n = cmp::min(super::MAXBYTES as usize, v.len());
            let mut searcher = AsciiChars::new();
            let mut chars = ['\0'; 16];
            for (index, &c) in v.iter().take(n).enumerate() {
                searcher.push(c.0 as u8);
                chars[index] = c.0;
            }

            let us = s.find(searcher.with_fallback(|b| {
                chars[..n].iter().position(|&c| c == b as char).is_some()
            }));

            let find = s.find(&chars[..n]);

            us == find
        }
        quickcheck(prop as fn(String, Vec<AsciiChar>) -> bool);
    }

    #[test]
    fn can_search_for_nul_bytes() {
        assert_eq!(Some(1), "a\0".find(ascii_chars!('\0')));
        assert_eq!(Some(0), "\0".find(ascii_chars!('\0')));
        assert_eq!(None, "".find(ascii_chars!('\0')));
    }

    #[test]
    fn can_search_in_nul_bytes() {
        assert_eq!(Some(1), "\0a".find(ascii_chars!('a')));
        assert_eq!(None, "\0".find(ascii_chars!('a')));
    }

    #[test]
    fn pattern_does_not_backtrack_after_first() {
        let mut searcher = ascii_chars!(' ').into_searcher("hello w ");
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
            let addr = 0 as *mut libc::c_void;
            let length = 2 * PAGE_SIZE as libc::size_t;
            let prot = libc::PROT_READ | libc::PROT_WRITE;
            let flags = libc::MAP_PRIVATE | MAP_ANONYMOUS;
            let fd = -1;
            let offset = 0;

            let first_page = libc::mmap(addr, length, prot, flags, fd, offset);
            assert!(!first_page.is_null());

            let second_page = first_page.offset(PAGE_SIZE as isize);

            if protect {
                // Prohibit any access to the second page, so that any attempt
                // to read or write it would trigger a segfault
                let addr = second_page;
                let length = PAGE_SIZE as libc::size_t;
                let prot = libc::PROT_NONE;

                let mprotect_retval = libc::mprotect(addr, length, prot);
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
        let needle = ascii_chars!('f');

        // Check all suffixes of our 16-byte string
        for offset in 0..text.len() {
            let tail = &text[offset..];
            assert_eq!(Some(tail.len() - 1), needle.find(tail));
        }
    }

    #[test]
    fn works_as_find_does_for_substrings() {
        fn prop(needle: String, haystack: String) -> bool {
            let s = Substring::new(&needle);
            s.find(&haystack) == haystack.find(&needle)
        }
        quickcheck(prop as fn(String, String) -> bool);
    }

    #[test]
    fn substring_is_found() {
        let substr = Substring::new("zz");
        assert_eq!(Some(0),  substr.find("zz"));
        assert_eq!(Some(1),  substr.find("0zz"));
        assert_eq!(Some(2),  substr.find("01zz"));
        assert_eq!(Some(3),  substr.find("012zz"));
        assert_eq!(Some(4),  substr.find("0123zz"));
        assert_eq!(Some(5),  substr.find("01234zz"));
        assert_eq!(Some(6),  substr.find("012345zz"));
        assert_eq!(Some(7),  substr.find("0123456zz"));
        assert_eq!(Some(8),  substr.find("01234567zz"));
        assert_eq!(Some(9),  substr.find("012345678zz"));
        assert_eq!(Some(10), substr.find("0123456789zz"));
        assert_eq!(Some(11), substr.find("0123456789Azz"));
        assert_eq!(Some(12), substr.find("0123456789ABzz"));
        assert_eq!(Some(13), substr.find("0123456789ABCzz"));
        assert_eq!(Some(14), substr.find("0123456789ABCDzz"));
        assert_eq!(Some(15), substr.find("0123456789ABCDEzz"));
        assert_eq!(Some(16), substr.find("0123456789ABCDEFzz"));
        assert_eq!(Some(17), substr.find("0123456789ABCDEFGzz"));
    }

    #[test]
    fn substring_is_not_found() {
        let substr = Substring::new("zz");
        assert_eq!(None, substr.find(""));
        assert_eq!(None, substr.find("0"));
        assert_eq!(None, substr.find("01"));
        assert_eq!(None, substr.find("012"));
        assert_eq!(None, substr.find("0123"));
        assert_eq!(None, substr.find("01234"));
        assert_eq!(None, substr.find("012345"));
        assert_eq!(None, substr.find("0123456"));
        assert_eq!(None, substr.find("01234567"));
        assert_eq!(None, substr.find("012345678"));
        assert_eq!(None, substr.find("0123456789"));
        assert_eq!(None, substr.find("0123456789A"));
        assert_eq!(None, substr.find("0123456789AB"));
        assert_eq!(None, substr.find("0123456789ABC"));
        assert_eq!(None, substr.find("0123456789ABCD"));
        assert_eq!(None, substr.find("0123456789ABCDE"));
        assert_eq!(None, substr.find("0123456789ABCDEF"));
        assert_eq!(None, substr.find("0123456789ABCDEFG"));
    }

    #[test]
    fn substring_has_false_positive() {
        // The PCMPESTRI instruction will mark the "a" before "ab" as
        // a match because it cannot look beyond the 16 byte window
        // of the haystack. We need to double-check any match to
        // ensure it completely matches.

        let substr = Substring::new("ab");
        assert_eq!(Some(16), substr.find("aaaaaaaaaaaaaaaaab"));
        //   this "a" is a false positive ~~~~~~~~~~~~~~~^
    }

    #[test]
    fn substring_needle_is_longer_than_16_bytes() {
        let needle = "0123456789abcdefg";
        let haystack = "0123456789abcdefgh";
        assert_eq!(Some(0), Substring::new(needle).find(haystack));
    }

    #[test]
    fn substring_as_pattern() {
        let needle = "and";
        let haystack = "moats and boats and waterfalls";
        let parts: Vec<_> = haystack.split(Substring::new(needle)).collect();
        assert_eq!(&parts, &["moats ", " boats ", " waterfalls"]);
    }
}

#[cfg(test)]
mod bench {
    extern crate test;

    use super::test::{SPACE, XML_DELIM_3, XML_DELIM_5};
    use super::{Substring, DirectSearch};
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
    fn space_asciichars_macro(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.find(ascii_chars!(' ')))
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
        bench_xml_delim_3(b, |hs| {
            hs.find(XML_DELIM_3.with_fallback(|c| c == b'<' || c == b'>' || c == b'&'))
        })
    }

    #[bench]
    fn xml_delim_3_asciichars_macro(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| hs.find(ascii_chars!('<', '>', '&')))
    }

    #[bench]
    fn xml_delim_3_find_byte_closure(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| {
            hs.as_bytes().iter().position(|&c| c == b'<' || c == b'>' || c == b'&')
        })
    }

    #[bench]
    fn xml_delim_3_find_char_set(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| hs.find(&['<', '>', '&'][..]))
    }

    #[bench]
    fn xml_delim_3_find_char_closure(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| hs.find(|c| c == '<' || c == '>' || c == '&'))
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
        bench_xml_delim_5(b, |hs| {
            hs.find(XML_DELIM_5.with_fallback(|c| {
                c == b'<' || c == b'>' || c == b'&' || c == b'\'' || c == b'"'
            }))
        })
    }

    #[bench]
    fn xml_delim_5_asciichars_macro(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| hs.find(ascii_chars!('<', '>', '&', '\'', '"')))
    }

    #[bench]
    fn xml_delim_5_find_byte_closure(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| {
            hs.as_bytes()
                .iter()
                .position(|&c| c == b'<' || c == b'>' || c == b'&' || c == b'\'' || c == b'"')
        })
    }

    #[bench]
    fn xml_delim_5_find_char_set(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| hs.find(&['<', '>', '&', '\'', '"'][..]))
    }

    #[bench]
    fn xml_delim_5_find_char_closure(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| {
            hs.find(|c| c == '<' || c == '>' || c == '&' || c == '\'' || c == '"')
        })
    }

    fn bench_substring<F>(b: &mut test::Bencher, f: F)
        where F: Fn(&str) -> Option<usize>
    {
        let mut haystack = prefix_string();
        haystack.push_str("xyzzy");

        b.iter(|| test::black_box(f(&haystack)));
        b.bytes = haystack.len() as u64;
    }

    #[bench]
    fn substring_with_cached_searcher(b: &mut test::Bencher) {
        let z = Substring::new("xyzzy");
        bench_substring(b, |hs| z.find(hs))
    }

    #[bench]
    fn substring_with_created_searcher(b: &mut test::Bencher) {
        bench_substring(b, |hs| Substring::new("xyzzy").find(hs))
    }

    #[bench]
    fn substring_as_pattern(b: &mut test::Bencher) {
        bench_substring(b, |hs| hs.find(Substring::new("xyzzy")))
    }

    #[bench]
    fn substring_find(b: &mut test::Bencher) {
        bench_substring(b, |hs| hs.find("xyzzy"))
    }
}
