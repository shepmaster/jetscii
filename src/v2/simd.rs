// # Warning
//
// Everything in this module assumes that the SSE 4.2 feature is available.

use std::{
    arch::x86_64::{_mm_cmpestrc, _mm_cmpestri, _mm_cmpestrm, __m128i, _mm_loadu_si128},
    cmp::min,
    slice,
};

include!(concat!(env!("OUT_DIR"), "/src/v2/simd_macros.rs"));

const BYTES_PER_OPERATION: usize = 16;

union TransmuteToSimd {
    simd: __m128i,
    bytes: [u8; 16],
    u64s: [u64; 2],
}

pub struct Fast {
    needle: __m128i,
    needle_len: i32,
}

impl Fast {
    const CONTROL_BYTE: i32 = 0;

    pub /* const */ fn new(bytes: [u8; 16], needle_len: i32) -> Self {
        Fast {
            needle: unsafe { TransmuteToSimd { bytes }.simd },
            needle_len,
        }
    }

    /// The PCMPxSTRx instructions always read 16 bytes worth of
    /// data. Although the instructions handle unaligned memory access
    /// just fine, they might attempt to read off the end of a page
    /// and into a protected area.
    ///
    /// To handle this case, we read in 16-byte aligned chunks with
    /// respect to the *end* of the byte slice. This makes the
    /// complicated part in searching the leftover bytes at the
    /// beginning of the byte slice.
    #[inline]
    #[target_feature(enable = "sse4.2")]
    pub unsafe fn find(&self, mut haystack: &[u8]) -> Option<usize> {
        // FIXME: EXPLAIN SAFETY

        if haystack.is_empty() {
            return None;
        }

        let mut offset = 0;

        if let Some(misaligned) = Misalignment::new(haystack) {
            if let Some(location) = self.cmpestrm(misaligned.leading, misaligned.leading_junk) {
                // Since the masking operation covers an entire
                // 16-byte chunk, we have to see if the match occurred
                // somewhere *after* our data
                if location < haystack.len() {
                    return Some(location);
                }
            }

            haystack = &haystack[misaligned.bytes_until_alignment..];
            offset += misaligned.bytes_until_alignment;
        }

        // TODO: try removing the 16-byte loop and check the disasm
        let n_complete_chunks = haystack.len() / BYTES_PER_OPERATION;

        // Getting the pointer once before the loop avoids the
        // overhead of manipulating the length of the slice inside the
        // loop.
        let mut haystack_ptr = haystack.as_ptr();
        let mut chunk_offset = 0;
        for _ in 0..n_complete_chunks {
            if let Some(location) = self.cmpestri(haystack_ptr, BYTES_PER_OPERATION as i32) {
                return Some(offset + chunk_offset + location);
            }

            haystack_ptr = haystack_ptr.offset(BYTES_PER_OPERATION as isize);
            chunk_offset += BYTES_PER_OPERATION;
        }
        haystack = &haystack[chunk_offset..];
        offset += chunk_offset;

        // By this point, the haystack's length must be less than 16
        // bytes. It is thus reasonable to truncate it into an i32.
        debug_assert!(haystack.len() < ::std::i32::MAX as usize);
        self.cmpestri(haystack.as_ptr(), haystack.len() as i32)
            .map(|loc| offset + loc)
    }

    #[inline]
    #[target_feature(enable = "sse4.2")]
    unsafe fn cmpestrm(&self, haystack: &[u8], leading_junk: usize) -> Option<usize> {
        // TODO: document why this is ok
        let haystack = _mm_loadu_si128(haystack.as_ptr() as *const __m128i);

        // TODO: Check that these are coalesced
        let found = _mm_cmpestrc(
            self.needle,
            self.needle_len,
            haystack,
            BYTES_PER_OPERATION as i32,
            Self::CONTROL_BYTE,
        );
        let mask = _mm_cmpestrm(
            self.needle,
            self.needle_len,
            haystack,
            BYTES_PER_OPERATION as i32,
            Self::CONTROL_BYTE,
        );

        if found != 0 {
            let mut mask = TransmuteToSimd { simd: mask }.u64s[0];

            // Byte: 7 6 5 4 3 2 1 0
            // Str : &[0, 1, 2, 3, ...]
            //
            // Bit-0 corresponds to Str-0; shifting to the right
            // removes the parts of the string that don't belong to
            // us.
            mask >>= leading_junk;
            // The first 1, starting from Bit-0 and going to Bit-7,
            // denotes the position of the first match.
            Some(mask.trailing_zeros() as usize)
        } else {
            None
        }
    }

    #[inline]
    #[target_feature(enable = "sse4.2")]
    unsafe fn cmpestri(&self, haystack: *const u8, haystack_len: i32) -> Option<usize> {
        // TODO: document why this is ok
        let haystack = _mm_loadu_si128(haystack as *const __m128i);

        // TODO: Check that these are coalesced
        let found = _mm_cmpestrc(
            self.needle,
            self.needle_len,
            haystack,
            haystack_len,
            Self::CONTROL_BYTE,
        );
        let location = _mm_cmpestri(
            self.needle,
            self.needle_len,
            haystack,
            haystack_len,
            Self::CONTROL_BYTE,
        );

        if found != 0 {
            Some(location as usize)
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct Misalignment<'a> {
    leading: &'a [u8],
    leading_junk: usize,
    bytes_until_alignment: usize,
}

impl<'a> Misalignment<'a> {
    /// # Cases
    ///
    /// 0123456789ABCDEF
    /// |--|                < 1.
    ///       |--|          < 2.
    ///             |--|    < 3.
    ///             |----|  < 4.
    ///
    /// 1. The input slice is aligned.
    /// 2. The input slice is unaligned and is completely within the 16-byte chunk.
    /// 3. The input slice is unaligned and touches the boundary of the 16-byte chunk.
    /// 4. The input slice is unaligned and crosses the boundary of the 16-byte chunk.
    #[inline]
    fn new(haystack: &[u8]) -> Option<Self> {
        let aligned_start = ((haystack.as_ptr() as usize) & !0xF) as *const u8;

        // If we are already aligned, there's nothing to do
        if aligned_start == haystack.as_ptr() {
            return None;
        }

        let aligned_end = unsafe { aligned_start.offset(BYTES_PER_OPERATION as isize) };

        let leading_junk = haystack.as_ptr() as usize - aligned_start as usize;
        let leading_len = min(haystack.len() + leading_junk, BYTES_PER_OPERATION);

        let leading = unsafe { slice::from_raw_parts(aligned_start, leading_len) };

        let bytes_until_alignment = if leading_len == BYTES_PER_OPERATION {
            aligned_end as usize - haystack.as_ptr() as usize
        } else {
            haystack.len()
        };

        Some(Misalignment {
            leading,
            leading_junk,
            bytes_until_alignment,
        })
    }
}

// TODO: Does x86 actually support this instruction?

#[cfg(test)]
mod test {
    extern crate libc;
    extern crate quickcheck;

    use self::quickcheck::quickcheck;

    use std::{ptr, str};

    use super::*;

    lazy_static! {
        static ref SPACE: Fast = fast!(b' ');
        static ref XML_DELIM_3: Fast = fast!(b'<', b'>', b'&');
        static ref XML_DELIM_5: Fast = fast!(b'<', b'>', b'&', b'\'', b'"');
    }

    trait SliceFindPolyfill<T> {
        fn find(&self, needles: &[T]) -> Option<usize>;
    }

    impl<T> SliceFindPolyfill<T> for [T]
    where
        T: PartialEq,
    {
        fn find(&self, needles: &[T]) -> Option<usize> {
            self.iter().position(|c| needles.contains(c))
        }
    }

    #[test]
    fn works_as_find_does_for_single_bytes() {
        fn prop(s: Vec<u8>, b: u8) -> bool {
            unsafe { fast!(b).find(&s) == s.find(&[b]) }
        }
        quickcheck(prop as fn(Vec<u8>, u8) -> bool);
    }

    #[test]
    fn works_as_find_does_for_multiple_bytes() {
        fn prop(s: Vec<u8>, (b1, b2, b3, b4): (u8, u8, u8, u8)) -> bool {
            unsafe { fast!(b1, b2, b3, b4).find(&s) == s.find(&[b1, b2, b3, b4]) }
        }
        quickcheck(prop as fn(Vec<u8>, (u8, u8, u8, u8)) -> bool);
    }

    #[test]
    fn works_as_find_does_for_up_to_16_bytes() {
        fn prop(s: Vec<u8>, b: Vec<u8>) -> bool {
            let mut b = b;

            let mut needle = [0; BYTES_PER_OPERATION];
            b.resize(BYTES_PER_OPERATION, 0);

            needle.copy_from_slice(&b);

            unsafe { Fast::new(needle, BYTES_PER_OPERATION as i32).find(&s) == s.find(&needle) }
        }
        quickcheck(prop as fn(Vec<u8>, Vec<u8>) -> bool);
    }

    #[test]
    fn can_search_for_null_bytes() {
        unsafe {
            let null = fast!(b'\0');
            assert_eq!(Some(1), null.find(b"a\0"));
            assert_eq!(Some(0), null.find(b"\0"));
            assert_eq!(None, null.find(b""));
        }
    }

    #[test]
    fn can_search_in_null_bytes() {
        let a = fast!(b'a');
        unsafe {
            assert_eq!(Some(1), a.find(b"\0a"));
            assert_eq!(None, a.find(b"\0"));
        }
    }

    #[test]
    fn space_is_found() {
        unsafe {
            // Since the algorithm operates on 16-byte chunks, it's
            // important to cover tests around that boundary. Since 16
            // isn't that big of a number, we might as well do all of
            // them.

            assert_eq!(Some(0), SPACE.find(b" "));
            assert_eq!(Some(1), SPACE.find(b"0 "));
            assert_eq!(Some(2), SPACE.find(b"01 "));
            assert_eq!(Some(3), SPACE.find(b"012 "));
            assert_eq!(Some(4), SPACE.find(b"0123 "));
            assert_eq!(Some(5), SPACE.find(b"01234 "));
            assert_eq!(Some(6), SPACE.find(b"012345 "));
            assert_eq!(Some(7), SPACE.find(b"0123456 "));
            assert_eq!(Some(8), SPACE.find(b"01234567 "));
            assert_eq!(Some(9), SPACE.find(b"012345678 "));
            assert_eq!(Some(10), SPACE.find(b"0123456789 "));
            assert_eq!(Some(11), SPACE.find(b"0123456789A "));
            assert_eq!(Some(12), SPACE.find(b"0123456789AB "));
            assert_eq!(Some(13), SPACE.find(b"0123456789ABC "));
            assert_eq!(Some(14), SPACE.find(b"0123456789ABCD "));
            assert_eq!(Some(15), SPACE.find(b"0123456789ABCDE "));
            assert_eq!(Some(16), SPACE.find(b"0123456789ABCDEF "));
            assert_eq!(Some(17), SPACE.find(b"0123456789ABCDEFG "));
        }
    }

    #[test]
    fn space_not_found() {
        unsafe {
            // Since the algorithm operates on 16-byte chunks, it's
            // important to cover tests around that boundary. Since 16
            // isn't that big of a number, we might as well do all of
            // them.

            assert_eq!(None, SPACE.find(b""));
            assert_eq!(None, SPACE.find(b"0"));
            assert_eq!(None, SPACE.find(b"01"));
            assert_eq!(None, SPACE.find(b"012"));
            assert_eq!(None, SPACE.find(b"0123"));
            assert_eq!(None, SPACE.find(b"01234"));
            assert_eq!(None, SPACE.find(b"012345"));
            assert_eq!(None, SPACE.find(b"0123456"));
            assert_eq!(None, SPACE.find(b"01234567"));
            assert_eq!(None, SPACE.find(b"012345678"));
            assert_eq!(None, SPACE.find(b"0123456789"));
            assert_eq!(None, SPACE.find(b"0123456789A"));
            assert_eq!(None, SPACE.find(b"0123456789AB"));
            assert_eq!(None, SPACE.find(b"0123456789ABC"));
            assert_eq!(None, SPACE.find(b"0123456789ABCD"));
            assert_eq!(None, SPACE.find(b"0123456789ABCDE"));
            assert_eq!(None, SPACE.find(b"0123456789ABCDEF"));
            assert_eq!(None, SPACE.find(b"0123456789ABCDEFG"));
        }
    }

    #[test]
    fn works_on_nonaligned_beginnings() {
        unsafe {
            // We have special code for strings that don't lie on 16-byte
            // boundaries. Since allocation seems to happen on these
            // boundaries by default, let's walk around a bit.

            let s = b"0123456789ABCDEF ".to_vec();

            assert_eq!(Some(16), SPACE.find(&s[0..]));
            assert_eq!(Some(15), SPACE.find(&s[1..]));
            assert_eq!(Some(14), SPACE.find(&s[2..]));
            assert_eq!(Some(13), SPACE.find(&s[3..]));
            assert_eq!(Some(12), SPACE.find(&s[4..]));
            assert_eq!(Some(11), SPACE.find(&s[5..]));
            assert_eq!(Some(10), SPACE.find(&s[6..]));
            assert_eq!(Some(9), SPACE.find(&s[7..]));
            assert_eq!(Some(8), SPACE.find(&s[8..]));
            assert_eq!(Some(7), SPACE.find(&s[9..]));
            assert_eq!(Some(6), SPACE.find(&s[10..]));
            assert_eq!(Some(5), SPACE.find(&s[11..]));
            assert_eq!(Some(4), SPACE.find(&s[12..]));
            assert_eq!(Some(3), SPACE.find(&s[13..]));
            assert_eq!(Some(2), SPACE.find(&s[14..]));
            assert_eq!(Some(1), SPACE.find(&s[15..]));
            assert_eq!(Some(0), SPACE.find(&s[16..]));
            assert_eq!(None, SPACE.find(&s[17..]));
        }
    }

    #[test]
    fn xml_delim_3_is_found() {
        unsafe {
            assert_eq!(Some(0), XML_DELIM_3.find(b"<"));
            assert_eq!(Some(0), XML_DELIM_3.find(b">"));
            assert_eq!(Some(0), XML_DELIM_3.find(b"&"));
            assert_eq!(None, XML_DELIM_3.find(b""));
        }
    }

    #[test]
    fn xml_delim_5_is_found() {
        unsafe {
            assert_eq!(Some(0), XML_DELIM_5.find(b"<"));
            assert_eq!(Some(0), XML_DELIM_5.find(b">"));
            assert_eq!(Some(0), XML_DELIM_5.find(b"&"));
            assert_eq!(Some(0), XML_DELIM_5.find(b"'"));
            assert_eq!(Some(0), XML_DELIM_5.find(b"\""));
            assert_eq!(None, XML_DELIM_5.find(b""));
        }
    }

    #[cfg(target_os = "macos")]
    const MAP_ANONYMOUS: libc::int32_t = libc::MAP_ANON;

    #[cfg(not(target_os = "macos"))]
    const MAP_ANONYMOUS: libc::int32_t = libc::MAP_ANONYMOUS;

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
    fn works_at_page_boundary() {
        // PCMPxSTRx instructions are known to read 16 bytes at a
        // time. This behaviour may cause accidental segfaults by
        // reading past the page boundary.
        //
        // For now, this test failing crashes the whole test
        // suite. This could be fixed by setting a custom signal
        // handler, though Rust lacks such facilities at the moment.

        // Allocate a 16-byte string at page boundary.  To verify this
        // test, set protect=false to prevent segfaults.
        let text = alloc_guarded_string("0123456789abcdef", true);

        // Will search for the last char
        let needle = fast!(b'f');

        // Check all suffixes of our 16-byte string
        for offset in 0..text.len() {
            let tail = &text[offset..];
            unsafe {
                assert_eq!(Some(tail.len() - 1), needle.find(tail.as_bytes()));
            }
        }
    }
}