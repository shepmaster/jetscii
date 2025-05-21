// # Warning
//
// Everything in this module assumes that the SSE 4.2 feature is available.

use std::{cmp::min, slice};

#[cfg(target_arch = "x86")]
use std::arch::x86 as target_arch;
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64 as target_arch;

use self::target_arch::{
    __m128i, _mm_cmpestri, _mm_cmpestrm, _mm_extract_epi16, _mm_loadu_si128,
    _SIDD_CMP_EQUAL_ORDERED,
};

include!(concat!(env!("OUT_DIR"), "/src/simd_macros.rs"));

const BYTES_PER_OPERATION: usize = 16;

union TransmuteToSimd {
    simd: __m128i,
    bytes: [u8; 16],
}

trait PackedCompareControl {
    fn needle(&self) -> __m128i;
    fn needle_len(&self) -> i32;
}

#[inline]
#[target_feature(enable = "sse4.2")]
unsafe fn find_small<C, const CONTROL_BYTE: i32>(
    packed: PackedCompare<C, CONTROL_BYTE>,
    haystack: &[u8],
) -> Option<usize>
where
    C: PackedCompareControl,
{
    let mut tail = [0u8; 16];
    core::ptr::copy_nonoverlapping(haystack.as_ptr(), tail.as_mut_ptr(), haystack.len());
    let haystack = &tail[..haystack.len()];
    debug_assert!(haystack.len() < ::std::i32::MAX as usize);
    packed.cmpestri(haystack.as_ptr(), haystack.len() as i32)
}

/// The `PCMPxSTRx` instructions always read 16 bytes worth of
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
unsafe fn find<C, const CONTROL_BYTE: i32>(
    packed: PackedCompare<C, CONTROL_BYTE>,
    mut haystack: &[u8],
) -> Option<usize>
where
    C: PackedCompareControl,
{
    // FIXME: EXPLAIN SAFETY

    if haystack.is_empty() {
        return None;
    }

    if haystack.len() < 16 {
        return find_small(packed, haystack);
    }

    let mut offset = 0;

    if let Some(misaligned) = Misalignment::new(haystack) {
        if let Some(location) = packed.cmpestrm(misaligned.leading, misaligned.leading_junk) {
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
        if let Some(location) = packed.cmpestri(haystack_ptr, BYTES_PER_OPERATION as i32) {
            return Some(offset + chunk_offset + location);
        }

        haystack_ptr = haystack_ptr.offset(BYTES_PER_OPERATION as isize);
        chunk_offset += BYTES_PER_OPERATION;
    }
    haystack = &haystack[chunk_offset..];
    offset += chunk_offset;

    // No data left to search
    if haystack.is_empty() {
        return None;
    }

    find_small(packed, haystack).map(|loc| loc + offset)
}

struct PackedCompare<T, const CONTROL_BYTE: i32>(T);
impl<T, const CONTROL_BYTE: i32> PackedCompare<T, CONTROL_BYTE>
where
    T: PackedCompareControl,
{
    #[inline]
    #[target_feature(enable = "sse4.2")]
    unsafe fn cmpestrm(&self, haystack: &[u8], leading_junk: usize) -> Option<usize> {
        // TODO: document why this is ok
        let haystack = _mm_loadu_si128(haystack.as_ptr() as *const __m128i);

        let mask = _mm_cmpestrm(
            self.0.needle(),
            self.0.needle_len(),
            haystack,
            BYTES_PER_OPERATION as i32,
            CONTROL_BYTE,
        );
        let mask = _mm_extract_epi16(mask, 0) as u16;

        if mask.trailing_zeros() < 16 {
            let mut mask = mask;
            // Byte: 7 6 5 4 3 2 1 0
            // Str : &[0, 1, 2, 3, ...]
            //
            // Bit-0 corresponds to Str-0; shifting to the right
            // removes the parts of the string that don't belong to
            // us.
            mask >>= leading_junk;
            // The first 1, starting from Bit-0 and going to Bit-7,
            // denotes the position of the first match.
            if mask == 0 {
                // All of our matches were before the slice started
                None
            } else {
                let first_match = mask.trailing_zeros() as usize;
                debug_assert!(first_match < 16);
                Some(first_match)
            }
        } else {
            None
        }
    }

    #[inline]
    #[target_feature(enable = "sse4.2")]
    unsafe fn cmpestri(&self, haystack: *const u8, haystack_len: i32) -> Option<usize> {
        debug_assert!(
            (1..=16).contains(&haystack_len),
            "haystack_len was {}",
            haystack_len,
        );

        // TODO: document why this is ok
        let haystack = _mm_loadu_si128(haystack as *const __m128i);

        let location = _mm_cmpestri(
            self.0.needle(),
            self.0.needle_len(),
            haystack,
            haystack_len,
            CONTROL_BYTE,
        );

        if location < 16 {
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

#[derive(Copy, Clone)]
pub struct Bytes {
    needle: __m128i,
    needle_len: i32,
}

impl Bytes {
    pub fn new(bytes: [u8; 16], needle_len: i32) -> Self {
        Bytes {
            needle: unsafe { TransmuteToSimd { bytes }.simd },
            needle_len,
        }
    }

    #[inline]
    #[target_feature(enable = "sse4.2")]
    pub unsafe fn find(&self, haystack: &[u8]) -> Option<usize> {
        find(PackedCompare::<_, 0>(self), haystack)
    }

    pub fn iter(self, haystack: &[u8]) -> BytesIter<'_> {
        BytesIter {
            bytes: self,
            haystack,
            offset: 0,
        }
    }
}

pub struct BytesIter<'a> {
    bytes: Bytes,
    haystack: &'a [u8],
    offset: usize,
}

impl Iterator for BytesIter<'_> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let next_offset = unsafe { self.bytes.find(self.haystack) };
        match next_offset {
            Some(i) => {
                self.haystack = &self.haystack[i + 1..];
                let res = self.offset + i;
                self.offset = res + 1;
                Some(res)
            }
            None => {
                self.haystack = &[];
                None
            }
        }
    }
}

impl<'b> PackedCompareControl for &'b Bytes {
    #[inline]
    fn needle(&self) -> __m128i {
        self.needle
    }

    #[inline]
    fn needle_len(&self) -> i32 {
        self.needle_len
    }
}

pub struct ByteSubstring<'a> {
    complete_needle: &'a [u8],
    needle: __m128i,
    needle_len: i32,
}

impl<'a> ByteSubstring<'a> {
    pub fn new(needle: &'a [u8]) -> Self {
        let mut simd_needle = [0; 16];
        let len = if simd_needle.len() < needle.len() {
            simd_needle.len()
        } else {
            needle.len()
        };
        let mut i = 0;
        while i < len {
            simd_needle[i] = needle[i];
            i += 1;
        }
        ByteSubstring {
            complete_needle: needle,
            needle: unsafe { TransmuteToSimd { bytes: simd_needle }.simd },
            needle_len: len as i32,
        }
    }

    #[cfg(feature = "pattern")]
    pub fn needle_len(&self) -> usize {
        self.complete_needle.len()
    }

    #[inline]
    #[target_feature(enable = "sse4.2")]
    pub unsafe fn find(&self, haystack: &[u8]) -> Option<usize> {
        let mut offset = 0;

        while let Some(idx) = find(
            PackedCompare::<_, _SIDD_CMP_EQUAL_ORDERED>(self),
            &haystack[offset..],
        ) {
            let abs_offset = offset + idx;
            // Found a match, but is it really?
            if haystack[abs_offset..].starts_with(self.complete_needle) {
                return Some(abs_offset);
            }

            // Skip past this false positive
            offset += idx + 1;
        }

        None
    }
}

impl<'a, 'b> PackedCompareControl for &'b ByteSubstring<'a> {
    #[inline]
    fn needle(&self) -> __m128i {
        self.needle
    }

    #[inline]
    fn needle_len(&self) -> i32 {
        self.needle_len
    }
}
