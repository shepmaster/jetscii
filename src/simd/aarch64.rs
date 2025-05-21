//! AArch64 NEON SIMD implementation
//!
//! Based on [this algorithm][1] simplified somewhat by aarch64 neon (by the ability to make a
//! table of 32 bytes by combining two vectors), combined with 64 byte movemask via interleaving
//! from [here][2], and the iteration idea (using a u64 bitset of known matches) from [here][3].
//!
//! [1]: http://0x80.pl/notesen/2018-10-18-simd-byte-lookup.html
//! [2]: https://community.arm.com/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon
//! [3]: https://lemire.me/blog/2024/07/20/scan-html-even-faster-with-simd-instructions-c-and-c/

use std::arch::aarch64::*;
use std::mem::transmute;

#[derive(Copy, Clone)]
pub struct Bytes {
    bitset: uint8x16x2_t,
}

type Vector = uint8x16_t;
type Chunk = uint8x16x4_t;

/// Mapping from a number `i` in 0..=7 to a bit mask with the `i`-th bit set.
const N_TO_N_BITS_TABLE: uint8x16_t = unsafe {
    let mut bits = [0u8; 16];
    let mut i = 0u8;
    while i < 8 {
        bits[i as usize] = 1 << i;
        i += 1;
    }
    transmute(bits)
};

impl Bytes {
    pub fn new(bytes: [u8; 16], needle_len: i32) -> Self {
        assert!((0..=16).contains(&needle_len));
        let needle_len = needle_len as u8;

        // Make a bitset from the bytes to search for
        let mut bitset = [0u8; 256 / 8];
        for i in 0..needle_len {
            let i = usize::from(i);
            let value = usize::from(bytes[i]);
            let byte = value / 8;
            let bit = value % 8;
            let mask = 1 << bit;
            bitset[byte] |= mask;
        }
        let bitset = unsafe { transmute(bitset) };

        Bytes { bitset }
    }

    #[target_feature(enable = "neon")]
    pub fn find(&self, haystack: &[u8]) -> Option<usize> {
        let mut vectors = haystack.chunks_exact(size_of::<Vector>());

        let mut offset = 0;
        for vector in vectors.by_ref() {
            let vector = unsafe { vld1q_u8(vector.as_ptr()) };
            let result = self.locale_in_vector(vector);
            if let Some(element) = first_element_set(result) {
                return Some(offset + usize::from(element));
            }
            offset += size_of_val(&vector);
        }
        let remaining = vectors.remainder();
        let mut fake_vector = [0; size_of::<Vector>()];
        fake_vector[..remaining.len()].copy_from_slice(remaining);
        let vector = unsafe { vld1q_u8(fake_vector.as_ptr()) };
        let result = self.locale_in_vector(vector);
        if let Some(element) = first_element_set(result) {
            if element < remaining.len() as u8 {
                return Some(offset + usize::from(element));
            }
        }

        None
    }

    pub fn iter<'a>(self, haystack: &'a [u8]) -> BytesIter<'a> {
        BytesIter::new(self, haystack)
    }

    /// Given a vector of 16 bytes of input, return a vector of true/false values.
    ///
    /// Each element in the output will be 0/255 based on if the input byte at that position
    /// is equal to one of the values being searched for.
    #[inline]
    #[target_feature(enable = "neon")]
    fn locale_in_vector(&self, v: Vector) -> Vector {
        let high_bits = vshrq_n_u8::<3>(v);
        let low_bit_masks = vqtbl2q_u8(self.bitset, high_bits);
        let low_bits = vandq_u8(v, vdupq_n_u8(0b0111));
        let low_bits = vqtbl1q_u8(N_TO_N_BITS_TABLE, low_bits);
        vtstq_u8(low_bits, low_bit_masks)
    }

    /// Returns a u64 where each set bit indicates a match in the chunk.
    ///
    /// Takes a "deinterleaved" chunk of 4 vectors, each with 16 bytes.
    /// The first element of the input is the first element of the first vector,
    /// the second element of the input is the first element of the second vector,
    /// the fifth element of the input is the second element of the first vector, etc.
    #[inline]
    #[target_feature(enable = "neon")]
    fn locate_in_chunk(&self, chunk: Chunk) -> u64 {
        let chunk_values = [chunk.0, chunk.1, chunk.2, chunk.3];
        // Get 4 "bool" vectors indicating if each element in the chunk is a match
        let matching_elements = chunk_values.map(|v| self.locale_in_vector(v));

        // Pack bits from the 4 vectors into a single vector

        // shift the second vector right by one, insert the top bit from the first vector
        // The top two bits each element of temp0 are from the first and second vector
        let temp0 = vsriq_n_u8::<1>(matching_elements[1], matching_elements[0]);

        // shift the fourth vector right by one, insert the top bit from the third vector
        // The top two bits each element of temp1 are from the third and fourth vector
        let temp1 = vsriq_n_u8::<1>(matching_elements[3], matching_elements[2]);

        // shift temp1 (the top two bits of which are from the third and fourth vector) right by 2,
        // insert the top two bits from temp0 (the top two bits of which are from the first and
        // second vector)
        // The top four bits of each element of temp2 are from the first, second, third, and fourth
        // vector
        let temp2 = vsriq_n_u8::<2>(temp1, temp0);

        // duplicate the top 4 bits into the bottom 4 bits of each element
        let temp3 = vsriq_n_u8::<4>(temp2, temp2);

        // The top/bottom 4 bits of each element are the same, so converting to a 64 bit bitset
        // takes those 4 bits from each element and places them next to each other
        vector_to_bitset(temp3)
    }
}

pub struct BytesIter<'a> {
    /// The bytes to search for
    bytes: Bytes,
    /// The remaining haystack (after the current bitset chunk)
    haystack: &'a [u8],
    /// The current offset (from the start of the original haystack)
    offset: usize,
    /// A reversed bitset of the the current chunk
    ///
    /// e.g. the most significant bit is set if the next byte matches one of the searched bytes
    current_bitset: u64,
}

impl<'a> BytesIter<'a> {
    fn new(bytes: Bytes, haystack: &'a [u8]) -> Self {
        Self {
            bytes,
            haystack,
            offset: 0,
            current_bitset: 0,
        }
    }

    #[target_feature(enable = "neon")]
    fn fill_bitset(&mut self) {
        while let Some((chunk, rest)) = self.haystack.split_at_checked(size_of::<Chunk>()) {
            self.haystack = rest;
            let chunk = unsafe { vld4q_u8(chunk.as_ptr()) };
            let bitset = self.bytes.locate_in_chunk(chunk);
            if bitset != 0 {
                // aarch64 doesn't have a count trailing zeros instruction, so
                // reverse the bits so we use leading_zeros instead
                self.current_bitset = bitset.reverse_bits();
                return;
            }
            self.offset += size_of_val(&chunk);
        }
        let mut fake_chunk = [0; size_of::<Chunk>()];
        fake_chunk[..self.haystack.len()].copy_from_slice(self.haystack);
        let chunk = unsafe { vld4q_u8(fake_chunk.as_ptr()) };
        self.current_bitset = self.bytes.locate_in_chunk(chunk);
        let mask = !(u64::MAX << self.haystack.len() as u64);
        self.current_bitset &= mask;
        // aarch64 doesn't have a count trailing zeros instruction, so
        // reverse the bits so we use leading_zeros instead
        self.current_bitset = self.current_bitset.reverse_bits();
        self.haystack = &[];
    }
}

impl<'a> Iterator for BytesIter<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let mut first_bit = self.current_bitset.leading_zeros();
        if first_bit == 64 {
            unsafe {
                self.fill_bitset();
            }
            first_bit = self.current_bitset.leading_zeros();
            if first_bit == 64 {
                return None;
            }
        }
        // toggle the highest bit
        self.current_bitset ^= 1 << (63 - first_bit);
        let result = self.offset + first_bit as usize;
        if self.current_bitset == 0 {
            self.offset += 64;
        }
        Some(result)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let min = self.current_bitset.count_ones() as usize;
        let max = min.checked_add(self.haystack.len());
        (min, max)
    }

    // We can be a little faster by avoiding iterating through the bits by counting bits directly
    fn count(self) -> usize {
        let mut count = self.current_bitset.count_ones() as usize;

        let mut chunks = self.haystack.chunks_exact(size_of::<Chunk>());
        for chunk in chunks.by_ref() {
            let chunk = unsafe { vld4q_u8(chunk.as_ptr()) };
            let result = unsafe { self.bytes.locate_in_chunk(chunk) };
            count += result.count_ones() as usize;
        }
        let remaining = chunks.remainder();
        let mut fake_chunk = [0; size_of::<Chunk>()];
        fake_chunk[..remaining.len()].copy_from_slice(remaining);
        let chunk = unsafe { vld4q_u8(fake_chunk.as_ptr()) };
        let result = unsafe { self.bytes.locate_in_chunk(chunk) };
        let mask = !(u64::MAX << self.haystack.len() as u64);
        count += (result & mask).count_ones() as usize;
        count
    }
}

/// Convert a vector into a u64
///
/// Returns a value where the first 4 bits are the low 4 bits of the first element,
/// the next 4 bits are the high 4 bits of the second element, and so on.
///
/// For a "bool" vector (where every element is either 255 or 0), the resulting u64
/// will have groups of 4 bits from each element. e.g. the number of trailing zero bits
/// is 4 times the number of trailing zero elements.
#[target_feature(enable = "neon")]
fn vector_to_bitset(vector: Vector) -> u64 {
    let result_vector = vshrn_n_u16::<4>(vreinterpretq_u16_u8(vector));
    vget_lane_u64::<0>(vreinterpret_u64_u8(result_vector))
}

/// Find the first element in a "bool" vector that is set or None if all elements are zero
#[target_feature(enable = "neon")]
fn first_element_set(vector: Vector) -> Option<u8> {
    let bitset = vector_to_bitset(vector);
    let first_bit = bitset.trailing_zeros() / 4;
    if first_bit < 16 {
        Some(first_bit as u8)
    } else {
        None
    }
}
