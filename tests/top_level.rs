use jetscii::{bytes, ByteSubstring, Bytes, BytesConst};
use lazy_static::lazy_static;
use memmap2::MmapMut;
use proptest::prelude::*;
use region::Protection;
use std::{fmt, str};

lazy_static! {
    static ref SPACE: BytesConst = bytes!(b' ');
    static ref XML_DELIM_3: BytesConst = bytes!(b'<', b'>', b'&');
    static ref XML_DELIM_5: BytesConst = bytes!(b'<', b'>', b'&', b'\'', b'"');
}

trait SliceFindPolyfill<T> {
    fn find_any(&self, needles: &[T]) -> Option<usize>;
    fn find_seq(&self, needle: &[T]) -> Option<usize>;
}

impl<T> SliceFindPolyfill<T> for [T]
where
    T: PartialEq,
{
    fn find_any(&self, needles: &[T]) -> Option<usize> {
        self.iter().position(|c| needles.contains(c))
    }

    fn find_seq(&self, needle: &[T]) -> Option<usize> {
        (0..self.len()).find(|&l| self[l..].starts_with(needle))
    }
}

struct Haystack {
    data: Vec<u8>,
    start: usize,
}

impl Haystack {
    fn without_start(&self) -> &[u8] {
        &self.data
    }

    fn with_start(&self) -> &[u8] {
        &self.data[self.start..]
    }
}

// Knowing the address of the data can be important
impl fmt::Debug for Haystack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Haystack")
            .field("data", &self.data)
            .field("(addr)", &self.data.as_ptr())
            .field("start", &self.start)
            .finish()
    }
}

/// Creates a set of bytes and an offset inside them. Allows
/// checking arbitrary memory offsets, not just where the
/// allocator placed a value.
fn haystack() -> BoxedStrategy<Haystack> {
    any::<Vec<u8>>()
        .prop_flat_map(|data| {
            let len = 0..=data.len();
            (Just(data), len)
        })
        .prop_map(|(data, start)| Haystack { data, start })
        .boxed()
}

#[derive(Debug)]
struct Needle {
    data: [u8; 16],
    len: usize,
}

impl Needle {
    fn as_slice(&self) -> &[u8] {
        &self.data[..self.len]
    }
}

/// Creates an array and the number of valid values
fn needle() -> BoxedStrategy<Needle> {
    (any::<[u8; 16]>(), 0..=16_usize)
        .prop_map(|(data, len)| Needle { data, len })
        .boxed()
}

proptest! {
    #[test]
    fn works_as_find_does_for_up_to_and_including_16_bytes(
        (haystack, needle) in (haystack(), needle())
    ) {
       let haystack = haystack.without_start();

       let us = Bytes::new(needle.data, needle.len as i32, |b| needle.as_slice().contains(&b)).find(haystack);
       let them = haystack.find_any(needle.as_slice());
       assert_eq!(us, them);
    }

    #[test]
    fn iter_works(
        (haystack, needle) in (haystack(), needle())
    ) {
       let haystack = haystack.without_start();

       let bytes = Bytes::new(needle.data, needle.len as i32, |b| needle.as_slice().contains(&b));
       let mut us = bytes.iter(haystack);
       let mut them = haystack.iter().enumerate().filter_map(|(i, b)| {
            if needle.as_slice().contains(b) {
                Some(i)
            } else {
                None
            }
       });
       loop {
           match (us.next(), them.next()) {
                (Some(us), Some(them)) => { assert_eq!(us, them); }
                (Some(_), None) => panic!("them iterator ended before us"),
                (None, Some(_)) => panic!("us iterator ended before them"),
                (None, None) => break,
           }
       }
    }

    #[test]
    fn works_as_find_does_for_various_memory_offsets(
        (needle, haystack) in (needle(), haystack())
    ) {
        let haystack = haystack.with_start();

        let us = Bytes::new(needle.data, needle.len as i32, |b| needle.as_slice().contains(&b)).find(haystack);
        let them = haystack.find_any(needle.as_slice());
        assert_eq!(us, them);
    }
}

#[test]
fn can_search_for_null_bytes() {
    let null = bytes!(b'\0');
    assert_eq!(Some(1), null.find(b"a\0"));
    assert_eq!(Some(0), null.find(b"\0"));
    assert_eq!(None, null.find(b""));
}

#[test]
fn can_search_in_null_bytes() {
    let a = bytes!(b'a');
    assert_eq!(Some(1), a.find(b"\0a"));
    assert_eq!(None, a.find(b"\0"));
}

#[test]
fn space_is_found() {
    // Since the simd algorithm operates on 16-byte chunks, it's
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

#[test]
fn space_not_found() {
    // Since the simd algorithm operates on 16-byte chunks, it's
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

#[test]
fn works_on_nonaligned_beginnings() {
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

#[test]
fn misalignment_does_not_cause_a_false_positive_before_start() {
    const AAAA: u8 = 0x01;

    let needle = Needle {
        data: [
            AAAA, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00,
        ],
        len: 1,
    };
    let haystack = Haystack {
        data: vec![
            AAAA, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ],
        start: 1,
    };

    let haystack = haystack.with_start();

    // Needs to trigger the misalignment code
    assert_ne!(0, (haystack.as_ptr() as usize) % 16);
    // There are 64 bits in the mask and we check to make sure the
    // result is less than the haystack
    assert!(haystack.len() > 64);

    let us = Bytes::new(needle.data, needle.len as i32, |b| {
        needle.as_slice().contains(&b)
    })
    .find(haystack);
    assert_eq!(None, us);
}

#[test]
fn xml_delim_3_is_found() {
    assert_eq!(Some(0), XML_DELIM_3.find(b"<"));
    assert_eq!(Some(0), XML_DELIM_3.find(b">"));
    assert_eq!(Some(0), XML_DELIM_3.find(b"&"));
    assert_eq!(None, XML_DELIM_3.find(b""));
}

#[test]
fn xml_delim_5_is_found() {
    assert_eq!(Some(0), XML_DELIM_5.find(b"<"));
    assert_eq!(Some(0), XML_DELIM_5.find(b">"));
    assert_eq!(Some(0), XML_DELIM_5.find(b"&"));
    assert_eq!(Some(0), XML_DELIM_5.find(b"'"));
    assert_eq!(Some(0), XML_DELIM_5.find(b"\""));
    assert_eq!(None, XML_DELIM_5.find(b""));
}

#[test]
fn do_not_find_zeros_after_end() {
    // The simd algorithm will end up with a partial vector when the haystack isn't
    // a multiple of 16 bytes. The rest of the vector will be filled with zeros.
    // Ensure we don't find a match in the remainder of that vector, which would be considered
    // after the end of the haystack.
    let needle = bytes!(b'\0', b'*');
    let haystack = b"123456";
    assert_eq!(None, needle.find(haystack));
}

proptest! {
    #[test]
    fn works_as_find_does_for_byte_substrings(
        (needle, haystack) in (any::<Vec<u8>>(), any::<Vec<u8>>())
    ) {
        if !needle.is_empty() {
            let us = {
                let s = ByteSubstring::new(&needle);
                s.find(&haystack)
            };
            let them = haystack.find_seq(&needle);
            assert_eq!(us, them);
        }
    }
}

#[test]
fn byte_substring_is_found() {
    let substr = ByteSubstring::new(b"zz");
    assert_eq!(Some(0), substr.find(b"zz"));
    assert_eq!(Some(1), substr.find(b"0zz"));
    assert_eq!(Some(2), substr.find(b"01zz"));
    assert_eq!(Some(3), substr.find(b"012zz"));
    assert_eq!(Some(4), substr.find(b"0123zz"));
    assert_eq!(Some(5), substr.find(b"01234zz"));
    assert_eq!(Some(6), substr.find(b"012345zz"));
    assert_eq!(Some(7), substr.find(b"0123456zz"));
    assert_eq!(Some(8), substr.find(b"01234567zz"));
    assert_eq!(Some(9), substr.find(b"012345678zz"));
    assert_eq!(Some(10), substr.find(b"0123456789zz"));
    assert_eq!(Some(11), substr.find(b"0123456789Azz"));
    assert_eq!(Some(12), substr.find(b"0123456789ABzz"));
    assert_eq!(Some(13), substr.find(b"0123456789ABCzz"));
    assert_eq!(Some(14), substr.find(b"0123456789ABCDzz"));
    assert_eq!(Some(15), substr.find(b"0123456789ABCDEzz"));
    assert_eq!(Some(16), substr.find(b"0123456789ABCDEFzz"));
    assert_eq!(Some(17), substr.find(b"0123456789ABCDEFGzz"));
}

#[test]
fn byte_substring_is_not_found() {
    let substr = ByteSubstring::new(b"zz");
    assert_eq!(None, substr.find(b""));
    assert_eq!(None, substr.find(b"0"));
    assert_eq!(None, substr.find(b"01"));
    assert_eq!(None, substr.find(b"012"));
    assert_eq!(None, substr.find(b"0123"));
    assert_eq!(None, substr.find(b"01234"));
    assert_eq!(None, substr.find(b"012345"));
    assert_eq!(None, substr.find(b"0123456"));
    assert_eq!(None, substr.find(b"01234567"));
    assert_eq!(None, substr.find(b"012345678"));
    assert_eq!(None, substr.find(b"0123456789"));
    assert_eq!(None, substr.find(b"0123456789A"));
    assert_eq!(None, substr.find(b"0123456789AB"));
    assert_eq!(None, substr.find(b"0123456789ABC"));
    assert_eq!(None, substr.find(b"0123456789ABCD"));
    assert_eq!(None, substr.find(b"0123456789ABCDE"));
    assert_eq!(None, substr.find(b"0123456789ABCDEF"));
    assert_eq!(None, substr.find(b"0123456789ABCDEFG"));
}

#[test]
fn byte_substring_has_false_positive() {
    // The PCMPESTRI instruction will mark the "a" before "ab" as
    // a match because it cannot look beyond the 16 byte window
    // of the haystack. We need to double-check any match to
    // ensure it completely matches.

    let substr = ByteSubstring::new(b"ab");
    assert_eq!(Some(16), substr.find(b"aaaaaaaaaaaaaaaaab"))
    //   this "a" is a false positive ~~~~~~~~~~~~~~~^
}

#[test]
fn byte_substring_needle_is_longer_than_16_bytes() {
    let needle = b"0123456789abcdefg";
    let haystack = b"0123456789abcdefgh";
    assert_eq!(Some(0), ByteSubstring::new(needle).find(haystack));
}

fn with_guarded_string(value: &str, f: impl FnOnce(&str)) {
    // Allocate a string that ends directly before a
    // read-protected page.

    let page_size = region::page::size();
    assert!(value.len() <= page_size);

    // Map two rw-accessible pages of anonymous memory
    let mut mmap = MmapMut::map_anon(2 * page_size).unwrap();

    let (first_page, second_page) = mmap.split_at_mut(page_size);

    // Prohibit any access to the second page, so that any attempt
    // to read or write it would trigger a segfault
    unsafe {
        region::protect(second_page.as_ptr(), page_size, Protection::NONE).unwrap();
    }

    // Copy bytes to the end of the first page
    let dest = &mut first_page[page_size - value.len()..];
    dest.copy_from_slice(value.as_bytes());
    f(unsafe { str::from_utf8_unchecked(dest) });
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
    with_guarded_string("0123456789abcdef", |text| {
        // Will search for the last char
        let needle = bytes!(b'f');

        // Check all suffixes of our 16-byte string
        for offset in 0..text.len() {
            let tail = &text[offset..];
            assert_eq!(Some(tail.len() - 1), needle.find(tail.as_bytes()));
        }
    });
}

#[test]
fn does_not_access_memory_after_haystack_when_haystack_is_multiple_of_16_bytes_and_no_match() {
    // For now, this test failing crashes the whole test
    // suite. This could be fixed by setting a custom signal
    // handler, though Rust lacks such facilities at the moment.
    with_guarded_string("0123456789abcdef", |text| {
        // Will search for a char not present
        let needle = bytes!(b'z');

        assert_eq!(None, needle.find(text.as_bytes()));
    });
}
