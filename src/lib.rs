#![feature(asm)]
#![feature(core)]
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
//! let parts: Vec<_> = part_number.split(search).collect();
//! assert_eq!(&parts, &["86", "J52", "rev1"]);
//! ```
//!
//! For maximum performance, you can create the searcher as a constant
//! item. Print an existing AsciiChars with the debug formatter to get
//! the appropriate invocation:
//!
//! ```
//! use jetscii::AsciiChars;
//! let search = AsciiChars { needle: 0x0000000000002d3a, count: 2 };
//! let part_number = "86-J52:rev1";
//! let parts: Vec<_> = part_number.split(search).collect();
//! assert_eq!(&parts, &["86", "J52", "rev1"]);
//! ```

use std::fmt;
use std::str::pattern::{Pattern,Searcher,SearchStep};

/// Searches a string for a set of ASCII characters. Up to 8
/// characters may be used.
///
/// The instance variables are public to allow creating a AsciiChars
/// as a constant item. This is temporary until Rust has better
/// compile-time function evaluation, so consider this an **unstable**
/// interface.
#[derive(Copy,Clone)]
pub struct AsciiChars {
    pub needle: u64,
    pub count: u8,
}

impl AsciiChars {
    pub fn new() -> AsciiChars {
        AsciiChars { needle: 0, count: 0 }
    }

    /// Add a new ASCII character to the set to search for.
    ///
    /// ### Panics
    ///
    /// - If you add more than 8 characters.
    /// - If you add a non-ASCII byte.
    pub fn push(&mut self, byte: u8) {
        assert!(byte < 128);
        assert!(self.count < 8);
        self.needle <<= 8;
        self.needle |= byte as u64;
        self.count += 1;
    }

    /// Find the index of the first character in the set.
    #[inline]
    pub fn find(self, haystack: &str) -> Option<usize> {
        let haystack = haystack.as_bytes();

        let ptr = haystack.as_ptr();
        let mut offset = 0;
        let mut len = haystack.len();

        let mut res: usize;

        // Zero-length strings have a pointer set to 0x1. Even though the
        // length is zero, we still trigger a bad access exception. I
        // think this indicates that the instruction reads in 16 bytes
        // worth of memory at a time, regardless of the length instruction.
        //
        // This could also be an indication of a subtle bug, where we
        // might trigger access violations if we are near the end of a
        // page. See the comment by Renat Saifutdinov on
        // http://www.strchr.com/strcmp_and_strlen_using_sse_4.2
        // It is suggested to use an "aligned read with mask false bits"
        // to avoid the problem.
        //
        // We don't do this yet.
        if len == 0 { return None }

        loop {
            unsafe {
                asm!("pcmpestri $$0, ($1, $5), $2"
                     : // output operands
                     "={ecx}"(res)
                     : // input operands
                     "r"(ptr),
                     "x"(self.needle),
                     "{rdx}"(len),
                     "{rax}"(self.count),
                     "r"(offset)
                     : // clobbers
                     : // options
                     );
            }

            // We know if it matched if the zero flag is set (or
            // unset?), we shouldn't need to test res...
            if res == 16 {
                if len <= 16 {
                    return None;
                }

                offset += 16;
                len -= 16;
            } else {
                return Some(res + offset);
            }
        }
    }
}

impl fmt::Debug for AsciiChars {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "AsciiChars {{ needle: 0x{:016x}, count: {} }}", self.needle, self.count)
    }
}

impl<'a> Pattern<'a> for AsciiChars {
    type Searcher = AsciiCharsSearcher<'a>;

    fn into_searcher(self, haystack: &'a str) -> AsciiCharsSearcher<'a> {
        AsciiCharsSearcher { haystack: haystack, offset: 0, needle: self }
    }
}

/// An implementation of `Searcher` using `AsciiChars`
#[derive(Debug,Copy,Clone)]
pub struct AsciiCharsSearcher<'a> {
    haystack: &'a str,
    offset: usize,
    needle: AsciiChars,
}

unsafe impl<'a> Searcher<'a> for AsciiCharsSearcher<'a> {
    fn haystack(&self) -> &'a str { self.haystack }

    #[inline]
    fn next(&mut self) -> SearchStep {
        if self.offset >= self.haystack.len() { return SearchStep::Done }

        let left_to_search = &self.haystack[self.offset..]; // TODO: unchecked_slice?
        let idx = self.needle.find(left_to_search);

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

    use super::AsciiChars;
    use self::quickcheck::quickcheck;
    use std::str::pattern::{Pattern,Searcher,SearchStep};

    pub const SPACE: AsciiChars       = AsciiChars { needle: 0x0000000000000020, count: 1 };
    // a
    pub const VOWEL: AsciiChars       = AsciiChars { needle: 0x0000000000000061, count: 1 };
    // a e i o u
    pub const VOWELS: AsciiChars      = AsciiChars { needle: 0x0000006165696f75, count: 5 };
    // < > &
    pub const XML_DELIM_3: AsciiChars = AsciiChars { needle: 0x00000000003c3e26, count: 3 };
    // < > & ' "
    pub const XML_DELIM_5: AsciiChars = AsciiChars { needle: 0x0000003c3e262722, count: 5 };

    #[test]
    fn works_as_find_does_for_single_characters() {
        // Quickcheck currently only generates Strings with A-Z, a-z, 0-9
        fn prop(s: String) -> bool {
            s.find(VOWEL) == s.find('a')
        }
        quickcheck(prop as fn(String) -> bool);
    }

    #[test]
    fn works_as_find_does_for_multiple_characters() {
        // Quickcheck currently only generates Strings with A-Z, a-z, 0-9
        fn prop(s: String) -> bool {
            s.find(VOWELS) == s.find(&['a', 'e', 'i', 'o', 'u'][..])
        }
        quickcheck(prop as fn(String) -> bool);
    }

    #[test]
    fn pattern_does_not_backtrack_after_first() {
        let mut searcher = SPACE.into_searcher("hello w ");
        assert_eq!(SearchStep::Reject(0,5), searcher.next());
        assert_eq!(SearchStep::Match(5,6), searcher.next());
        assert_eq!(SearchStep::Reject(6,7), searcher.next());
        assert_eq!(SearchStep::Match(7,8), searcher.next());
        assert_eq!(SearchStep::Done, searcher.next());
    }

    #[test]
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
    fn xml_delim_3_is_found() {
        assert_eq!(Some(0), XML_DELIM_3.find("<"));
        assert_eq!(Some(0), XML_DELIM_3.find(">"));
        assert_eq!(Some(0), XML_DELIM_3.find("&"));
        assert_eq!(None,    XML_DELIM_3.find(""));
    }

    #[test]
    fn xml_delim_5_is_found() {
        assert_eq!(Some(0), XML_DELIM_5.find("<"));
        assert_eq!(Some(0), XML_DELIM_5.find(">"));
        assert_eq!(Some(0), XML_DELIM_5.find("&"));
        assert_eq!(Some(0), XML_DELIM_5.find("'"));
        assert_eq!(Some(0), XML_DELIM_5.find("\""));
        assert_eq!(None,    XML_DELIM_5.find(""));
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
    fn space_assembly(b: &mut test::Bencher) {
        bench_space(b, |hs| SPACE.find(hs))
    }

    #[bench]
    fn space_assembly_as_pattern(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.find(SPACE))
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
    fn xml_delim_3_assembly(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| XML_DELIM_3.find(hs))
    }

    #[bench]
    fn xml_delim_3_assembly_as_pattern(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| hs.find(XML_DELIM_3))
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
    fn xml_delim_5_assembly(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| XML_DELIM_5.find(hs))
    }

    #[bench]
    fn xml_delim_5_assembly_as_pattern(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| hs.find(XML_DELIM_5))
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
