#![feature(test)]
#![feature(asm)]
#![feature(core)]

use std::str::pattern::{Pattern,Searcher,SearchStep};

#[derive(Debug,Copy,Clone)]
pub struct ByteSearch {
    pub needle: u64,
    pub count: u8,
}

impl ByteSearch {
    pub fn new() -> ByteSearch {
        ByteSearch { needle: 0, count: 0 }
    }

    pub fn push(&mut self, byte: u8) {
        assert!(self.count < 8);
        self.needle <<= 8;
        self.needle |= byte as u64;
        self.count += 1;
    }
}

#[derive(Debug,Copy,Clone)]
pub struct ByteSearcher<'a> {
    haystack: &'a str,
    offset: usize,
    needle: ByteSearch,
}

impl<'a> Pattern<'a> for ByteSearch {
    type Searcher = ByteSearcher<'a>;

    fn into_searcher(self, haystack: &'a str) -> ByteSearcher<'a> {
        ByteSearcher { haystack: haystack, offset: 0, needle: self }
    }
}

unsafe impl<'a> Searcher<'a> for ByteSearcher<'a> {
    fn haystack(&self) -> &'a str { self.haystack }

    #[inline]
    fn next(&mut self) -> SearchStep {
        if self.offset >= self.haystack.len() { return SearchStep::Done }

        let left_to_search = &self.haystack[self.offset..]; // TODO: unchecked_slice?
        let idx = find(left_to_search, self.needle);

        let next_offset = idx.unwrap_or(self.haystack.len());

        let (res, next_offset) = if next_offset == 0 {
            (SearchStep::Match(self.offset, self.offset + 1), self.offset + 1)
        } else {
            (SearchStep::Reject(self.offset, next_offset), next_offset)
        };

        self.offset = next_offset;
        res
    }
}

#[inline(never)]
fn find(haystack: &str, needle: ByteSearch) -> Option<usize> {
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
                 "x"(needle.needle),
                 "{rdx}"(len),
                 "{rax}"(needle.count),
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

#[cfg(test)]
mod test {
    extern crate quickcheck;

    use super::ByteSearch;
    use self::quickcheck::quickcheck;

    pub const SPACE: ByteSearch       = ByteSearch { needle: 0x0000000000000020, count: 1 };
    // < > &
    pub const XML_DELIM_3: ByteSearch = ByteSearch { needle: 0x00000000003c3e26, count: 3 };
    // < > & ' "
    pub const XML_DELIM_5: ByteSearch = ByteSearch { needle: 0x0000003c3e262722, count: 5 };

    pub fn find_space(haystack: &str) -> Option<usize> {
        super::find(haystack, SPACE)
    }

    pub fn find_xml_delim_3(haystack: &str) -> Option<usize> {
        super::find(haystack, XML_DELIM_3)
    }

    pub fn find_xml_delim_5(haystack: &str) -> Option<usize> {
        super::find(haystack, XML_DELIM_5)
    }

    #[test]
    fn works_as_find_does_for_single_character() {
        fn prop(s: String) -> bool {
            find_space(&s) == s.find(' ')
        }
        quickcheck(prop as fn(String) -> bool);
    }

    #[test]
    fn works_as_find_does_for_single_character2() {
        fn prop(s: String) -> bool {
            s.find(SPACE) == s.find(' ')
        }
        quickcheck(prop as fn(String) -> bool);
    }

    #[test]
    fn works_as_find_does_for_single_character3() {
        fn prop(s: String) -> bool {
            s.find(SPACE) == find_space(&s)
        }
        quickcheck(prop as fn(String) -> bool);
    }

    #[test]
    fn space_is_found() {
        // Since the algorithm operates on 16-byte chunks, it's
        // important to cover tests around that boundary. Since 16
        // isn't that big of a number, we might as well do all of
        // them.

        assert_eq!(Some(0),  find_space(" "));
        assert_eq!(Some(1),  find_space("0 "));
        assert_eq!(Some(2),  find_space("01 "));
        assert_eq!(Some(3),  find_space("012 "));
        assert_eq!(Some(4),  find_space("0123 "));
        assert_eq!(Some(5),  find_space("01234 "));
        assert_eq!(Some(6),  find_space("012345 "));
        assert_eq!(Some(7),  find_space("0123456 "));
        assert_eq!(Some(8),  find_space("01234567 "));
        assert_eq!(Some(9),  find_space("012345678 "));
        assert_eq!(Some(10), find_space("0123456789 "));
        assert_eq!(Some(11), find_space("0123456789A "));
        assert_eq!(Some(12), find_space("0123456789AB "));
        assert_eq!(Some(13), find_space("0123456789ABC "));
        assert_eq!(Some(14), find_space("0123456789ABCD "));
        assert_eq!(Some(15), find_space("0123456789ABCDE "));
        assert_eq!(Some(16), find_space("0123456789ABCDEF "));
        assert_eq!(Some(17), find_space("0123456789ABCDEFG "));
    }

    #[test]
    fn space_not_found() {
        // Since the algorithm operates on 16-byte chunks, it's
        // important to cover tests around that boundary. Since 16
        // isn't that big of a number, we might as well do all of
        // them.

        assert_eq!(None, find_space(""));
        assert_eq!(None, find_space("0"));
        assert_eq!(None, find_space("01"));
        assert_eq!(None, find_space("012"));
        assert_eq!(None, find_space("0123"));
        assert_eq!(None, find_space("01234"));
        assert_eq!(None, find_space("012345"));
        assert_eq!(None, find_space("0123456"));
        assert_eq!(None, find_space("01234567"));
        assert_eq!(None, find_space("012345678"));
        assert_eq!(None, find_space("0123456789"));
        assert_eq!(None, find_space("0123456789A"));
        assert_eq!(None, find_space("0123456789AB"));
        assert_eq!(None, find_space("0123456789ABC"));
        assert_eq!(None, find_space("0123456789ABCD"));
        assert_eq!(None, find_space("0123456789ABCDE"));
        assert_eq!(None, find_space("0123456789ABCDEF"));
        assert_eq!(None, find_space("0123456789ABCDEFG"));
    }

    #[test]
    fn xml_delim_3_is_found() {
        assert_eq!(Some(0), find_xml_delim_3("<"));
        assert_eq!(Some(0), find_xml_delim_3(">"));
        assert_eq!(Some(0), find_xml_delim_3("&"));
        assert_eq!(None,    find_xml_delim_3(""));
    }

    #[test]
    fn xml_delim_5_is_found() {
        assert_eq!(Some(0), find_xml_delim_5("<"));
        assert_eq!(Some(0), find_xml_delim_5(">"));
        assert_eq!(Some(0), find_xml_delim_5("&"));
        assert_eq!(Some(0), find_xml_delim_5("'"));
        assert_eq!(Some(0), find_xml_delim_5("\""));
        assert_eq!(None,    find_xml_delim_5(""));
    }
}

#[cfg(test)]
mod bench {
    extern crate test;

    use super::test::{find_space,find_xml_delim_3,find_xml_delim_5};
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
        bench_space(b, |hs| find_space(hs))
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
        bench_xml_delim_3(b, |hs| find_xml_delim_3(hs))
    }

    #[bench]
    fn xml_delim_3_find_char_set(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| hs.find(&['<', '>', '&'][..]))
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
        bench_xml_delim_5(b, |hs| find_xml_delim_5(hs))
    }

    #[bench]
    fn xml_delim_5_find_char_set(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| hs.find(&['<', '>', '&', '\'', '"'][..]))
    }
}
