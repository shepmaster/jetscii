#![feature(test)]
#![feature(asm)]

struct ByteSearch {
    needle: u64,
    count: u8,
}

impl ByteSearch {
    fn new() -> ByteSearch {
        ByteSearch { needle: 0, count: 0 }
    }

    fn push(&mut self, byte: u8) {
        assert!(self.count < 8);
        self.needle <<= 8;
        self.needle |= byte as u64;
        self.count += 1;
    }
}

const SPACE: u64 = b' ' as u64;
const XML_DELIM_3: u64 = (b'<' as u64) << 16 | (b'>' as u64) << 8 | b'&' as u64;
const XML_DELIM_5: u64 = 0x3c3e262722;

fn find_space(haystack: &str) -> Option<usize> {
    find(haystack, SPACE, 1)
}

fn find_xml_delim_3(haystack: &str) -> Option<usize> {
    find(haystack, XML_DELIM_3, 3)
}

fn find_xml_delim_5(haystack: &str) -> Option<usize> {
    find(haystack, XML_DELIM_5, 5)
}

#[inline(never)]
fn find(haystack: &str, needle: u64, needle_len: u8) -> Option<usize> {
    let haystack = haystack.as_bytes();

    let ptr = haystack.as_ptr();
    let mut offset = 0;
    let mut len = haystack.len();

    let mut res: usize;

    loop {
        unsafe {
            asm!("pcmpestri $$0, ($1, $5), $2"
                 : // output operands
                 "={ecx}"(res)
                 : // input operands
                 "r"(ptr),
                 "x"(needle),
                 "{rdx}"(len),
                 "{rax}"(needle_len),
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

fn main() {
    let mut bs = ByteSearch::new();
    bs.push(b'<');
    bs.push(b'>');
    bs.push(b'&');
    bs.push(b'\'');
    bs.push(b'"');
    println!("{:x}, {}", bs.needle, bs.count);

    let res = find_space("hello world");
    println!("Found a space at {:?}", res);
}

#[test]
fn space_is_found() {
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

extern crate test;

#[inline(always)]
fn bench_space<F>(b: &mut test::Bencher, f: F)
    where F: Fn(&str) -> Option<usize>
{
    let mut haystack:String = ::std::iter::repeat("a").take(5 * 1024 * 1024).collect();
    haystack.push(' ');

    b.iter(|| test::black_box(f(&haystack)));
    b.bytes = haystack.len() as u64;
}

#[bench]
fn space_assembly_5mb(b: &mut test::Bencher) {
    bench_space(b, |hs| find_space(hs))
}

#[bench]
fn space_find_string_5mb(b: &mut test::Bencher) {
    bench_space(b, |hs| hs.find(" "))
}

#[bench]
fn space_find_char_5mb(b: &mut test::Bencher) {
    bench_space(b, |hs| hs.find(' '))
}

#[bench]
fn space_find_char_set_5mb(b: &mut test::Bencher) {
    bench_space(b, |hs| hs.find(&[' '][..]))
}

#[bench]
fn space_find_byte_5mb(b: &mut test::Bencher) {
    bench_space(b, |hs| hs.as_bytes().iter().position(|&v| v == b' '))
}

#[inline(always)]
fn bench_xml_delim_3<F>(b: &mut test::Bencher, f: F)
    where F: Fn(&str) -> Option<usize>
{
    let mut haystack:String = ::std::iter::repeat("a").take(5 * 1024 * 1024).collect();
    haystack.push('&');

    b.iter(|| test::black_box(f(&haystack)));
    b.bytes = haystack.len() as u64;
}

#[bench]
fn xml_delim_3_assembly_5mb(b: &mut test::Bencher) {
    bench_xml_delim_3(b, |hs| find_xml_delim_3(hs))
}

#[bench]
fn xml_delim_3_find_char_set_5mb(b: &mut test::Bencher) {
    bench_xml_delim_3(b, |hs| hs.find(&['<', '>', '&'][..]))
}

#[inline(always)]
fn bench_xml_delim_5<F>(b: &mut test::Bencher, f: F)
    where F: Fn(&str) -> Option<usize>
{
    let mut haystack:String = ::std::iter::repeat("a").take(2).collect();
    haystack.push('"');

    b.iter(|| test::black_box(f(&haystack)));
    b.bytes = haystack.len() as u64;
}

#[bench]
fn xml_delim_5_assembly_5mb(b: &mut test::Bencher) {
    bench_xml_delim_5(b, |hs| find_xml_delim_5(hs))
}

#[bench]
fn xml_delim_5_find_char_set_5mb(b: &mut test::Bencher) {
    bench_xml_delim_5(b, |hs| hs.find(&['<', '>', '&', '\'', '"'][..]))
}
