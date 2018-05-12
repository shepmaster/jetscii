use std::marker::PhantomData;

include!(concat!(env!("OUT_DIR"), "/src/v2_macros.rs"));

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
mod simd;

#[cfg(not(target_feature = "sse4.2"))]
mod fallback;

#[cfg(feature = "pattern")]
mod pattern;

pub struct Bytes<F>
where
    F: Fn(u8) -> bool,
{
    // Include this implementation only when compiling for x86_64 as
    // that's the only platform that we support.
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    fast: simd::Fast,

    // If we are *guaranteed* to have SSE 4.2, then there's no reason
    // to have this implementation.
    #[cfg(not(target_feature = "sse4.2"))]
    fallback: fallback::Fallback<F>,

    // Since we might not use the fallback implementation, we add this
    // to avoid unused type parameters.
    _fallback: PhantomData<F>,
}

impl<F> Bytes<F>
where
    F: Fn(u8) -> bool,
{
    #[allow(unused_variables)]
    pub /* const */ fn new(bytes: [u8; 16], len: i32, fallback: F) -> Self {
        Bytes {
            #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
            fast: simd::Fast::new(bytes, len),

            #[cfg(not(target_feature = "sse4.2"))]
            fallback: fallback::Fallback::new(fallback),

            _fallback: PhantomData,
        }
    }

    #[inline]
    pub fn find(&self, haystack: &[u8]) -> Option<usize> {
        // If we can tell at compile time that we have support,
        // call the optimized code directly.
        #[cfg(target_feature = "sse4.2")]
        {
            unsafe { self.fast.find(haystack) }
        }

        // If we can tell at compile time that we will *never* have
        // support, call the fallback directly.
        #[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
        {
            self.fallback.find(haystack)
        }

        // Otherwise, we will be run on a machine with or without
        // support, so we perform runtime detection.
        #[cfg(all(any(target_arch = "x86", target_arch = "x86_64"),
                  not(target_feature = "sse4.2")))]
        {
            if is_x86_feature_detected!("sse4.2") {
                unsafe { self.fast.find(haystack) }
            } else {
                self.fallback.find(haystack)
            }
        }
    }
}

pub type BytesConst = Bytes<fn(u8) -> bool>;

pub struct AsciiChars<F>(Bytes<F>)
where
    F: Fn(u8) -> bool;

impl<F> AsciiChars<F>
where
    F: Fn(u8) -> bool,
{
    pub fn new(bytes: [u8; 16], len: i32, fallback: F) -> Self {
        for &b in &bytes {
            assert!(b < 128, "Cannot have non-ASCII bytes");
        }
        AsciiChars(Bytes::new(bytes, len, fallback))
    }

    #[inline]
    pub fn find(&self, haystack: &str) -> Option<usize> {
        self.0.find(haystack.as_bytes())
    }
}

pub type AsciiCharsConst = AsciiChars<fn(u8) -> bool>;

#[cfg(all(test, feature = "benchmarks"))]
mod bench {
    extern crate test;

    use super::*;

    lazy_static! {
        static ref SPACE: AsciiCharsConst = ascii_chars2!(b' ');
        static ref XML_DELIM_3: AsciiCharsConst = ascii_chars2!(b'<', b'>', b'&');
        static ref XML_DELIM_5: AsciiCharsConst = ascii_chars2!(b'<', b'>', b'&', b'\'', b'"');
    }

    fn prefix_string() -> String {
        "a".repeat(5 * 1024 * 1024)
    }

    fn bench_space<F>(b: &mut test::Bencher, f: F)
    where
        F: Fn(&str) -> Option<usize>,
    {
        let mut haystack = prefix_string();
        haystack.push(' ');

        b.iter(|| test::black_box(f(&haystack)));
        b.bytes = haystack.len() as u64;
    }

    #[bench]
    fn space_ascii_chars(b: &mut test::Bencher) {
        bench_space(b, |hs| SPACE.find(hs))
    }

    #[bench]
    fn space_stdlib_find_string(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.find(" "))
    }

    #[bench]
    fn space_stdlib_find_char(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.find(' '))
    }

    #[bench]
    fn space_stdlib_find_char_set(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.find(&[' '][..]))
    }

    #[bench]
    fn space_stdlib_find_closure(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.find(|c| c == ' '))
    }

    #[bench]
    fn space_stdlib_iterator_position(b: &mut test::Bencher) {
        bench_space(b, |hs| hs.as_bytes().iter().position(|&v| v == b' '))
    }

    fn bench_xml_delim_3<F>(b: &mut test::Bencher, f: F)
    where
        F: Fn(&str) -> Option<usize>,
    {
        let mut haystack = prefix_string();
        haystack.push('&');

        b.iter(|| test::black_box(f(&haystack)));
        b.bytes = haystack.len() as u64;
    }

    #[bench]
    fn xml_delim_3_ascii_chars(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| XML_DELIM_3.find(hs))
    }

    #[bench]
    fn xml_delim_3_stdlib_find_char_set(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| hs.find(&['<', '>', '&'][..]))
    }

    #[bench]
    fn xml_delim_3_stdlib_find_char_closure(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| hs.find(|c| c == '<' || c == '>' || c == '&'))
    }

    #[bench]
    fn xml_delim_3_stdlib_iterator_position(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| {
            hs.as_bytes()
                .iter()
                .position(|&c| c == b'<' || c == b'>' || c == b'&')
        })
    }

    fn bench_xml_delim_5<F>(b: &mut test::Bencher, f: F)
    where
        F: Fn(&str) -> Option<usize>,
    {
        let mut haystack = prefix_string();
        haystack.push('"');

        b.iter(|| test::black_box(f(&haystack)));
        b.bytes = haystack.len() as u64;
    }

    #[bench]
    fn xml_delim_5_ascii_chars(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| XML_DELIM_5.find(hs))
    }

    #[bench]
    fn xml_delim_5_stdlib_find_char_set(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| hs.find(&['<', '>', '&', '\'', '"'][..]))
    }

    #[bench]
    fn xml_delim_5_stdlib_find_char_closure(b: &mut test::Bencher) {
        bench_xml_delim_5(b, |hs| {
            hs.find(|c| c == '<' || c == '>' || c == '&' || c == '\'' || c == '"')
        })
    }

    #[bench]
    fn xml_delim_5_stdlib_iterator_position(b: &mut test::Bencher) {
        bench_xml_delim_3(b, |hs| {
            hs.as_bytes()
                .iter()
                .position(|&c| c == b'<' || c == b'>' || c == b'&' || c == b'\'' || c == b'"')
        })
    }
}
