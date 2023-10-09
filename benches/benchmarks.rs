use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use jetscii::{ascii_chars, AsciiCharsConst, SubstringConst};
use std::hint::black_box;

static SPACE: AsciiCharsConst = ascii_chars!(' ');
static XML_DELIM_3: AsciiCharsConst = ascii_chars!('<', '>', '&');
static XML_DELIM_5: AsciiCharsConst = ascii_chars!('<', '>', '&', '\'', '"');
static BIG_16: AsciiCharsConst =
    ascii_chars!('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P');

static SUBSTRING: SubstringConst = SubstringConst::new("xyzzy");

fn prefix_string() -> String {
    "a".repeat(5 * 1024 * 1024)
}

fn spaces(c: &mut Criterion) {
    let mut haystack = prefix_string();
    haystack.push(' ');
    let haystack = black_box(haystack);

    let mut group = c.benchmark_group("find_last_space");
    group.throughput(Throughput::Bytes(haystack.len() as u64));

    group.bench_function("ascii_chars", |b| {
        b.iter(|| SPACE.find(&haystack));
    });
    group.bench_function("stdlib_find_string", |b| {
        b.iter(|| haystack.find(" "));
    });
    group.bench_function("stdlib_find_char", |b| {
        b.iter(|| haystack.find(' '));
    });
    group.bench_function("stdlib_find_char_set", |b| {
        b.iter(|| haystack.find(&[' '][..]));
    });
    group.bench_function("stdlib_find_closure", |b| {
        b.iter(|| haystack.find(|c| c == ' '));
    });
    group.bench_function("stdlib_iter_position", |b| {
        b.iter(|| haystack.bytes().position(|c| c == b' '));
    });
    group.bench_function("memchr", |b| {
        b.iter(|| memchr::memchr(b' ', haystack.as_bytes()));
    });
}

fn xml3(c: &mut Criterion) {
    let mut haystack = prefix_string();
    haystack.push('&');
    let haystack = black_box(haystack);

    let mut group = c.benchmark_group("find_xml_3");
    group.throughput(Throughput::Bytes(haystack.len() as u64));

    group.bench_function("ascii_chars", |b| {
        b.iter(|| XML_DELIM_3.find(&haystack));
    });
    group.bench_function("stdlib_find_char_set", |b| {
        b.iter(|| haystack.find(&['<', '>', '&'][..]));
    });
    group.bench_function("stdlib_find_closure", |b| {
        b.iter(|| haystack.find(|c| c == '<' || c == '>' || c == '&'));
    });
    group.bench_function("stdlib_iter_position", |b| {
        b.iter(|| {
            haystack
                .bytes()
                .position(|c| c == b'<' || c == b'>' || c == b'&')
        });
    });
    group.bench_function("memchr", |b| {
        b.iter(|| memchr::memchr3(b'<', b'>', b'&', haystack.as_bytes()));
    });
}

fn xml5(c: &mut Criterion) {
    let mut haystack = prefix_string();
    haystack.push('"');
    let haystack = black_box(haystack);

    let mut group = c.benchmark_group("find_xml_5");
    group.throughput(Throughput::Bytes(haystack.len() as u64));

    group.bench_function("ascii_chars", |b| {
        b.iter(|| XML_DELIM_5.find(&haystack));
    });
    group.bench_function("stdlib_find_char_set", |b| {
        b.iter(|| haystack.find(&['<', '>', '&', '\'', '"'][..]));
    });
    group.bench_function("stdlib_find_closure", |b| {
        b.iter(|| haystack.find(|c| c == '<' || c == '>' || c == '&' || c == '\'' || c == '"'));
    });
    group.bench_function("stdlib_iter_position", |b| {
        b.iter(|| {
            haystack
                .bytes()
                .position(|c| c == b'<' || c == b'>' || c == b'&' || c == b'\'' || c == b'"')
        });
    });
    group.bench_function("memchr", |b| {
        b.iter(|| {
            let bytes = haystack.as_bytes();
            let indexes = [
                memchr::memchr3(b'<', b'>', b'&', bytes),
                memchr::memchr2(b'\'', b'"', bytes),
            ];
            indexes.iter().copied().flatten().min()
        });
    });
}

fn big_16(c: &mut Criterion) {
    let mut haystack = prefix_string();
    haystack.push('P');
    let haystack = black_box(haystack);

    let mut group = c.benchmark_group("find_big_16");
    group.throughput(Throughput::Bytes(haystack.len() as u64));

    group.bench_function("ascii_chars", |b| {
        b.iter(|| BIG_16.find(&haystack));
    });
    group.bench_function("stdlib_find_char_set", |b| {
        b.iter(|| {
            haystack.find(
                &[
                    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
                ][..],
            )
        });
    });
    group.bench_function("stdlib_find_closure", |b| {
        b.iter(|| {
            haystack.find(|c| {
                c == 'A'
                    || c == 'B'
                    || c == 'C'
                    || c == 'D'
                    || c == 'E'
                    || c == 'F'
                    || c == 'G'
                    || c == 'H'
                    || c == 'I'
                    || c == 'J'
                    || c == 'K'
                    || c == 'L'
                    || c == 'M'
                    || c == 'N'
                    || c == 'O'
                    || c == 'P'
            })
        });
    });
    group.bench_function("stdlib_iter_position", |b| {
        b.iter(|| {
            haystack.bytes().position(|c| {
                c == b'A'
                    || c == b'B'
                    || c == b'C'
                    || c == b'D'
                    || c == b'E'
                    || c == b'F'
                    || c == b'G'
                    || c == b'H'
                    || c == b'I'
                    || c == b'J'
                    || c == b'K'
                    || c == b'L'
                    || c == b'M'
                    || c == b'N'
                    || c == b'O'
                    || c == b'P'
            })
        });
    });
    group.bench_function("memchr", |b| {
        b.iter(|| {
            let bytes = haystack.as_bytes();
            let indexes = [
                memchr::memchr3(b'A', b'B', b'C', bytes),
                memchr::memchr3(b'D', b'E', b'F', bytes),
                memchr::memchr3(b'G', b'H', b'I', bytes),
                memchr::memchr3(b'J', b'K', b'L', bytes),
                memchr::memchr3(b'M', b'N', b'O', bytes),
                memchr::memchr(b'P', bytes),
            ];
            indexes.iter().copied().flatten().min()
        })
    });
}

fn substr(c: &mut Criterion) {
    let mut haystack = prefix_string();
    haystack.push_str("xyzzy");
    let haystack = black_box(haystack);

    let mut group = c.benchmark_group("find_substring");
    group.throughput(Throughput::Bytes(haystack.len() as u64));

    group.bench_function("substring", |b| {
        b.iter(|| SUBSTRING.find(&haystack));
    });
    group.bench_function("stdlib_find_string", |b| {
        b.iter(|| haystack.find("xyzzy"));
    });
    group.bench_function("memchr", |b| {
        let finder = memchr::memmem::Finder::new(b"xyzzy");
        b.iter(|| finder.find(haystack.as_bytes()));
    });
}

criterion_group!(benches, spaces, xml3, xml5, big_16, substr);
criterion_main!(benches);
