use criterion::{criterion_group, criterion_main, BatchSize, Criterion, Throughput};
use jetscii::{ascii_chars, AsciiCharsConst, SubstringConst};
use std::hint::black_box;
use std::sync::OnceLock;

static SPACE: OnceLock<AsciiCharsConst> = OnceLock::new();

fn space() -> &'static AsciiCharsConst {
    SPACE.get_or_init(|| ascii_chars!(' '))
}

static XML_DELIM_3: OnceLock<AsciiCharsConst> = OnceLock::new();

fn xml_delim_3() -> &'static AsciiCharsConst {
    XML_DELIM_3.get_or_init(|| ascii_chars!('<', '>', '&'))
}

static XML_DELIM_5: OnceLock<AsciiCharsConst> = OnceLock::new();

fn xml_delim_5() -> &'static AsciiCharsConst {
    XML_DELIM_5.get_or_init(|| ascii_chars!('<', '>', '&', '\'', '"'))
}

static BIG_16: OnceLock<AsciiCharsConst> = OnceLock::new();

fn big_16() -> &'static AsciiCharsConst {
    BIG_16.get_or_init(|| {
        ascii_chars!('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P')
    })
}

static SUBSTRING: OnceLock<SubstringConst> = OnceLock::new();

fn substring() -> &'static SubstringConst {
    SUBSTRING.get_or_init(|| SubstringConst::new("xyzzy"))
}

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
        let space = space();
        b.iter(|| space.find(&haystack));
    });
    group.bench_function("teddy", |b| {
        let searcher = aho_corasick::packed::Searcher::new([" "]).unwrap();
        b.iter(|| searcher.find(&haystack).map(|m| m.start()));
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
        let xml_delim_3 = xml_delim_3();
        b.iter(|| xml_delim_3.find(&haystack));
    });
    group.bench_function("stdlib_iter_position", |b| {
        b.iter(|| {
            haystack
                .bytes()
                .position(|c| c == b'<' || c == b'>' || c == b'&')
        });
    });
    group.bench_function("teddy", |b| {
        let searcher = aho_corasick::packed::Searcher::new(["<", ">", "&"]).unwrap();
        b.iter(|| searcher.find(&haystack).map(|m| m.start()));
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
        let xml_delim_5 = xml_delim_5();
        b.iter(|| xml_delim_5.find(&haystack));
    });
    group.bench_function("stdlib_iter_position", |b| {
        b.iter(|| {
            haystack
                .bytes()
                .position(|c| c == b'<' || c == b'>' || c == b'&' || c == b'\'' || c == b'"')
        });
    });
    group.bench_function("teddy", |b| {
        let searcher = aho_corasick::packed::Searcher::new(["<", ">", "&", "'", "\""]).unwrap();
        b.iter(|| searcher.find(&haystack).map(|m| m.start()));
    });
}

fn big_16_benches(c: &mut Criterion) {
    let mut haystack = prefix_string();
    haystack.push('P');
    let haystack = black_box(haystack);

    let mut group = c.benchmark_group("find_big_16");
    group.throughput(Throughput::Bytes(haystack.len() as u64));

    group.bench_function("ascii_chars", |b| {
        let big_16 = big_16();
        b.iter(|| big_16.find(&haystack));
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
    group.bench_function("teddy", |b| {
        let searcher = aho_corasick::packed::Searcher::new(
            b"ABCDEFGHIJKLMNOP".iter().map(|b| std::array::from_ref(b)),
        )
        .unwrap();
        b.iter(|| searcher.find(&haystack).map(|m| m.start()));
    });

    group.finish();

    let mut haystack = prefix_string();
    haystack.insert(0, 'P');
    let haystack = black_box(haystack);
    let mut group = c.benchmark_group("find_big_16_early_return");
    group.throughput(Throughput::Bytes(1));

    group.bench_function("ascii_chars", |b| {
        let big_16 = big_16();
        b.iter(|| big_16.find(&haystack));
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
    group.bench_function("teddy", |b| {
        let searcher = aho_corasick::packed::Searcher::new(
            b"ABCDEFGHIJKLMNOP".iter().map(|b| std::array::from_ref(b)),
        )
        .unwrap();
        b.iter(|| searcher.find(&haystack).map(|m| m.start()));
    });
}

fn substr(c: &mut Criterion) {
    let mut haystack = prefix_string();
    haystack.push_str("xyzzy");
    let haystack = black_box(haystack);

    let mut group = c.benchmark_group("find_substring");
    group.throughput(Throughput::Bytes(haystack.len() as u64));

    group.bench_function("substring", |b| {
        let substring = substring();
        b.iter(|| substring.find(&haystack));
    });
    group.bench_function("stdlib_find_string", |b| {
        b.iter(|| haystack.find("xyzzy"));
    });
    group.bench_function("memchr", |b| {
        let finder = memchr::memmem::Finder::new(b"xyzzy");
        b.iter(|| finder.find(haystack.as_bytes()));
    });
}

fn iterate_xml_many_match(c: &mut Criterion) {
    let haystack = black_box(include_str!("plant_catalog.xml"));
    let mut group = c.benchmark_group("iterate_xml_3");

    group.throughput(Throughput::Bytes(haystack.len() as u64));
    group.bench_function("ascii_chars", |b| {
        let xml_delim_3 = xml_delim_3();
        b.iter_batched(
            || xml_delim_3.as_bytes().iter(haystack.as_bytes()),
            |iter| {
                for offset in iter {
                    black_box(offset);
                }
            },
            BatchSize::SmallInput,
        );
    });
    group.bench_function("stdlib_iter_position", |b| {
        b.iter(|| {
            let mut haystack = &haystack[..];
            let mut offset = 0;
            while let Some(pos) = haystack
                .bytes()
                .position(|c| c == b'<' || c == b'>' || c == b'&')
            {
                haystack = &haystack[pos + 1..];
                offset += pos;
                black_box(offset);
            }
        });
    });
    group.bench_function("memchr", |b| {
        b.iter_batched(
            || memchr::memchr3_iter(b'<', b'>', b'&', haystack.as_bytes()),
            |iter| {
                for offset in iter {
                    black_box(offset);
                }
            },
            BatchSize::SmallInput,
        );
    });
    group.finish();
}

fn iterate_few_match(c: &mut Criterion) {
    let haystack = black_box(include_str!("plant_catalog.xml"));
    let mut group = c.benchmark_group("iterate_few_matches");
    let chars: AsciiCharsConst = ascii_chars!(b'?', b'-', b'\0');

    group.throughput(Throughput::Bytes(haystack.len() as u64));
    group.bench_function("ascii_chars", |b| {
        b.iter(|| {
            let mut haystack = &haystack[..];
            let mut offset = 0;
            while let Some(pos) = chars.find(haystack) {
                haystack = &haystack[pos + 1..];
                offset += pos;
                black_box(offset);
            }
        });
    });
    group.bench_function("stdlib_iter_position", |b| {
        b.iter(|| {
            let mut haystack = &haystack[..];
            let mut offset = 0;
            while let Some(pos) = haystack
                .bytes()
                .position(|c| c == b'?' || c == b'-' || c == b'\0')
            {
                haystack = &haystack[pos + 1..];
                offset += pos;
                black_box(offset);
            }
        });
    });
    group.bench_function("memchr", |b| {
        b.iter_batched(
            || memchr::memchr3_iter(b'?', b'-', b'\0', haystack.as_bytes()),
            |iter| {
                for offset in iter {
                    black_box(offset);
                }
            },
            BatchSize::SmallInput,
        );
    });
    group.finish();
}

criterion_group!(
    benches,
    spaces,
    xml3,
    xml5,
    big_16_benches,
    substr,
    iterate_xml_many_match,
    iterate_few_match,
);
criterion_main!(benches);
