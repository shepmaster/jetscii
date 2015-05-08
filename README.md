# Jetscii

A tiny library to efficiently search strings for ASCII characters.

[![Build Status](https://travis-ci.org/shepmaster/jetscii.svg)](https://travis-ci.org/shepmaster/jetscii) [![Current Version](http://meritbadge.herokuapp.com/jetscii)](https://crates.io/crates/jetscii)

[Documentation](https://shepmaster.github.io/jetscii/)

## Example

```rust
use jetscii::AsciiChars;
let mut search = AsciiChars::new();
search.push(b'-');
search.push(b':');
let part_number = "86-J52:rev1";
let parts: Vec<_> = part_number.split(search).collect();
assert_eq!(&parts, &["86", "J52", "rev1"]);
```

## What's so special about this library?

We use a particular x86-64 SSE 4.2 instruction (`PCMPESTRI`) to gain
great speedups. This method stays fast even when searching for one
character in a set of up to 8 choices.

When `PCMPESTRI` is not available, we fall back to a
universally-supported byte iterator method.

## Benchmarks

### Single character

Searching a 5MiB string of `a`s with a single space at the end:

| Method                                           | Speed     |
|--------------------------------------------------|-----------|
| **`str.find(AsciiChars)`**                       | 6501 MB/s |
| `str.as_bytes().iter().position(|&v| v == b' ')` | 1620 MB/s |
| `str.find(|c| c == ' ')`                         | 1090 MB/s |
| `str.find(' ')`                                  | 1085 MB/s |
| `str.find(&[' '][..])`                           |  602 MB/s |
| `str.find(" ")`                                  |  293 MB/s |

### Set of characters

Searching a 5MiB string of `a`s with a single ampersand at the end:

| Method                                           | Speed     |
|--------------------------------------------------|-----------|
| **`str.find(AsciiChars)`**                       | 6480 MB/s |
| `str.as_bytes().iter().position(|&v| ...)`       | 1620 MB/s |
| `str.find(|c| ...)`                              | 1022 MB/s |
| `str.find(&['<', '>', '&'][..])`                 |  361 MB/s |

## Contributing

1. Fork it ( https://github.com/shepmaster/jetscii/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Add a failing test.
4. Add code to pass the test.
5. Commit your changes (`git commit -am 'Add some feature'`)
6. Ensure tests pass.
7. Push to the branch (`git push origin my-new-feature`)
8. Create a new Pull Request
