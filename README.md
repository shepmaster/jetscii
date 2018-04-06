# Jetscii

A tiny library to efficiently search strings for substrings or sets of
ASCII characters.

[![Build Status](https://travis-ci.org/shepmaster/jetscii.svg)](https://travis-ci.org/shepmaster/jetscii) [![Current Version](http://meritbadge.herokuapp.com/jetscii)](https://crates.io/crates/jetscii)

[Documentation](https://shepmaster.github.io/jetscii/)

## Examples

### Searching for a set of ASCII characters

```rust
#[macro_use]
extern crate jetscii;

fn main() {
    let part_number = "86-J52:rev1";
    let parts: Vec<_> = part_number.split(ascii_chars!('-', ':')).collect();
    assert_eq!(&parts, &["86", "J52", "rev1"]);
}
```

### Searching for a substring

```rust
use jetscii::Substring;
let colors: Vec<_> = "red, blue, green".split(Substring::new(", ")).collect();
assert_eq!(&colors, &["red", "blue", "green"]);
```

## What's so special about this library?

We use a particular set of x86-64 SSE 4.2 instructions (`PCMPESTRI`
and `PCMPESTRM`) to gain great speedups. This method stays fast even
when searching for a character in a set of up to 16 choices.

When the `PCMPxSTRx` instructions are not available, we fall back to
reasonably fast but universally-supported methods.

## Benchmarks

### Single character

Searching a 5MiB string of `a`s with a single space at the end:

| Method                                             | Speed     |
|----------------------------------------------------|-----------|
| **`str.find(AsciiChars)`**                         | 5719 MB/s |
| `str.as_bytes().iter().position(\|&v\| v == b' ')` | 1620 MB/s |
| `str.find(\|c\| c == ' ')`                         | 1090 MB/s |
| `str.find(' ')`                                    | 1085 MB/s |
| `str.find(&[' '][..])`                             |  602 MB/s |
| `str.find(" ")`                                    |  293 MB/s |

### Set of characters

Searching a 5MiB string of `a`s with a single ampersand at the end:

| Method                                           | Speed     |
|--------------------------------------------------|-----------|
| **`str.find(AsciiChars)`**                       | 5688 MB/s |
| `str.as_bytes().iter().position(\|&v\| ...)`     | 1620 MB/s |
| `str.find(\|c\| ...)`                            | 1022 MB/s |
| `str.find(&['<', '>', '&'][..])`                 |  361 MB/s |

### Substrings

Searching a 5MiB string of `a`s with the string `xyzzy` at the end:

| Method                                           | Speed     |
|--------------------------------------------------|-----------|
| **`str.find(Substring::new("xyzzy"))`**          | 5017 MB/s |
| str.find("xyzzy")                                | 3837 MB/s |

## Contributing

1. Fork it (https://github.com/shepmaster/jetscii/fork)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Add a failing test.
4. Add code to pass the test.
5. Commit your changes (`git commit -am 'Add some feature'`)
6. Ensure tests pass.
7. Push to the branch (`git push origin my-new-feature`)
8. Create a new Pull Request
