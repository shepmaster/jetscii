#![cfg_attr(feature = "pattern", feature(pattern))]

//! A tiny library to efficiently search strings for sets of ASCII
//! characters or byte slices for sets of bytes.
//!
//! ## Examples
//!
//! ### Searching for a set of ASCII characters
//!
//! ```rust
//! use jetscii::ascii_chars;
//!
//! fn main() {
//!     let part_number = "86-J52:rev1";
//!     let first = ascii_chars!('-', ':').find(part_number);
//!     assert_eq!(first, Some(2));
//! }
//! ```
//!
//! ### Searching for a set of bytes
//!
//! ```rust
//! use jetscii::bytes;
//!
//! fn main() {
//!     let raw_data = [0x00, 0x01, 0x10, 0xFF, 0x42];
//!     let first = bytes!(0x01, 0x10).find(&raw_data);
//!     assert_eq!(first, Some(1));
//! }
//! ```
//!
//! ### Searching for a substring
//!
//! ```
//! use jetscii::Substring;
//!
//! let colors = "red, blue, green";
//! let first = Substring::new(", ").find(colors);
//! assert_eq!(first, Some(3));
//! ```
//!
//! ### Searching for a subslice
//!
//! ```
//! use jetscii::ByteSubstring;
//!
//! let raw_data = [0x00, 0x01, 0x10, 0xFF, 0x42];
//! let first = ByteSubstring::new(&[0x10, 0xFF]).find(&raw_data);
//! assert_eq!(first, Some(2));
//! ```
//!
//! ## Using the pattern API
//!
//! If this crate is compiled with the unstable `pattern` feature
//! flag, [`AsciiChars`] will implement the
//! [`Pattern`][std::str::pattern::Pattern] trait, allowing it to be
//! used with many traditional methods.
//!
//! ```
//! # #[cfg(feature = "pattern")]
//! use jetscii::ascii_chars;
//!
//! fn main() {
//! # #[cfg(feature = "pattern")] {
//!     let part_number = "86-J52:rev1";
//!     let parts: Vec<_> = part_number.split(ascii_chars!('-', ':')).collect();
//!     assert_eq!(&parts, &["86", "J52", "rev1"]);
//! # }
//! }
//! ```
//!
//! ```
//! # #[cfg(feature = "pattern")] {
//! use jetscii::Substring;
//!
//! let colors = "red, blue, green";
//! let colors: Vec<_> = colors.split(Substring::new(", ")).collect();
//! assert_eq!(&colors, &["red", "blue", "green"]);
//! # }
//! ```
//!
//! ## What's so special about this library?
//!
//! We use a particular set of SSE 4.2 instructions (`PCMPESTRI`
//! and `PCMPESTRM`) to gain great speedups. This method stays fast even
//! when searching for a byte in a set of up to 16 choices.
//!
//! When the `PCMPxSTRx` instructions are not available, we fall back to
//! reasonably fast but universally-supported methods.
//!
//! ## Benchmarks
//!
//! These numbers come from running on my personal laptop; always
//! benchmark with data and machines similar to your own.
//!
//! ### Single character
//!
//! Searching a 5MiB string of `a`s with a single space at the end for a space:
//!
//! | Method                                                      | Speed          |
//! |-------------------------------------------------------------|----------------|
//! | <code>ascii_chars!(' ').find(s)</code>                      | 11504 MB/s     |
//! | <code>s.as_bytes().iter().position(\|&c\| c == b' ')</code> | 2377 MB/s      |
//! | <code>s.find(" ")</code>                                    | 2149 MB/s      |
//! | <code>s.find(&[' '][..])</code>                             | 1151 MB/s      |
//! | **<code>s.find(' ')</code>**                                | **14600 MB/s** |
//! | <code>s.find(\|c\| c == ' ')</code>                         | 1080 MB/s      |
//!
//! ### Set of 3 characters
//!
//! Searching a 5MiB string of `a`s with a single ampersand at the end for `<`, `>`, and `&`:
//!
//! | Method                                                      | Speed          |
//! |-------------------------------------------------------------|----------------|
//! | **<code>ascii_chars!(/\* ... \*/).find(s)</code>**          | **11513 MB/s** |
//! | <code>s.as_bytes().iter().position(\|&c\| /* ... */)</code> | 1644 MB/s      |
//! | <code>s.find(&[/* ... */][..])</code>                       | 1079 MB/s      |
//! | <code>s.find(\|c\| /* ... */))</code>                       | 1084 MB/s      |
//!
//! ### Set of 5 characters
//!
//! Searching a 5MiB string of `a`s with a single ampersand at the end for `<`, `>`, `&`, `'`, and `"`:
//!
//! | Method                                                      | Speed          |
//! |-------------------------------------------------------------|----------------|
//! | **<code>ascii_chars!(/\* ... \*/).find(s)</code>**          | **11504 MB/s** |
//! | <code>s.as_bytes().iter().position(\|&c\| /* ... */)</code> | 812 MB/s       |
//! | <code>s.find(&[/* ... */][..]))</code>                      | 538 MB/s       |
//! | <code>s.find(\|c\| /* ... */)</code>                        | 1082 MB/s      |
//!
//! ### Substring
//!
//! Searching a 5MiB string of `a`s with the string "xyzzy" at the end for "xyzzy":
//!
//! | Method                                           | Speed          |
//! |--------------------------------------------------|----------------|
//! | **<code>Substring::new("xyzzy").find(s)</code>** | **11475 MB/s** |
//! | <code>s.find("xyzzy")</code>                     | 5391 MB/s      |

use std::marker::PhantomData;

include!(concat!(env!("OUT_DIR"), "/src/macros.rs"));

mod simd;

// This module may not be used if e.g. we statically know we have the sse4.2 target feature
#[allow(unused)]
mod fallback;

#[cfg(feature = "pattern")]
mod pattern;

macro_rules! dispatch {
    (x86: $x86:expr, aarch64: $aarch64:expr, fallback: $fallback:expr,) => ({
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        {
            dispatch!(
                target_feature: "sse4.2",
                is_feature_enabled: is_x86_feature_detected,
                simd: $x86,
                fallback: $fallback,
            )
        }

        #[cfg(any(target_arch = "aarch64", target_arch = "arm64ec"))]
        {
            dispatch!(
                target_feature: "neon",
                is_feature_enabled: is_aarch64_feature_detected,
                simd: $aarch64,
                fallback: $fallback,
            )
        }

        // If we can tell at compile time that we will *never* have
        // support, call the fallback directly.
        #[cfg(not(any(target_arch = "x86", target_arch = "x86_64", target_arch = "aarch64", target_arch = "arm64ec")))]
        {
            $fallback
        }
    });
    (target_feature: $target_feature:tt, is_feature_enabled: $is_feature_enabled:ident, simd: $simd:expr, fallback: $fallback:expr,) => ({
            // If we can tell at compile time that we have support,
            // call the optimized code directly.
            #[cfg(target_feature = $target_feature)]
            {
                $simd
            }
            // Otherwise, we will be run on a machine with or without
            // support, so we perform runtime detection.
            #[cfg(not(target_feature = $target_feature))]
            {
                if std::arch::$is_feature_enabled!($target_feature) {
                    $simd
                } else {
                    $fallback
                }
            }
    });
}

/// Searches a slice for a set of bytes. Up to 16 bytes may be used.
pub struct Bytes<F> {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    x86: simd::x86::Bytes,

    #[cfg(any(target_arch = "aarch64", target_arch = "arm64ec"))]
    aarch64: simd::aarch64::Bytes,

    #[cfg(not(any(
        all(
            any(target_arch = "x86", target_arch = "x86_64"),
            target_feature = "sse4.2"
        ),
        all(
            any(target_arch = "aarch64", target_arch = "arm64ec"),
            target_feature = "neon"
        ),
    )))]
    fallback: fallback::Bytes<F>,

    // Since we might not use the fallback implementation, we add this
    // to avoid unused type parameters.
    _fallback: PhantomData<F>,
}

impl<F> Bytes<F>
where
    F: Fn(u8) -> bool,
{
    /// Manual constructor; prefer using [`bytes!`] instead.
    ///
    /// Provide an array of bytes to search for, the number of
    /// valid bytes provided, and a closure to use when the SIMD
    /// intrinsics are not available. The closure **must** search for
    /// the same bytes as in the array.
    #[allow(unused_variables)]
    #[must_use]
    pub fn new(bytes: [u8; 16], len: i32, fallback: F) -> Self {
        Bytes {
            #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
            x86: simd::x86::Bytes::new(bytes, len),

            #[cfg(any(target_arch = "aarch64", target_arch = "arm64ec"))]
            aarch64: simd::aarch64::Bytes::new(bytes, len),

            #[cfg(not(any(
                all(
                    any(target_arch = "x86", target_arch = "x86_64"),
                    target_feature = "sse4.2"
                ),
                all(
                    any(target_arch = "aarch64", target_arch = "arm64ec"),
                    target_feature = "neon"
                ),
            )))]
            fallback: fallback::Bytes::new(fallback),

            _fallback: PhantomData,
        }
    }

    /// Searches the slice for the first matching byte in the set.
    #[inline]
    #[must_use]
    pub fn find(&self, haystack: &[u8]) -> Option<usize> {
        dispatch! {
            x86: unsafe { self.x86.find(haystack) },
            aarch64: unsafe { self.aarch64.find(haystack) },
            fallback: self.fallback.find(haystack),
        }
    }

    pub fn iter<'a>(&'a self, haystack: &'a [u8]) -> BytesIter<'a, F> {
        dispatch! {
            x86: BytesIter::X86(self.x86.iter(haystack)),
            aarch64: BytesIter::Aarch64(self.aarch64.iter(haystack)),
            fallback: BytesIter::Fallback(self.fallback.iter(haystack)),
        }
    }
}

/// A convenience type that can be used in a constant or static.
pub type BytesConst = Bytes<fn(u8) -> bool>;

pub enum BytesIter<'a, F> {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    X86(simd::x86::BytesIter<'a>),

    #[cfg(any(target_arch = "aarch64", target_arch = "arm64ec"))]
    Aarch64(simd::aarch64::BytesIter<'a>),

    #[cfg(not(any(
        all(
            any(target_arch = "x86", target_arch = "x86_64"),
            target_feature = "sse4.2"
        ),
        all(
            any(target_arch = "aarch64", target_arch = "arm64ec"),
            target_feature = "neon"
        ),
    )))]
    Fallback(fallback::BytesIter<'a, F>),
    // Since we might not use the fallback implementation, we add this
    // to avoid unused type parameters.
    // This type is impossible to construct, but we still use the F parameter
    _Impossible(std::convert::Infallible, PhantomData<F>),
}

impl<'a, F> Iterator for BytesIter<'a, F>
where
    F: Fn(u8) -> bool,
{
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
            Self::X86(iter) => iter.next(),

            #[cfg(any(target_arch = "aarch64", target_arch = "arm64ec"))]
            Self::Aarch64(iter) => iter.next(),

            #[cfg(not(any(
                all(
                    any(target_arch = "x86", target_arch = "x86_64"),
                    target_feature = "sse4.2"
                ),
                all(
                    any(target_arch = "aarch64", target_arch = "arm64ec"),
                    target_feature = "neon"
                ),
            )))]
            Self::Fallback(iter) => iter.next(),
            // Since we might not use the fallback implementation, we add this
            // to avoid unused type parameters.
            // This type is impossible to construct, but we still use the F parameter
            &mut Self::_Impossible(infailable, _) => match infailable {},
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
            Self::X86(iter) => iter.size_hint(),

            #[cfg(any(target_arch = "aarch64", target_arch = "arm64ec"))]
            Self::Aarch64(iter) => iter.size_hint(),

            #[cfg(not(any(
                all(
                    any(target_arch = "x86", target_arch = "x86_64"),
                    target_feature = "sse4.2"
                ),
                all(
                    any(target_arch = "aarch64", target_arch = "arm64ec"),
                    target_feature = "neon"
                ),
            )))]
            Self::Fallback(iter) => iter.size_hint(),
            // Since we might not use the fallback implementation, we add this
            // to avoid unused type parameters.
            // This type is impossible to construct, but we still use the F parameter
            &Self::_Impossible(infailable, _) => match infailable {},
        }
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        match self {
            #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
            Self::X86(iter) => iter.count(),

            #[cfg(any(target_arch = "aarch64", target_arch = "arm64ec"))]
            Self::Aarch64(iter) => iter.count(),

            #[cfg(not(any(
                all(
                    any(target_arch = "x86", target_arch = "x86_64"),
                    target_feature = "sse4.2"
                ),
                all(
                    any(target_arch = "aarch64", target_arch = "arm64ec"),
                    target_feature = "neon"
                ),
            )))]
            Self::Fallback(iter) => iter.count(),
            // Since we might not use the fallback implementation, we add this
            // to avoid unused type parameters.
            // This type is impossible to construct, but we still use the F parameter
            Self::_Impossible(infailable, _) => match infailable {},
        }
    }
}

/// Searches a string for a set of ASCII characters. Up to 16
/// characters may be used.
pub struct AsciiChars<F>(Bytes<F>);

impl<F> AsciiChars<F>
where
    F: Fn(u8) -> bool,
{
    /// Manual constructor; prefer using [`ascii_chars!`] instead.
    ///
    /// Provide an array of ASCII bytes to search for, the number of
    /// valid bytes provided, and a closure to use when the SIMD
    /// intrinsics are not available. The closure **must** search for
    /// the same characters as in the array.
    ///
    /// ### Panics
    ///
    /// - If you provide a non-ASCII byte.
    #[must_use]
    pub fn new(chars: [u8; 16], len: i32, fallback: F) -> Self {
        for &b in &chars {
            assert!(b < 128, "Cannot have non-ASCII bytes");
        }
        Self(Bytes::new(chars, len, fallback))
    }

    /// Searches the string for the first matching ASCII byte in the set.
    #[inline]
    #[must_use]
    pub fn find(&self, haystack: &str) -> Option<usize> {
        self.0.find(haystack.as_bytes())
    }

    /// Return an iterator over the indices of the specified characters in the haystack.
    pub fn iter<'a>(&'a self, haystack: &'a str) -> AsciiCharsIter<'a, F> {
        AsciiCharsIter(self.0.iter(haystack.as_bytes()))
    }

    /// Get a [`Bytes`] reference with the same ascii characters
    pub fn as_bytes(&self) -> &Bytes<F> {
        &self.0
    }
}

/// A convenience type that can be used in a constant or static.
pub type AsciiCharsConst = AsciiChars<fn(u8) -> bool>;

pub struct AsciiCharsIter<'a, F>(BytesIter<'a, F>);

impl<F> Iterator for AsciiCharsIter<'_, F>
where
    F: Fn(u8) -> bool,
{
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }

    fn count(self) -> usize {
        self.0.count()
    }
}

/// Searches a slice for the first occurence of the subslice.
pub struct ByteSubstring<'a> {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    x86: simd::x86::ByteSubstring<'a>,

    #[cfg(not(
        any(
            all(any(target_arch = "x86", target_arch = "x86_64"), target_feature = "sse4.2"),
            // No aarch64 simd implementation of substring search
            // all(any(target_arch = "aarch64", target_arch = "arm64ec"), target_feature = "neon"),
        )
    ))]
    fallback: fallback::ByteSubstring<'a>,
}

impl<'a> ByteSubstring<'a> {
    #[must_use]
    pub fn new(needle: &'a [u8]) -> Self {
        Self {
            #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
            x86: simd::x86::ByteSubstring::new(needle),

            #[cfg(not(
                any(
                    all(any(target_arch = "x86", target_arch = "x86_64"), target_feature = "sse4.2"),
                    // No aarch64 simd implementation of substring search
                    // all(any(target_arch = "aarch64", target_arch = "arm64ec"), target_feature = "neon"),
                )
            ))]
            fallback: fallback::ByteSubstring::new(needle),
        }
    }

    #[cfg(feature = "pattern")]
    #[must_use]
    fn needle_len(&self) -> usize {
        dispatch! {
            x86: self.x86.needle_len(),
            aarch64: self.fallback.needle_len(),
            fallback: self.fallback.needle_len(),
        }
    }

    /// Searches the slice for the first occurence of the subslice.
    #[inline]
    #[must_use]
    pub fn find(&self, haystack: &[u8]) -> Option<usize> {
        dispatch! {
            x86: unsafe { self.x86.find(haystack) },
            aarch64: self.fallback.find(haystack),
            fallback: self.fallback.find(haystack),
        }
    }
}

/// A convenience type that can be used in a constant or static.
pub type ByteSubstringConst = ByteSubstring<'static>;

/// Searches a string for the first occurence of the substring.
pub struct Substring<'a>(ByteSubstring<'a>);

impl<'a> Substring<'a> {
    #[must_use]
    pub fn new(needle: &'a str) -> Self {
        Substring(ByteSubstring::new(needle.as_bytes()))
    }

    #[cfg(feature = "pattern")]
    #[must_use]
    fn needle_len(&self) -> usize {
        self.0.needle_len()
    }

    /// Searches the string for the first occurence of the substring.
    #[inline]
    #[must_use]
    pub fn find(&self, haystack: &str) -> Option<usize> {
        self.0.find(haystack.as_bytes())
    }
}

/// A convenience type that can be used in a constant or static.
pub type SubstringConst = Substring<'static>;
