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

#[cfg(any(jetscii_sse4_2 = "yes", jetscii_sse4_2 = "maybe"))]
mod simd;

#[cfg(any(jetscii_sse4_2 = "maybe", jetscii_sse4_2 = "no"))]
mod fallback;

#[cfg(feature = "pattern")]
mod pattern;

macro_rules! dispatch {
    (simd: $simd:expr,fallback: $fallback:expr,) => {
        // If we can tell at compile time that we have support,
        // call the optimized code directly.
        #[cfg(jetscii_sse4_2 = "yes")]
        {
            $simd
        }

        // If we can tell at compile time that we will *never* have
        // support, call the fallback directly.
        #[cfg(jetscii_sse4_2 = "no")]
        {
            $fallback
        }

        // Otherwise, we will be run on a machine with or without
        // support, so we perform runtime detection.
        #[cfg(jetscii_sse4_2 = "maybe")]
        {
            if is_x86_feature_detected!("sse4.2") {
                $simd
            } else {
                $fallback
            }
        }
    };
}

/// Searches a slice for a set of bytes. Up to 16 bytes may be used.
pub struct Bytes<F>
where
    F: Fn(u8) -> bool,
{
    #[cfg(any(jetscii_sse4_2 = "yes", jetscii_sse4_2 = "maybe"))]
    simd: simd::Bytes,

    #[cfg(any(jetscii_sse4_2 = "maybe", jetscii_sse4_2 = "no"))]
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
    pub fn new(bytes: [u8; 16], len: i32, fallback: F) -> Self {
        Bytes {
            #[cfg(any(jetscii_sse4_2 = "yes", jetscii_sse4_2 = "maybe"))]
            simd: simd::Bytes::new(bytes, len),

            #[cfg(any(jetscii_sse4_2 = "maybe", jetscii_sse4_2 = "no"))]
            fallback: fallback::Bytes::new(fallback),

            _fallback: PhantomData,
        }
    }

    // This constructor is used by the `bytes!` macro, and shouldn't be relied on directly.
    //
    // To allow F to be dropped in a const context, we require F to be Copy
    // (thus, have no destructor). If we _know_ we're using sse4.2, we don't use
    // the fallback at all, so it will be dropped in this function.
    #[doc(hidden)]
    #[allow(unused_variables)]
    pub const fn new_const(bytes: [u8; 16], len: i32, fallback: F) -> Self
    where
        F: Copy,
    {
        Self {
            #[cfg(any(jetscii_sse4_2 = "yes", jetscii_sse4_2 = "maybe"))]
            simd: simd::Bytes::new(bytes, len),

            #[cfg(any(jetscii_sse4_2 = "maybe", jetscii_sse4_2 = "no"))]
            fallback: fallback::Bytes::new(fallback),

            _fallback: PhantomData,
        }
    }

    /// Searches the slice for the first matching byte in the set.
    #[inline]
    pub fn find(&self, haystack: &[u8]) -> Option<usize> {
        dispatch! {
            simd: unsafe { self.simd.find(haystack) },
            fallback: self.fallback.find(haystack),
        }
    }
}

/// A convenience type that can be used in a constant or static.
pub type BytesConst = Bytes<fn(u8) -> bool>;

/// Searches a string for a set of ASCII characters. Up to 16
/// characters may be used.
pub struct AsciiChars<F>(Bytes<F>)
where
    F: Fn(u8) -> bool;

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
    pub fn new(chars: [u8; 16], len: i32, fallback: F) -> Self {
        for &b in &chars {
            assert!(b < 128, "Cannot have non-ASCII bytes");
        }
        Self(Bytes::new(chars, len, fallback))
    }

    // This constructor is used by the `ascii_chars!` macro, and shouldn't be relied on directly.
    //
    // To allow F to be dropped in a const context, we require F to be Copy
    // (thus, have no destructor). If we _know_ we're using sse4.2, we don't use
    // the fallback at all, so it will be dropped in this function.
    #[doc(hidden)]
    pub const fn new_const(chars: [u8; 16], len: i32, fallback: F) -> Self
    where
        F: Copy,
    {
        let mut i = 0;
        while i < chars.len() {
            assert!(chars[i] < 128, "Cannot have non-ASCII bytes");
            i += 1;
        }
        Self(Bytes::new_const(chars, len, fallback))
    }

    /// Searches the string for the first matching ASCII byte in the set.
    #[inline]
    pub fn find(&self, haystack: &str) -> Option<usize> {
        self.0.find(haystack.as_bytes())
    }
}

/// A convenience type that can be used in a constant or static.
pub type AsciiCharsConst = AsciiChars<fn(u8) -> bool>;

/// Searches a slice for the first occurence of the subslice.
pub struct ByteSubstring<'a> {
    #[cfg(any(jetscii_sse4_2 = "yes", jetscii_sse4_2 = "maybe"))]
    simd: simd::ByteSubstring<'a>,

    #[cfg(any(jetscii_sse4_2 = "maybe", jetscii_sse4_2 = "no"))]
    fallback: fallback::ByteSubstring<'a>,
}

impl<'a> ByteSubstring<'a> {
    pub const fn new(needle: &'a [u8]) -> Self {
        ByteSubstring {
            #[cfg(any(jetscii_sse4_2 = "yes", jetscii_sse4_2 = "maybe"))]
            simd: simd::ByteSubstring::new(needle),

            #[cfg(any(jetscii_sse4_2 = "maybe", jetscii_sse4_2 = "no"))]
            fallback: fallback::ByteSubstring::new(needle),
        }
    }

    #[cfg(feature = "pattern")]
    fn needle_len(&self) -> usize {
        dispatch! {
            simd: self.simd.needle_len(),
            fallback: self.fallback.needle_len(),
        }
    }

    /// Searches the slice for the first occurence of the subslice.
    #[inline]
    pub fn find(&self, haystack: &[u8]) -> Option<usize> {
        dispatch! {
            simd: unsafe { self.simd.find(haystack) },
            fallback: self.fallback.find(haystack),
        }
    }
}

/// A convenience type that can be used in a constant or static.
pub type ByteSubstringConst = ByteSubstring<'static>;

/// Searches a string for the first occurence of the substring.
pub struct Substring<'a>(ByteSubstring<'a>);

impl<'a> Substring<'a> {
    pub const fn new(needle: &'a str) -> Self {
        Substring(ByteSubstring::new(needle.as_bytes()))
    }

    #[cfg(feature = "pattern")]
    fn needle_len(&self) -> usize {
        self.0.needle_len()
    }

    /// Searches the string for the first occurence of the substring.
    #[inline]
    pub fn find(&self, haystack: &str) -> Option<usize> {
        self.0.find(haystack.as_bytes())
    }
}

/// A convenience type that can be used in a constant or static.
pub type SubstringConst = Substring<'static>;
