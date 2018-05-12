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

#[cfg(test)]
mod test {}
