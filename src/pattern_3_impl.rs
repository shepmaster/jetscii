use pattern_3::{Pattern, Searcher, Consumer, Haystack, Span};
use std::ops::Range;

// Thoughts
//
// A lot of types that fit together in subtle ways
// searcher and consumer fell redundant
// missing slice of chars impl?

use AsciiChars;

impl<'a, F, H> Pattern<H> for &'a AsciiChars<F>
where
    H: Haystack<Target = str>,
    F: Fn(u8) -> bool, // Repeating from struct bound
{
    type Searcher = Self;
    type Consumer = Self;

    fn into_searcher(self) -> Self::Searcher { self }
    fn into_consumer(self) -> Self::Consumer { self }
}

unsafe impl<'a, F> Searcher<str> for &'a AsciiChars<F>
where
    F: Fn(u8) -> bool, // Repeating from struct bound
{
    fn search(&mut self, span: Span<&str>) -> Option<Range<usize>> {
        let (hay, range) = span.into_parts();
        self.find(&hay[range]).map(|i| i..i+1)
    }
}

unsafe impl<'a, F> Consumer<str> for &'a AsciiChars<F>
where
    F: Fn(u8) -> bool, // Repeating from struct bound
{
    fn consume(&mut self, span: Span<&str>) -> Option<usize> {
        let (hay, range) = span.into_parts();
        self.find(&hay[range][..1]).map(|_| 1)
    }
}

// TODO: substring

#[cfg(test)]
mod test {
    use proptest::{self, collection::vec as vec_strat, prelude::*};

    use super::*;

    fn ascii_char() -> BoxedStrategy<char> {
        // This is inclusive
        proptest::char::range(0 as char, 127 as char).boxed()
    }

    proptest! {
        #[test]
        fn works_as_find_does_for_up_to_and_including_16_characters(
            (haystack, needle_raw) in (any::<String>(), vec_strat(ascii_char(), 0..=16))
        ) {
            let mut bytes = [b'\0'; 16];
            for (&c, b) in needle_raw.iter().zip(&mut bytes) {
                *b = c as u8;
            }

            let us = AsciiChars::new(bytes, needle_raw.len() as i32, |b| {
                needle_raw.iter().any(|&c| c as u8 == b)
            });

            let them = &needle_raw[..];

            use ::pattern_3::ext::find;
            assert_eq!(find(haystack.as_str(), &us), find(haystack.as_str(), them));
        }
    }
}
