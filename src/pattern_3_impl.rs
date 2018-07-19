use pattern_3::{Pattern, Searcher, Consumer, Haystack, Span};
use std::ops::Range;

// Thoughts
//
// - A lot of types that fit together in subtle ways
// - `Searcher` and `Consumer` feel redundant based on their descriptions
// - Unclear what `Searcher` and `Consumer` map to in function-space, in order to test them.
// - I may not be thinking of my pattern in terms of what Searcher / Consumer offer,
//   so implementing one feels non-performant

// Failures:
//
// - Didn't properly return the offset in the hay (`range.start`), but only in the haystack
// - `consume` was **completely** broken, but was not tested at all (see thoughts)

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
        let (field, range) = span.into_parts();
        let start = range.start;
        let haystack = &field[range];
        self.find(haystack).map(|i| i + start).map(|i| i..i+1)
    }
}

unsafe impl<'a, F> Consumer<str> for &'a AsciiChars<F>
where
    F: Fn(u8) -> bool, // Repeating from struct bound
{
    fn consume(&mut self, span: Span<&str>) -> Option<usize> {
        let (field, range) = span.into_parts();
        let start = range.start;
        let haystack = &field[range];

        haystack.chars().next()
            .and_then(|c| {
                if c.len_utf8() == 1 {
                    // We only support ASCII characters, which are a single byte
                    self.find(&haystack[..1])
                } else {
                    None
                }
            })
            .map(|_| start + 1)
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

            let haystack = haystack.as_str();

            use ::pattern_3::ext::find;
            assert_eq!(find(haystack, &us), find(haystack, them));

            use ::pattern_3::ext::matches;
            assert!(matches(haystack, &us).eq(matches(haystack, them)));

            use ::pattern_3::ext::trim_start;
            assert_eq!(trim_start(haystack, &us), trim_start(haystack, them));

            use ::pattern_3::ext::match_indices;
            assert!(match_indices(haystack, &us).eq(match_indices(haystack, them)));
        }
    }
}
