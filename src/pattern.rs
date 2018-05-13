use std::str::pattern::{Pattern, SearchStep, Searcher};

use super::AsciiChars;

impl<'a, F> Pattern<'a> for AsciiChars<F>
where
    F: Fn(u8) -> bool,
{
    type Searcher = AsciiCharsSearcher<'a, F>;

    fn into_searcher(self, haystack: &'a str) -> Self::Searcher {
        AsciiCharsSearcher {
            haystack: haystack,
            offset: 0,
            searcher: self,
        }
    }
}

pub struct AsciiCharsSearcher<'a, F>
where
    F: Fn(u8) -> bool,
{
    haystack: &'a str,
    offset: usize,
    searcher: AsciiChars<F>,
}

unsafe impl<'a, F> Searcher<'a> for AsciiCharsSearcher<'a, F>
where
    F: Fn(u8) -> bool,
{
    fn haystack(&self) -> &'a str {
        self.haystack
    }

    #[inline]
    fn next(&mut self) -> SearchStep {
        if self.offset >= self.haystack.len() {
            return SearchStep::Done;
        }

        let left_to_search = &self.haystack[self.offset..]; // TODO: unchecked_slice?
        let idx = self.searcher.find(left_to_search);

        // If there's no match, then the rest of the string should be
        // returned.
        let idx = idx.unwrap_or(self.haystack.len());

        let (res, next_offset) = if idx == 0 {
            // A match occurs at the beginning of the string
            let next = self.offset + 1;
            (SearchStep::Match(self.offset, next), next)
        } else {
            // A match occurs somewhere further in the string
            let next = self.offset + idx;
            (SearchStep::Reject(self.offset, next), next)
        };

        self.offset = next_offset;
        res
    }
}

#[cfg(test)]
mod test {
    use quickcheck::{quickcheck, Arbitrary, Gen};
    use std::cmp;

    use super::*;

    #[derive(Debug, Copy, Clone)]
    struct AsciiChar(u8);

    impl Arbitrary for AsciiChar {
        fn arbitrary<G>(g: &mut G) -> AsciiChar
        where
            G: Gen,
        {
            AsciiChar(g.gen_range::<u8>(0, 128))
        }
    }

    #[test]
    fn works_as_find_does_for_single_characters() {
        fn prop(s: String, c: AsciiChar) -> bool {
            let us = ascii_chars!(c.0);
            let them = c.0 as char;
            s.find(us) == s.find(them)
        }
        quickcheck(prop as fn(String, AsciiChar) -> bool);
    }

    #[test]
    fn works_as_find_does_for_multiple_characters() {
        fn prop(s: String, (c1, c2, c3, c4): (AsciiChar, AsciiChar, AsciiChar, AsciiChar)) -> bool {
            let us = ascii_chars!(c1.0, c2.0, c3.0, c4.0);
            let them = &[c1.0 as char, c2.0 as char, c3.0 as char, c4.0 as char][..];
            s.find(us) == s.find(them)
        }
        quickcheck(prop as fn(String, (AsciiChar, AsciiChar, AsciiChar, AsciiChar)) -> bool);
    }

    #[test]
    fn works_as_find_does_for_up_to_16_characters() {
        fn prop(s: String, v: Vec<AsciiChar>) -> bool {
            let n = cmp::min(16, v.len());

            let mut bytes = [b'\0'; 16];
            for (c, c2) in v.iter().take(n).zip(&mut bytes) {
                *c2 = c.0;
            }
            let chars: Vec<_> = v.iter().map(|c| c.0 as char).collect();

            let us = AsciiChars::new(bytes, n as i32, |b| bytes[..n].iter().any(|&c| c == b));

            let them = &chars[..n];

            s.find(us) == s.find(them)
        }
        quickcheck(prop as fn(String, Vec<AsciiChar>) -> bool);
    }

    #[test]
    fn can_search_for_nul_bytes() {
        assert_eq!(Some(1), "a\0".find(ascii_chars!('\0')));
        assert_eq!(Some(0), "\0".find(ascii_chars!('\0')));
        assert_eq!(None, "".find(ascii_chars!('\0')));
    }

    #[test]
    fn can_search_in_nul_bytes() {
        assert_eq!(Some(1), "\0a".find(ascii_chars!('a')));
        assert_eq!(None, "\0".find(ascii_chars!('a')));
    }

    #[test]
    fn pattern_does_not_backtrack_after_first() {
        let mut searcher = ascii_chars!(' ').into_searcher("hello w ");
        assert_eq!(SearchStep::Reject(0, 5), searcher.next());
        assert_eq!(SearchStep::Match(5, 6), searcher.next());
        assert_eq!(SearchStep::Reject(6, 7), searcher.next());
        assert_eq!(SearchStep::Match(7, 8), searcher.next());
        assert_eq!(SearchStep::Done, searcher.next());
    }
}
