// TODO: Try boxing the closure to see if we can hide the type
// TODO: Or maybe use a function pointer?

pub struct Bytes<F> {
    fallback: F,
}

impl<F> Bytes<F>
where
    F: Fn(u8) -> bool,
{
    pub fn new(fallback: F) -> Self {
        Bytes { fallback }
    }

    pub fn find(&self, haystack: &[u8]) -> Option<usize> {
        haystack.iter().copied().position(&self.fallback)
    }

    pub fn iter<'a>(&'a self, haystack: &'a [u8]) -> BytesIter<'a, F> {
        BytesIter {
            bytes: self,
            haystack,
            offset: 0,
        }
    }
}

pub struct ByteSubstring<'a> {
    needle: &'a [u8],
}

impl<'a> ByteSubstring<'a> {
    pub fn new(needle: &'a[u8]) -> Self {
        ByteSubstring { needle }
    }

    #[cfg(feature = "pattern")]
    pub fn needle_len(&self) -> usize {
        self.needle.len()
    }

    pub fn find(&self, haystack: &[u8]) -> Option<usize> {
        haystack
            .windows(self.needle.len())
            .position(|window| window == self.needle)
    }
}

pub struct BytesIter<'a, F> {
    bytes: &'a Bytes<F>,
    haystack: &'a [u8],
    offset: usize,
}
impl<'a, F> Iterator for BytesIter<'a, F>
where 
    F: Fn(u8) -> bool,
{
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.bytes.find(self.haystack);
        if let Some(idx) = idx {
            self.haystack = &self.haystack[idx + 1..];
            let result = self.offset + idx;
            self.offset = result + 1;
            Some(result)
        } else {
            self.haystack = &[];
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.haystack.len()))
    }
}
