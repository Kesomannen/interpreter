#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn merge(a: Span, b: Span) -> Self {
        Self {
            start: a.start,
            end: b.end,
        }
    }

    pub fn extend_with(&mut self, other: Span) {
        self.end = other.end;
    }

    pub fn extend(&mut self, to: usize) {
        self.end = to;
    }
}
