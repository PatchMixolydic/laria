use std::{
    cmp::{max, min},
    ops::Range,
};

/// Represents a highlighted span in a source
/// listing.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Span {
    pub start: usize,
    pub length: usize,
}

impl Span {
    pub const fn new(start: usize, length: usize) -> Self {
        Self { start, length }
    }

    /// Create an empty span that highlights nothing.
    pub const fn empty() -> Self {
        Self {
            start: 0,
            length: 0,
        }
    }

    pub const fn is_empty(&self) -> bool {
        self.length == 0
    }

    /// Produces a new span that contains both `self` and `other`.
    ///
    /// For instance, if we have two spans:
    /// ```rust,ignore
    /// fn main() { println!("Hello world"); }
    /// ^^          ^^^^^^^^
    /// span_a      span_b
    /// ```
    ///
    /// ...then `span_a.combine(span_b)` would return this:
    /// ```rust,ignore
    /// fn main() { println!("Hello world"); }
    /// ^^^^^^^^^^^^^^^^^^^^
    /// span_a.combine(span_b)
    /// ```
    ///
    /// `span_b.combine(span_a)` would produce the same result.
    #[must_use = "`combine` produces a new `Span`"]
    pub fn combine(self, other: Span) -> Span {
        if other.is_empty() {
            // x + 0 = x,
            // so we don't need to do anything
            self
        } else if self.is_empty() {
            // 0 + x = x,
            // so we must take on the value of other
            other
        } else {
            // Span from where the earliest start to the latest end
            let start = min(self.start, other.start);
            let end = max(self.start + self.length, other.start + other.length);
            Span::new(start, end - start)
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::empty()
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..(span.start + span.length)
    }
}

#[cfg(test)]
mod tests {
    use super::Span;

    #[test]
    fn grow_to_contain_low_high() {
        // foo + foo = foo
        // ^       ^   ^^^
        let mut span_a = Span::new(0, 1);
        let span_b = Span::new(2, 1);
        let span_ab = Span::new(0, 3);
        span_a = span_a.combine(span_b);

        assert_eq!(span_a, span_ab);
    }

    #[test]
    fn grow_to_contain_high_low() {
        // foo + foo = foo
        //   ^   ^     ^^^
        let mut span_a = Span::new(2, 1);
        let span_b = Span::new(0, 1);
        let span_ab = Span::new(0, 3);
        span_a = span_a.combine(span_b);

        assert_eq!(span_a, span_ab);
    }

    #[test]
    fn grow_to_contain_same_spot_different_length() {
        // foo + foo = foo
        // ^^^   ^     ^^^
        let mut span_a = Span::new(0, 3);
        let span_b = Span::new(0, 1);
        let span_ab = Span::new(0, 3);
        span_a = span_a.combine(span_b);

        assert_eq!(span_a, span_ab);
    }

    #[test]
    fn grow_to_contain_short_long() {
        // foobar + foobar = foobar
        //  ^          ^^^    ^^^^^
        let mut span_a = Span::new(1, 1);
        let span_b = Span::new(3, 3);
        let span_ab = Span::new(1, 5);
        span_a = span_a.combine(span_b);

        assert_eq!(span_a, span_ab);
    }

    #[test]
    fn grow_to_contain_long_short() {
        // foobar + foobar = foobar
        //  ^^^          ^    ^^^^^
        let mut span_a = Span::new(1, 3);
        let span_b = Span::new(5, 1);
        let span_ab = Span::new(1, 5);
        span_a = span_a.combine(span_b);

        assert_eq!(span_a, span_ab);
    }
}
