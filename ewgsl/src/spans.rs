use std::{fmt::Debug, hash::Hash};

/// A start (inclusive) and end (exclusive) within a given string marking the location of some text of interest.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    /// The start (inclusive) byte that this span covers.
    pub fn start(&self) -> usize {
        self.start
    }
    /// The end (exclusive) byte that this span covers.
    pub fn end(&self) -> usize {
        self.end
    }

    pub fn union(&self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.min(other.end),
        }
    }
}

impl From<pest::error::InputLocation> for Span {
    fn from(value: pest::error::InputLocation) -> Self {
        match value {
            pest::error::InputLocation::Pos(pos) => Self {
                start: pos,
                end: pos + 1,
            },
            pest::error::InputLocation::Span((start, end)) => Self { start, end },
        }
    }
}

impl<'a> From<pest::Span<'a>> for Span {
    fn from(value: pest::Span) -> Self {
        Self {
            start: value.start(),
            end: value.end(),
        }
    }
}

/// An object paired with a span.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct WithSpan<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> WithSpan<T> {
    pub fn erase_span(self) -> T {
        self.inner
    }
}

impl<T> AsRef<T> for WithSpan<T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T> AsMut<T> for WithSpan<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

/// Marker type denoting that this object carries span information with its children.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct WithSpans(());
/// Marker type denoting that this object does not carry span information with its children.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct WithoutSpans(());

pub trait SpanPair {
    type Spanned<T: Debug + Clone + Hash + Eq>: Debug + Clone + Hash + Eq;
}

impl SpanPair for WithSpans {
    type Spanned<T: Debug + Clone + Hash + Eq> = WithSpan<T>;
}

impl SpanPair for WithoutSpans {
    type Spanned<T: Debug + Clone + Hash + Eq> = T;
}
