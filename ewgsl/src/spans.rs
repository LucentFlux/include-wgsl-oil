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

impl<T> Spanned for WithSpan<T> {
    type Spanless = T;

    fn erase_spans(self) -> Self::Spanless {
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
#[derive(Debug)]
pub struct WithSpans(());
/// Marker type denoting that this object does not carry span information with its children.
#[derive(Debug)]
pub struct WithoutSpans(());

mod sealed {
    use super::*;
    pub trait SpanPairSealed {}
    impl SpanPairSealed for WithSpans {}
    impl SpanPairSealed for WithoutSpans {}
}

/// Represents that either the object has span information (using the [`WithSpans`] marker type), or that it does not
/// (using the [`WithoutSpans`] marker type).
pub trait Spanning: sealed::SpanPairSealed + Debug {
    type Spanned<T>;
    type Span;

    /// Maps a spanned object, preserving any span information.
    fn map_spanned<T, U>(v: Self::Spanned<T>, f: impl FnOnce(T) -> U) -> Self::Spanned<U>;
    fn get_span<T>(v: &Self::Spanned<T>) -> Self::Span;
}

impl Spanning for WithSpans {
    type Spanned<T> = WithSpan<T>;
    type Span = Span;

    fn map_spanned<T, U>(v: Self::Spanned<T>, f: impl FnOnce(T) -> U) -> Self::Spanned<U> {
        WithSpan {
            span: v.span,
            inner: f(v.inner),
        }
    }
    fn get_span<T>(v: &Self::Spanned<T>) -> Self::Span {
        v.span
    }
}

impl Spanning for WithoutSpans {
    type Spanned<T> = T;
    type Span = ();

    fn map_spanned<T, U>(v: Self::Spanned<T>, f: impl FnOnce(T) -> U) -> Self::Spanned<U> {
        f(v)
    }
    fn get_span<T>(_: &Self::Spanned<T>) -> Self::Span {
        ()
    }
}

/// Indicates that an object has span information that can be stripped.
pub trait Spanned {
    type Spanless;

    fn erase_spans(self) -> Self::Spanless;
}

impl<T: Spanned> Spanned for Vec<T> {
    type Spanless = Vec<T::Spanless>;

    fn erase_spans(self) -> Self::Spanless {
        self.into_iter().map(T::erase_spans).collect()
    }
}