use std::{
    fmt::Debug,
    hash::Hash,
    ops::{Deref, Range},
};

use crate::parsing::{
    expression::{BinaryOperator, Swizzle, UnaryOperator},
    ident::Ident,
};

/// A start (inclusive) and end (exclusive) within a given string marking the location of some text of interest.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn empty() -> Self {
        Self { start: 0, end: 0 }
    }

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

impl<T> MaybeSpanned<T> for WithSpan<T> {
    type Span = Span;
    type Mapped<U> = WithSpan<U>;

    fn inner(&self) -> &T {
        &self.inner
    }

    fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    fn span(&self) -> Self::Span {
        self.span
    }

    fn map_spanned<U>(self, f: impl FnOnce(T) -> U) -> Self::Mapped<U> {
        WithSpan {
            inner: f(self.inner),
            span: self.span,
        }
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

impl<T> Deref for WithSpan<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// Marker type denoting that this object carries span information with its children.
#[derive(Debug)]
pub struct WithSpans(());
/// Marker type denoting that this object does not carry span information with its children.
#[derive(Debug)]
pub struct WithoutSpans(());

/// Represents an object that may or may not carry span information about the type `T`.
pub trait MaybeSpanned<T> {
    /// The span information carried, or `()`.
    type Span: PartialEq + Eq + Debug + Hash + Copy + Clone;
    /// Another, equally spanned object but holding a different value.
    type Mapped<U>: MaybeSpanned<U, Span = Self::Span>;

    fn inner(&self) -> &T;
    fn inner_mut(&mut self) -> &mut T;

    /// Extracts just the span information from this object
    fn span(&self) -> Self::Span;

    /// Maps this possibly spanned object, preserving any span information.
    fn map_spanned<U>(self, f: impl FnOnce(T) -> U) -> Self::Mapped<U>;
}

impl<T> MaybeSpanned<T> for T {
    type Span = ();
    type Mapped<U> = U;

    fn inner(&self) -> &T {
        self
    }

    fn inner_mut(&mut self) -> &mut T {
        self
    }

    fn span(&self) -> Self::Span {
        ()
    }

    fn map_spanned<U>(self, f: impl FnOnce(T) -> U) -> Self::Mapped<U> {
        f(self)
    }
}

mod sealed {
    use super::*;

    pub trait SpanPairSealed {}
    impl SpanPairSealed for WithSpans {}
    impl SpanPairSealed for WithoutSpans {}
}

/// Represents that either the object has span information (using the [`WithSpans`] marker type), or that it does not
/// (using the [`WithoutSpans`] marker type).
pub trait Spanning: sealed::SpanPairSealed + Debug {
    type Spanned<T>: MaybeSpanned<T, Span = Self::Span>;
    type Span: PartialEq + Eq + Debug + Hash + Copy + Clone;
}

impl Spanning for WithSpans {
    type Spanned<T> = WithSpan<T>;
    type Span = Span;
}

impl Spanning for WithoutSpans {
    type Spanned<T> = T;
    type Span = ();
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

impl<T: Spanned> Spanned for Range<T> {
    type Spanless = Range<T::Spanless>;

    fn erase_spans(self) -> Self::Spanless {
        self.start.erase_spans()..self.end.erase_spans()
    }
}
