use perfect_derive::perfect_derive;
use std::{fmt::Debug, hash::Hash, marker::PhantomData, ops::Range};

use crate::EqIn;

/// A start (inclusive) and end (exclusive) within a given string marking the location of some text of interest.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start == 0 && self.end == 0 {
            f.write_str("EmptySpan")
        } else {
            f.debug_struct("Span")
                .field("start", &self.start)
                .field("end", &self.end)
                .finish()
        }
    }
}

/// Marker type denoting that this object carries valid span information with its children.
#[derive(Debug)]
pub struct SpansPresent(());
/// Marker type denoting that this object's spans have been erased.
#[derive(Debug)]
pub struct SpansErased(());

mod sealed {
    use super::*;

    pub trait SpanStateSealed {}
    impl SpanStateSealed for SpansPresent {}
    impl SpanStateSealed for SpansErased {}
}

/// Represents that either the object has span information (using the [`SpansPresent`] marker type), or that the span information has been erased
/// (using the [`SpansErased`] marker type).
pub trait SpanState: sealed::SpanStateSealed + 'static {
    const SPANS_PRESENT: bool;
}
impl SpanState for SpansPresent {
    const SPANS_PRESENT: bool = true;
}
impl SpanState for SpansErased {
    const SPANS_PRESENT: bool = false;
}

/// An object paired with a span.
#[perfect_derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct WithSpan<T, S: SpanState = SpansPresent> {
    inner: T,
    span: Span,
    _state: PhantomData<S>,
}

impl<T, S: SpanState> WithSpan<T, S> {
    pub fn unwrap(self) -> T {
        self.inner
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithSpan<U, S> {
        WithSpan {
            inner: f(self.inner),
            span: self.span,
            _state: self._state,
        }
    }
}

impl<T> WithSpan<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self {
            inner,
            span,
            _state: PhantomData,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl<T> Spanned for WithSpan<T> {
    type Spanless = WithSpan<T, SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        WithSpan {
            inner: self.inner,
            span: Span::empty(),
            _state: PhantomData,
        }
    }
}

impl<'a, T: EqIn<'a>, S: SpanState> EqIn<'a> for WithSpan<T, S> {
    type Context<'b> = T::Context<'b> where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        self.span == other.span && self.inner.eq_in(own_context, &other.inner, other_context)
    }
}

impl<T> From<T> for WithSpan<T, SpansErased> {
    fn from(value: T) -> Self {
        Self {
            inner: value,
            span: Span::empty(),
            _state: PhantomData,
        }
    }
}

impl<T: Debug, S: SpanState> Debug for WithSpan<T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if S::SPANS_PRESENT {
            f.debug_struct("WithSpan")
                .field("inner", &self.inner)
                .field("span", &self.span)
                .finish()
        } else {
            self.inner.fmt(f)
        }
    }
}

/// Indicates that an object has span information that can be stripped.
pub(crate) trait Spanned {
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
