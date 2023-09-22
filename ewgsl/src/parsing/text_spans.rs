use perfect_derive::perfect_derive;
use std::{fmt::Debug, hash::Hash, marker::PhantomData, ops::Range};

use crate::arena::{Arena, Handle};

/// A start (inclusive) and end (exclusive) within a given string marking the location of some text of interest.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    // An empty span, pointing to nothing.
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
#[cfg(feature = "span_erasure")]
pub struct SpansErased(());

mod sealed {
    use super::*;

    pub trait SpanStateSealed {}
    impl SpanStateSealed for SpansPresent {}
    #[cfg(feature = "span_erasure")]
    impl SpanStateSealed for SpansErased {}
}

/// Represents that either the object has span information (using the [`SpansPresent`] marker type), or that the span information has been erased
/// (using the [`SpansErased`] marker type).
pub trait SpanState: 'static + Sized + sealed::SpanStateSealed {
    const SPANS_PRESENT: bool;
}
impl SpanState for SpansPresent {
    const SPANS_PRESENT: bool = true;
}
#[cfg(feature = "span_erasure")]
impl SpanState for SpansErased {
    const SPANS_PRESENT: bool = false;
}

/// An object paired with a span, where the object has itself span information.
#[perfect_derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpannedParent<T, S: SpanState = SpansPresent> {
    inner: T,
    span: Span,
    _state: PhantomData<S>,
}

impl<T, S: SpanState> SpannedParent<T, S> {
    pub fn unwrap(self) -> T {
        self.inner
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> SpannedParent<U, S> {
        SpannedParent {
            inner: f(self.inner),
            span: self.span,
            _state: self._state,
        }
    }
}

impl<T> SpannedParent<T> {
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

#[cfg(feature = "eq")]
impl<'a, T: crate::EqIn<'a>, S: SpanState> crate::EqIn<'a> for SpannedParent<T, S> {
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

/// Allow tests to construct span-erased modules using `.into()`.
#[cfg(feature = "span_erasure")]
#[cfg(test)]
impl<T> From<T> for SpannedParent<T, SpansErased> {
    fn from(value: T) -> Self {
        Self {
            inner: value,
            span: Span::empty(),
            _state: PhantomData,
        }
    }
}

impl<T: Debug, S: SpanState> Debug for SpannedParent<T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if S::SPANS_PRESENT {
            f.debug_struct("SpannedParent")
                .field("inner", &self.inner)
                .field("span", &self.span)
                .finish()
        } else {
            self.inner.fmt(f)
        }
    }
}

/// An object paired with a span, where the object contained does not have span information.
#[perfect_derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpannedLeaf<T, S: SpanState = SpansPresent> {
    inner: T,
    span: Span,
    _state: PhantomData<S>,
}

impl<T, S: SpanState> SpannedLeaf<T, S> {
    pub fn unwrap(self) -> T {
        self.inner
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> SpannedLeaf<U, S> {
        SpannedLeaf {
            inner: f(self.inner),
            span: self.span,
            _state: self._state,
        }
    }
}

impl<T> SpannedLeaf<T> {
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

#[cfg(feature = "eq")]
impl<'a, T: crate::EqIn<'a>, S: SpanState> crate::EqIn<'a> for SpannedLeaf<T, S> {
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

/// Allow tests to construct span-erased modules using `.into()`.
#[cfg(feature = "span_erasure")]
#[cfg(test)]
impl<T> From<T> for SpannedLeaf<T, SpansErased> {
    fn from(value: T) -> Self {
        Self {
            inner: value,
            span: Span::empty(),
            _state: PhantomData,
        }
    }
}

impl<T: Debug, S: SpanState> Debug for SpannedLeaf<T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if S::SPANS_PRESENT {
            f.debug_struct("SpannedLeaf")
                .field("inner", &self.inner)
                .field("span", &self.span)
                .finish()
        } else {
            self.inner.fmt(f)
        }
    }
}

/// Indicates that an object has span information, which can be stripped if the `erase_spans` feature is enabled.
pub(crate) trait Spanned {
    #[cfg(feature = "span_erasure")]
    type Spanless;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless;

    /*type Span;
    type Inner;

    fn span(&self) -> Span;
    fn inner(&self) -> Span;*/
}

impl<T: Spanned> Spanned for SpannedParent<T> {
    #[cfg(feature = "span_erasure")]
    type Spanless = SpannedParent<T::Spanless, SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        SpannedParent {
            inner: self.inner.erase_spans(),
            span: Span::empty(),
            _state: PhantomData,
        }
    }
}

impl<T> Spanned for SpannedLeaf<T> {
    #[cfg(feature = "span_erasure")]
    type Spanless = SpannedLeaf<T, SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        SpannedLeaf {
            inner: self.inner,
            span: Span::empty(),
            _state: PhantomData,
        }
    }
}

impl<T: Spanned> Spanned for Option<T> {
    #[cfg(feature = "span_erasure")]
    type Spanless = Option<T::Spanless>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        self.map(T::erase_spans)
    }
}

impl<T: Spanned> Spanned for Vec<T> {
    #[cfg(feature = "span_erasure")]
    type Spanless = Vec<T::Spanless>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        self.into_iter().map(T::erase_spans).collect()
    }
}

impl<T: Spanned> Spanned for Range<T> {
    #[cfg(feature = "span_erasure")]
    type Spanless = Range<T::Spanless>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        self.start.erase_spans()..self.end.erase_spans()
    }
}

impl<T: Spanned> Spanned for Handle<T> {
    #[cfg(feature = "span_erasure")]
    type Spanless = Handle<T::Spanless>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        self.retype()
    }
}

impl<T: Spanned> Spanned for Arena<T> {
    #[cfg(feature = "span_erasure")]
    type Spanless = Arena<T::Spanless>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        self.map(|vs| vs.erase_spans())
    }
}
