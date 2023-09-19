//! Taken (with modification) from https://github.com/gfx-rs/naga/blob/master/src/arena.rs

use std::{
    collections::HashSet,
    fmt,
    hash::Hash,
    marker::PhantomData,
    num::NonZeroUsize,
    ops::{self, Range},
};

use crate::spans::{self, Spanned};

/// An unique index in the arena array that a handle points to.
/// The "non-zero" part ensures that an `Option<Handle<T>>` has
/// the same size and representation as `Handle<T>`.
type Index = NonZeroUsize;

/// A strongly typed reference to an arena item.
///
/// A `Handle` value can be used as an index into an [`Arena`].
pub struct Handle<T> {
    index: Index,
    marker: PhantomData<T>,
}

impl<T> Handle<T> {
    pub(crate) const fn new(index: Index) -> Self {
        Handle {
            index,
            marker: PhantomData,
        }
    }

    /// Returns the zero-based index of this handle.
    pub const fn index(self) -> usize {
        self.index.get() - 1
    }

    /// Convert a `usize` index into a `Handle<T>`.
    fn from_usize(index: usize) -> Self {
        let handle_index =
            Index::new(index + 1).expect("Failed to insert into arena. Handle overflows");
        Handle::new(handle_index)
    }

    /// Convert a `usize` index into a `Handle<T>`, without range checks.
    const unsafe fn from_usize_unchecked(index: usize) -> Self {
        Handle::new(Index::new_unchecked(index + 1))
    }

    /// Gets the next handle, for use in Range generation where the upper bound must be exclusive.
    pub(crate) fn exclusive(&self) -> Self {
        Self {
            index: self.index.saturating_add(1),
            marker: self.marker,
        }
    }
}

impl<T: Spanned> Spanned for Handle<T> {
    type Spanless = Handle<T::Spanless>;

    fn erase_spans(self) -> Self::Spanless {
        Handle {
            index: self.index,
            marker: PhantomData,
        }
    }
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Handle {
            index: self.index,
            marker: self.marker,
        }
    }
}

impl<T> Copy for Handle<T> {}

// Does not make sense to compare handles, since they are essentially pointers.
// We should only compare their pointed to objects, which may be in different arenas.
/*impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for Handle<T> {}

impl<T> PartialOrd for Handle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl<T> Ord for Handle<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index)
    }
}

impl<T> hash::Hash for Handle<T> {
    fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
        self.index.hash(hasher)
    }
}
*/

impl<T> fmt::Debug for Handle<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "[{}]", self.index)
    }
}

#[derive(Clone, Copy, Debug, thiserror::Error, PartialEq)]
#[error("Handle {index} of {kind} is either not present, or inaccessible yet")]
pub struct BadHandle {
    pub kind: &'static str,
    pub index: usize,
}

impl BadHandle {
    fn new<T>(handle: Handle<T>) -> Self {
        Self {
            kind: std::any::type_name::<T>(),
            index: handle.index(),
        }
    }
}

/// A strongly typed range of handles.
pub type HandleRange<T> = ops::Range<Handle<T>>;

pub fn empty_handle_range<T>() -> ops::Range<Handle<T>> {
    Handle::new(NonZeroUsize::MIN)..Handle::new(NonZeroUsize::MIN)
}

// NOTE: Keep this diagnostic in sync with that of [`BadHandle`].
#[derive(Clone, Debug, thiserror::Error)]
#[error("Handle range {range:?} of {kind} is either not present yet, or inaccessible")]
pub struct BadHandleRange {
    // This error is used for many `Handle` types, but there's no point in making this generic, so
    // we just flatten them all to `Handle<()>` here.
    kind: &'static str,
    range: HandleRange<()>,
}

impl BadHandleRange {
    pub fn new<T>(range: HandleRange<T>) -> Self {
        Self {
            kind: std::any::type_name::<T>(),
            range: (Handle::new(range.start.index)..Handle::new(range.end.index)),
        }
    }
}

/// An arena holding some kind of component (e.g., type, constant,
/// instruction, etc.) that can be referenced.
///
/// Adding new items to the arena produces a strongly-typed [`Handle`].
/// The arena can be indexed using the given handle to obtain
/// a reference to the stored item.
pub struct Arena<T, S: spans::SpanState = spans::SpansPresent> {
    /// Values of this arena.
    data: Vec<spans::WithSpan<T, S>>,
}

impl<T, S: spans::SpanState> Arena<T, S> {
    /// Create a new arena with no initial capacity allocated.
    pub const fn new() -> Self {
        Arena { data: Vec::new() }
    }

    /// Returns the current number of items stored in this arena.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns `true` if the arena contains no elements.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns an iterator over the items stored in this arena, returning both
    /// the item's handle and a reference to it.
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (Handle<T>, &spans::WithSpan<T, S>)> {
        self.data
            .iter()
            .enumerate()
            .map(|(i, v)| unsafe { (Handle::from_usize_unchecked(i), v) })
    }

    /// Returns a iterator over the items stored in this arena,
    /// returning both the item's handle and a mutable reference to it.
    pub fn iter_mut(
        &mut self,
    ) -> impl DoubleEndedIterator<Item = (Handle<T>, &mut spans::WithSpan<T, S>)> {
        self.data
            .iter_mut()
            .enumerate()
            .map(|(i, v)| unsafe { (Handle::from_usize_unchecked(i), v) })
    }

    /// Adds a new value to the arena, returning a typed handle.
    pub fn append(&mut self, value: spans::WithSpan<T, S>) -> Handle<T> {
        let index = self.data.len();
        self.data.push(value);
        Handle::from_usize(index)
    }

    /// Uses a handle to look up a value. Use `arena[handle]` for a version that panics on error instead.
    pub fn try_get(&self, handle: Handle<T>) -> Result<&spans::WithSpan<T, S>, BadHandle> {
        self.data
            .get(handle.index())
            .ok_or_else(|| BadHandle::new(handle))
    }

    pub fn try_get_all(
        &self,
        range: Range<Handle<T>>,
    ) -> Result<impl Iterator<Item = &spans::WithSpan<T, S>>, BadHandleRange> {
        let start = range.start.index.get();
        let end = range.end.index.get();

        assert!(
            end >= start,
            "end should always come after start in a range"
        );

        if end > self.data.len() {
            return Err(BadHandleRange::new(range));
        }

        return Ok(self.data[start..end].iter());
    }

    /// Get a mutable reference to an element in the arena.
    pub fn try_get_mut(
        &mut self,
        handle: Handle<T>,
    ) -> Result<&mut spans::WithSpan<T, S>, BadHandle> {
        self.data
            .get_mut(handle.index())
            .ok_or_else(|| BadHandle::new(handle))
    }

    /// Clears the arena keeping all allocations
    pub fn clear(&mut self) {
        self.data.clear()
    }

    /// Reserves capacity for at least `additional` more elements to be inserted
    /// in the given `Vec<T>`. The collection may reserve more space to
    /// speculatively avoid frequent reallocations. After calling `reserve`,
    /// capacity will be greater than or equal to `self.len() + additional`.
    /// Does nothing if capacity is already sufficient.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity exceeds `isize::MAX` bytes.
    pub fn reserve(&mut self, additional: usize) {
        self.data.reserve(additional)
    }

    /// Reserves the minimum capacity for at least `additional` more elements to
    /// be inserted in the given `Vec<T>`. Unlike [`reserve`], this will not
    /// deliberately over-allocate to speculatively avoid frequent allocations.
    /// After calling `reserve_exact`, capacity will be greater than or equal to
    /// `self.len() + additional`. Does nothing if the capacity is already
    /// sufficient.
    ///
    /// Note that the allocator may give the collection more space than it
    /// requests. Therefore, capacity can not be relied upon to be precisely
    /// minimal. Prefer [`reserve`] if future insertions are expected.
    ///
    /// [`reserve`]: Vec::reserve
    ///
    /// # Panics
    ///
    /// Panics if the new capacity exceeds `isize::MAX` bytes.
    pub fn reserve_exact(&mut self, additional: usize) {
        self.data.reserve_exact(additional)
    }

    /// Assert that `handle` is valid for this arena.
    pub fn contains_handle(&self, handle: Handle<T>) -> bool {
        return handle.index() < self.data.len();
    }

    /// Maps this arena to a new one, where each handle for this one is still valid for the new one, and where the spans are unchanged.
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> Arena<U, S> {
        Arena {
            data: self.data.into_iter().map(move |v| v.map(&mut f)).collect(),
        }
    }
}

impl<T> Arena<T, spans::SpansPresent> {
    /// Remove all of the span information from this module. Useful when testing semantic equivalence
    /// of two modules. See [`crate::parsing::ParsedModule::erase_spans`]
    pub fn erase_spans(self) -> Arena<T, spans::SpansErased> {
        Arena {
            data: self.data.into_iter().map(|vs| vs.erase_spans()).collect(),
        }
    }
}

impl<T, S: spans::SpanState> Default for Arena<T, S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, S: spans::SpanState> From<Vec<spans::WithSpan<T, S>>> for Arena<T, S> {
    fn from(data: Vec<spans::WithSpan<T, S>>) -> Self {
        Self { data }
    }
}

impl<T: fmt::Debug, S: spans::SpanState> fmt::Debug for Arena<T, S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<T, S: spans::SpanState> ops::Index<Handle<T>> for Arena<T, S> {
    type Output = spans::WithSpan<T, S>;

    fn index(&self, handle: Handle<T>) -> &spans::WithSpan<T, S> {
        &self.data[handle.index()]
    }
}

impl<T, S: spans::SpanState> ops::IndexMut<Handle<T>> for Arena<T, S> {
    fn index_mut(&mut self, handle: Handle<T>) -> &mut spans::WithSpan<T, S> {
        &mut self.data[handle.index()]
    }
}

impl<T, S: spans::SpanState> ops::Index<HandleRange<T>> for Arena<T, S> {
    type Output = [spans::WithSpan<T, S>];

    fn index(&self, range: HandleRange<T>) -> &[spans::WithSpan<T, S>] {
        &self.data[range.start.index()..range.end.index()]
    }
}

impl<T: Hash + Eq, S: spans::SpanState> PartialEq for Arena<T, S> {
    fn eq(&self, other: &Self) -> bool {
        fn vecs_contain_same<T: Hash + Eq>(v1: &Vec<T>, v2: &Vec<T>) -> bool {
            v1.iter().collect::<HashSet<_>>() == v2.iter().collect::<HashSet<_>>()
        }
        return vecs_contain_same(&self.data, &other.data);
    }
}

impl<T: Hash + Eq, S: spans::SpanState> Eq for Arena<T, S> {}

/// Akin to the [`vec`] macro, for creating an arena inline, with or without spans.
///
/// # Usage
///
/// With spans:
/// ```
/// # let span1 = ewgsl::spans::Span::empty();
/// # let span2 = ewgsl::spans::Span::empty();
/// # let v1 = 12;
/// # let v2 = 24;
/// let arena = ewgsl::arena![
///     span1 => v1,
///     span2 => v2,
///     /* ... */
/// ];
/// ```
///
/// Without spans:
/// ```
/// # let v1 = 7usize;
/// # let v2 = 0usize;
/// let arena = ewgsl::arena![
///     v1,
///     v2,
///     /* ... */
/// ];
/// ```
#[macro_export]
macro_rules! arena {
    (
        $($span:expr => $item:expr),* $(,)?
    ) => {
        $crate::arena::Arena::<_, $crate::spans::SpansPresent>::from(vec![
            $(
                $crate::spans::WithSpan::new($item, $span)
            ),*
        ])
    };

    (
        $($item:expr),* $(,)?
    ) => {{
        use $crate::spans::Spanned;
        $crate::arena::Arena::<_, $crate::spans::SpansErased>::from(vec![
            $(
                $crate::spans::WithSpan::new($item, $crate::spans::Span::empty()).erase_spans()
            ),*
        ])
    }};
}
pub use arena;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_handle_range_is_empty() {
        let empty = empty_handle_range::<u8>();
        assert!(empty.start.index >= empty.end.index)
    }
    #[test]
    fn append_non_unique() {
        let mut arena: Arena<u8, spans::SpansErased> = Arena::new();
        let t1 = arena.append(0.into());
        let t2 = arena.append(0.into());
        assert!(t1.index != t2.index);
        assert!(arena[t1] == arena[t2]);
    }

    #[test]
    fn append_unique() {
        let mut arena: Arena<u8, spans::SpansErased> = Arena::new();
        let t1 = arena.append(0.into());
        let t2 = arena.append(1.into());
        assert!(t1.index != t2.index);
        assert!(arena[t1] != arena[t2]);
    }
}
