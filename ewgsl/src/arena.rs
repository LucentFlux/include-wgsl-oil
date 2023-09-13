//! Taken (with modification) from https://github.com/gfx-rs/naga/blob/master/src/arena.rs

use std::{
    cmp::Ordering,
    fmt::{self, Debug},
    hash::{self, Hash},
    marker::PhantomData,
    num::NonZeroU32,
    ops,
};

use crate::spans::{self, Spanned};

/// An unique index in the arena array that a handle points to.
/// The "non-zero" part ensures that an `Option<Handle<T>>` has
/// the same size and representation as `Handle<T>`.
type Index = NonZeroU32;

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
        let index = self.index.get() - 1;
        index as usize
    }

    /// Convert a `usize` index into a `Handle<T>`.
    fn from_usize(index: usize) -> Self {
        let handle_index = u32::try_from(index + 1)
            .ok()
            .and_then(Index::new)
            .expect("Failed to insert into arena. Handle overflows");
        Handle::new(handle_index)
    }

    /// Convert a `usize` index into a `Handle<T>`, without range checks.
    const unsafe fn from_usize_unchecked(index: usize) -> Self {
        Handle::new(Index::new_unchecked((index + 1) as u32))
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

impl<T> PartialEq for Handle<T> {
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

impl<T> fmt::Debug for Handle<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "[{}]", self.index)
    }
}

impl<T> hash::Hash for Handle<T> {
    fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
        self.index.hash(hasher)
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
    Handle::new(NonZeroU32::MIN)..Handle::new(NonZeroU32::MIN)
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
pub struct Arena<T, S: spans::Spanning = spans::WithSpans> {
    /// Values of this arena.
    data: Vec<S::Spanned<T>>,
}

impl<T, S: spans::Spanning> Default for Arena<T, S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, S: spans::Spanning> fmt::Debug for Arena<T, S>
where
    S::Spanned<T>: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<T, S: spans::Spanning> Arena<T, S> {
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
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (Handle<T>, &S::Spanned<T>)> {
        self.data
            .iter()
            .enumerate()
            .map(|(i, v)| unsafe { (Handle::from_usize_unchecked(i), v) })
    }

    /// Returns a iterator over the items stored in this arena,
    /// returning both the item's handle and a mutable reference to it.
    pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = (Handle<T>, &mut S::Spanned<T>)> {
        self.data
            .iter_mut()
            .enumerate()
            .map(|(i, v)| unsafe { (Handle::from_usize_unchecked(i), v) })
    }

    /// Adds a new value to the arena, returning a typed handle.
    pub fn append(&mut self, value: S::Spanned<T>) -> Handle<T> {
        let index = self.data.len();
        self.data.push(value);
        Handle::from_usize(index)
    }

    pub fn try_get(&self, handle: Handle<T>) -> Result<&S::Spanned<T>, BadHandle> {
        self.data
            .get(handle.index())
            .ok_or_else(|| BadHandle::new(handle))
    }

    /// Get a mutable reference to an element in the arena.
    pub fn try_get_mut(&mut self, handle: Handle<T>) -> Result<&mut S::Spanned<T>, BadHandle> {
        self.data
            .get_mut(handle.index())
            .ok_or_else(|| BadHandle::new(handle))
    }

    /// Clears the arena keeping all allocations
    pub fn clear(&mut self) {
        self.data.clear()
    }

    /// Assert that `handle` is valid for this arena.
    pub fn contains_handle(&self, handle: Handle<T>) -> bool {
        return handle.index() < self.data.len();
    }

    /// Maps this arena to a new one, where each handle for this one is still valid for the new one, and where the spans are unchanged.
    pub fn map<U>(&self, f: impl FnMut(T) -> U) -> Arena<U, S> {
        Arena {
            data: self
                .data
                .into_iter()
                .map(|v| S::map_spanned(v, f))
                .collect(),
        }
    }
}

impl<T> Arena<T, spans::WithSpans> {
    /// Remove all of the span information from this module. Useful when testing semantic equivalence
    /// of two modules. See [`crate::parsing::ParsedModule::erase_spans`]
    pub fn erase_spans(self) -> Arena<T, spans::WithoutSpans> {
        Arena {
            data: self.data.into_iter().map(|vs| vs.inner).collect(),
        }
    }
}

impl<T, S: spans::Spanning> ops::Index<Handle<T>> for Arena<T, S> {
    type Output = S::Spanned<T>;
    fn index(&self, handle: Handle<T>) -> &S::Spanned<T> {
        &self.data[handle.index()]
    }
}

impl<T, S: spans::Spanning> ops::IndexMut<Handle<T>> for Arena<T, S> {
    fn index_mut(&mut self, handle: Handle<T>) -> &mut S::Spanned<T> {
        &mut self.data[handle.index()]
    }
}

impl<T, S: spans::Spanning> ops::Index<HandleRange<T>> for Arena<T, S> {
    type Output = [S::Spanned<T>];
    fn index(&self, range: HandleRange<T>) -> &[S::Spanned<T>] {
        &self.data[range.start.index()..range.end.index()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_handle_range_is_empty() {
        assert!(empty_handle_range::<u8>().is_empty())
    }
    #[test]
    fn append_non_unique() {
        let mut arena: Arena<u8, spans::WithoutSpans> = Arena::new();
        let t1 = arena.append(0);
        let t2 = arena.append(0);
        assert!(t1 != t2);
        assert!(arena[t1] == arena[t2]);
    }

    #[test]
    fn append_unique() {
        let mut arena: Arena<u8, spans::WithoutSpans> = Arena::new();
        let t1 = arena.append(0);
        let t2 = arena.append(1);
        assert!(t1 != t2);
        assert!(arena[t1] != arena[t2]);
    }
}
