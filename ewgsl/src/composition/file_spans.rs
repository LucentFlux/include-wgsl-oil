use std::{fmt::Debug, hash::Hash};

use super::ModuleId;

/// A start (inclusive) and end (exclusive) within a given string marking the location of some text of interest.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileSpan<'a> {
    start: usize,
    end: usize,
    file: Option<ModuleId<'a>>,
}

impl<'a> FileSpan<'a> {
    // An empty span, pointing to nothing.
    pub fn empty() -> Self {
        Self {
            start: 0,
            end: 0,
            file: None,
        }
    }

    /// The start (inclusive) byte that this span covers.
    pub fn start(&self) -> usize {
        self.start
    }
    /// The end (exclusive) byte that this span covers.
    pub fn end(&self) -> usize {
        self.end
    }
    /// The module id that this span refers to, inside of a [`super::WorkingSet`].
    pub fn module_id(&self) -> Option<ModuleId<'a>> {
        self.file
    }
}

/// An object, associated with a location in a file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileSpanned<'a, T> {
    inner: T,
    span: FileSpan<'a>,
}
