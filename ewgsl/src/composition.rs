//! Shader modules can be composed, through the use of `mod` and `use` statements. This module handles combining a set of modules into one.

pub mod file_spans;

use std::{collections::HashMap, marker::PhantomData, path::PathBuf};

use daggy::Dag;

use crate::parsing::ParsedModule;

use self::file_spans::FileSpanned;

#[derive(Debug, thiserror::Error)]
pub enum ImportResolutionError<'a> {
    #[error("root shader file missing: could not find file at path `{root_path}`")]
    MissingRoot { root_path: PathBuf },
    #[error("no such file or directory: `{requested_path}`")]
    UnknownFile {
        requested_identifier: FileSpanned<'a, &'a str>,
        requested_path: PathBuf,
    },
}

/// The id of a module within a working set.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId<'a>(usize, PhantomData<&'a ()>);

/// A collection of modules that can all refer to each other.
pub struct WorkingSet<'a> {
    /// Modules in this set, and their locations relative to the project root.
    modules: Vec<(PathBuf, ParsedModule<'a>)>,
    path_lookup: HashMap<PathBuf, ModuleId<'a>>,
    index_file_path: PathBuf,
}

impl<'a> WorkingSet<'a> {
    /// A new empty set of modules.
    pub fn new(index_file_path: PathBuf) -> Self {
        Self {
            modules: Vec::new(),
            path_lookup: HashMap::new(),
            index_file_path,
        }
    }

    /// Adds a new parsed module, and returns a handle to it within this set.
    pub fn append(&mut self, module: ParsedModule<'a>, path: PathBuf) -> ModuleId<'a> {
        // Add to vec
        let id = self.modules.len();
        self.modules.push((path.clone(), module));

        // Add to lookups
        let id = ModuleId(id, PhantomData);

        self.path_lookup.insert(path, id);

        return id;
    }

    pub fn calculate_hierarchy(&self) -> Result<ModuleHierarchy<'a>, ImportResolutionError<'a>> {
        let root = self.path_lookup.get(&self.index_file_path);
        let root = match root {
            Some(root) => root,
            None => {
                return Err(ImportResolutionError::MissingRoot {
                    root_path: self.index_file_path.clone(),
                })
            }
        };

        todo!()
    }
}

/// The tree of modules that register each other with `mod` expressions.
pub struct ModuleHierarchy<'a> {
    root: ModuleId<'a>,
    structure: Dag<ModuleId<'a>, ()>,
}
