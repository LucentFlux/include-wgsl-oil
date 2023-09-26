use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    path::PathBuf,
};

use daggy::petgraph::visit::IntoNodeReferences;
use naga_oil::compose::{ComposableModuleDescriptor, ShaderDefValue, ShaderLanguage};

use crate::{
    files::{AbsoluteRustRootPathBuf, AbsoluteWGSLFilePathBuf},
    IMPORT_CUSTOM_PATH_AS_REGEX, IMPORT_CUSTOM_PATH_REGEX, IMPORT_ITEMS_REGEX,
};

/// Finds an arbitrary path between two nodes in a dag.
fn find_any_path<N, E>(
    dag: &daggy::Dag<N, E>,
    start: daggy::NodeIndex,
    end: daggy::NodeIndex,
) -> Vec<daggy::NodeIndex> {
    daggy::petgraph::algo::all_simple_paths(dag, start, end, 0, None)
        .next()
        .expect("`find_any_path` should only be called when such a path exists")
}

/// Finds all import declarations in a source file, returning all of the paths given.
fn all_imports_in_source<'a>(source: &'a str) -> Vec<&'a str> {
    let mut requirements = Vec::new();
    for import in IMPORT_CUSTOM_PATH_AS_REGEX.captures_iter(&source) {
        requirements.push(import.get(1).unwrap().as_str())
    }
    for import in IMPORT_CUSTOM_PATH_REGEX.captures_iter(&source) {
        requirements.push(import.get(1).unwrap().as_str())
    }
    for import in IMPORT_ITEMS_REGEX.captures_iter(&source) {
        requirements.push(import.get(1).unwrap().as_str())
    }
    return requirements;
}

/// Given a path to a file and the string given to describe an import, tries to resolve the requested import file.
fn resolve_import(
    importing: &AbsoluteWGSLFilePathBuf,
    source_root: Option<&AbsoluteRustRootPathBuf>,
    request_string: &str,
) -> Result<PathBuf, Vec<PathBuf>> {
    let mut tried_paths = Vec::new();

    // Try interpret as relative to importing file
    let relative = importing.join(request_string);
    tried_paths.push(relative.clone());
    if relative.exists() {
        return Ok(relative.canonicalize().unwrap());
    }

    // Try interpret as relative to source root
    if let Some(source_root) = source_root {
        let relative = source_root.join(request_string);
        tried_paths.push(relative.clone());
        if relative.exists() {
            return Ok(relative.canonicalize().unwrap());
        }
    }

    return Err(tried_paths);
}

pub(crate) enum ImportResolutionError {
    Cycle {
        cycle_path: Vec<AbsoluteWGSLFilePathBuf>,
    },
    Unresolved {
        requested: String,
        importer: PathBuf,
        searched: HashSet<PathBuf>,
    },
}

impl Display for ImportResolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportResolutionError::Cycle { cycle_path } => {
                write!(f, "found import cycle: \n")?;
                for node in cycle_path {
                    write!(f, "`{}` ->\n", node.display())?;
                }
                write!(f, "`{}`", cycle_path.first().unwrap().display())
            }
            ImportResolutionError::Unresolved {
                requested,
                importer,
                searched,
            } => {
                write!(
                    f,
                    "could not resolve import `{}` in file `{}`:\nlooked in location(s) {}",
                    requested,
                    importer.display(),
                    searched
                        .into_iter()
                        .map(|path| format!("`{}`", path.display()))
                        .fold(String::new(), |a, b| a + ", " + &b)
                )
            }
        }
    }
}

/// Gives all of the files required for a module and the order in which they need to be processed by `naga_oil::compose`.
pub(crate) struct ImportOrder {
    dag: daggy::Dag<AbsoluteWGSLFilePathBuf, ()>,
    node_of_interest: daggy::NodeIndex,
}

impl ImportOrder {
    /// Given a root module, traverses the file system to find all imports
    pub(crate) fn calculate(
        absolute_source_path: AbsoluteWGSLFilePathBuf,
        source_root: Option<&AbsoluteRustRootPathBuf>,
    ) -> Result<Self, ImportResolutionError> {
        assert!(absolute_source_path.is_file());
        assert!(absolute_source_path.is_absolute());

        let mut order = daggy::Dag::<AbsoluteWGSLFilePathBuf, ()>::new();
        let mut nodes = HashMap::new();

        // Follow a DFS over imports, detecting cycles using daggy.
        let mut search_front = std::collections::VecDeque::from(vec![(
            Option::<AbsoluteWGSLFilePathBuf>::None,
            absolute_source_path.clone(),
        )]);
        while let Some((importing_path, imported_path)) = search_front.pop_front() {
            assert!(imported_path.is_file());
            assert!(imported_path.is_absolute());

            // If we haven't seen the dependency before, add it to the record
            let imported_node = match nodes.get(&imported_path) {
                None => {
                    let node = order.add_node(imported_path.clone());
                    nodes.insert(imported_path.clone(), node);
                    node
                }
                Some(node) => *node,
            };

            // If it was imported by a file, add an import edge
            if let Some(importing_path) = importing_path {
                let importing_node = *nodes
                    .get(&importing_path)
                    .expect("importees should always be added before their imports");

                let res = order.add_edge(importing_node, imported_node, ());
                if let Err(_) = res {
                    // Cycle on imports
                    let cycle_path = find_any_path(&order, imported_node, importing_node);
                    let cycle_path = cycle_path
                        .into_iter()
                        .map(|node| order[node].clone())
                        .collect();
                    return Err(ImportResolutionError::Cycle { cycle_path });
                }
            }

            // Then add the imports requested by this file
            let source = match std::fs::read_to_string(&*imported_path) {
                Ok(source) => source,
                Err(_) => continue,
            };
            for requested in all_imports_in_source(&source) {
                match resolve_import(&imported_path, source_root, requested) {
                    Ok(import) => search_front.push_back((
                        Some(imported_path.clone()),
                        AbsoluteWGSLFilePathBuf::new(import),
                    )),
                    Err(err) => {
                        return Err(ImportResolutionError::Unresolved {
                            requested: requested.to_owned(),
                            importer: imported_path.to_path_buf(),
                            searched: err.into_iter().collect(),
                        });
                    }
                }
            }
        }

        return Ok(ImportOrder {
            dag: order,
            node_of_interest: nodes[&absolute_source_path],
        });
    }

    /// Gives a vector of every node that needs to be imported, in order of import from leaf to the node of interest.
    /// The root node is excluded from the import order.
    fn import_order(&self) -> Vec<daggy::NodeIndex> {
        let mut res_reversed = Vec::new();

        let mut dfs = daggy::petgraph::visit::Bfs::new(&self.dag, self.node_of_interest);
        while let Some(node) = dfs.next(&self.dag) {
            if node != self.node_of_interest {
                res_reversed.push(node);
            }
        }

        res_reversed.reverse();
        return res_reversed;
    }

    /// Generates versions of the paths referred to by this import set, to deduplicate imports in `naga_oil` which refer to the same file but use a different path.
    pub(crate) fn reduced_names(&self) -> HashMap<AbsoluteWGSLFilePathBuf, String> {
        let mut forwards = HashMap::new();
        let mut backwards = HashMap::new();

        // Assign names by increasing the amount of the path present until distinguished
        // First assign each path just its suffix, without the extension
        for (_, path) in self.dag.node_references() {
            let file_name = path
                .file_name()
                .unwrap()
                .to_string_lossy()
                .split(".")
                .next()
                .unwrap()
                .to_string();

            forwards.insert(path.clone(), file_name.clone());
            backwards
                .entry(file_name)
                .or_insert(vec![])
                .push((1usize, path.clone()));
        }

        // Then remove from backwards any non-collisions and resolve collisions until no collisions are present
        while let Some(colliding_name) = backwards.keys().next() {
            let colliding_name = colliding_name.clone();
            let collisions = backwards.remove(&colliding_name).expect("just popped key");
            if collisions.len() <= 1 {
                // No collision
                continue;
            }

            for (i, (path_size, path)) in collisions.into_iter().enumerate() {
                forwards.remove(&path);

                let path_part_count = path.components().count();
                let new_name = if path_size >= path_part_count {
                    colliding_name.clone() + &format!("{}", i)
                } else {
                    let extra_component = path
                        .components()
                        .rev()
                        .skip(path_size)
                        .next()
                        .expect("checked len was less than size")
                        .as_os_str()
                        .to_string_lossy();
                    colliding_name.clone() + &extra_component
                };

                forwards.insert(path.clone(), new_name.clone());
                backwards
                    .entry(new_name)
                    .or_insert(vec![])
                    .push((path_size + 1, path.clone()));
            }
        }

        return forwards;
    }

    /// Gives an iterator over every file that needs to be imported, in order of import from leaf to the node of interest.
    /// The root node is excluded from the iterator.
    pub(crate) fn iter<'a>(&'a self) -> impl Iterator<Item = Import<'a>> {
        self.import_order().into_iter().map(|node| Import {
            path: &self.dag[node],
        })
    }
}

pub(crate) struct OwnedComposableImport {
    source: String,
    file_path: String,
    as_name: Option<String>,
    shader_defs: HashMap<String, ShaderDefValue>,
}

impl OwnedComposableImport {
    pub(crate) fn borrow_composable_descriptor<'a>(&'a self) -> ComposableModuleDescriptor<'a> {
        ComposableModuleDescriptor {
            source: &self.source,
            file_path: &self.file_path,
            language: ShaderLanguage::Wgsl,
            as_name: self.as_name.clone(),
            additional_imports: &[],
            shader_defs: self.shader_defs.clone(),
        }
    }
}

/// A single requested import to a shader.
pub(crate) struct Import<'a> {
    path: &'a AbsoluteWGSLFilePathBuf,
}

impl<'a> Import<'a> {
    pub(crate) fn to_composable_module_descriptor(
        &self,
        module_names: &HashMap<AbsoluteWGSLFilePathBuf, String>,
        definitions: HashMap<String, ShaderDefValue>,
    ) -> Result<OwnedComposableImport, Vec<String>> {
        let source = match std::fs::read_to_string(&**self.path) {
            Ok(source) => source,
            Err(e) => panic!("failed to read `wgsl` source file: {}", e),
        };

        if source.contains("#define") {
            return Err(vec![format!(
                r"imported shader file `{}` contained a `#define` statement \
                - only top-level files may contain preprocessor definitions",
                self.path.display()
            )]);
        }

        // Replace `@export` directives with equivalent whitespace
        let source = source.replace("@export", "       ");

        let name = &module_names[&*self.path];
        Ok(OwnedComposableImport {
            source,
            file_path: self.path.to_string_lossy().to_string(),
            as_name: Some(name.clone()),
            shader_defs: definitions,
        })
    }

    pub(crate) fn path(&self) -> AbsoluteWGSLFilePathBuf {
        self.path.clone()
    }
}
