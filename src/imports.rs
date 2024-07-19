use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    path::PathBuf,
};

use daggy::{petgraph::visit::IntoNodeReferences, Walker};
use regex::{Captures, Regex};

use crate::{
    files::{AbsoluteRustRootPathBuf, AbsoluteWGSLFilePathBuf},
    module::Module,
};

lazy_static::lazy_static! {
    static ref IMPORT_CUSTOM_PATH_REGEX: Regex = Regex::new(r"(?:^|\n)\s*#\s*import\s+([^\s]+\.wgsl)").unwrap();
    static ref IMPORT_CUSTOM_PATH_AS_REGEX: Regex = Regex::new(r"(?:^|\n)\s*#\s*import\s+([^\s]+\.wgsl)\s+as\s+([^\s]+)").unwrap();
    static ref IMPORT_ITEMS_REGEX: Regex = Regex::new(r"(?:^|\n)\s*#\s*import\s+([^\s]+\.wgsl)\s+([^\s]+(?:\s*,\s*[^\s]+)*)").unwrap();
    static ref IMPORT_SINGLE_ITEM_REGEX: Regex = Regex::new(r"(?:^|\n)\s*#\s*import\s+([^\s]+\.wgsl)\s*::\s*([^\s]+)").unwrap();
    static ref IMPORT_ITEMS_BRACKETS_REGEX: Regex = Regex::new(r"(?:^|\n)\s*#\s*import\s+([^\s]+\.wgsl)\s*::\s*\{\s*([^\s]+(?:\s*,\s*[^\s]+)*)\s*\}").unwrap();
}

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
fn all_imports_in_source<'a>(source: &'a str) -> HashSet<&'a str> {
    let mut requirements = HashSet::new();
    for import in IMPORT_CUSTOM_PATH_REGEX.captures_iter(&source) {
        requirements.insert(import.get(1).unwrap().as_str());
    }
    for import in IMPORT_CUSTOM_PATH_AS_REGEX.captures_iter(&source) {
        requirements.insert(import.get(1).unwrap().as_str());
    }
    for import in IMPORT_ITEMS_REGEX.captures_iter(&source) {
        requirements.insert(import.get(1).unwrap().as_str());
    }
    for import in IMPORT_SINGLE_ITEM_REGEX.captures_iter(&source) {
        requirements.insert(import.get(1).unwrap().as_str());
    }
    for import in IMPORT_ITEMS_BRACKETS_REGEX.captures_iter(&source) {
        requirements.insert(import.get(1).unwrap().as_str());
    }
    return requirements;
}

/// Finds all import declarations in a source file, returning all of the paths given.
fn replace_import_names_in_source<'a>(
    source: &'a str,
    subs: impl Fn(&str) -> Option<String>,
) -> String {
    let source = IMPORT_CUSTOM_PATH_REGEX.replace_all(source, |capture: &Captures<'_>| {
        let full = capture.get(0).unwrap().as_str();

        let name = capture.get(1).unwrap().as_str();
        let sub = match subs(name) {
            Some(sub) => sub,
            None => return full.to_owned(),
        };

        // Right alignment is needed for naga_oil to correctly parse rust-style imports:
        // `#import foo.wgsl::bar` will become `#import      foo::bar`
        // naga_oil does not support spaces between import items
        let sub = format!("{:>len$}", sub, len = name.len());

        capture.get(0).unwrap().as_str().replace(name, &sub)
    });

    return source.to_string();
}

pub(crate) fn replace_imports_in_source(
    source: &str,
    importing: &Module,
    source_root: Option<&AbsoluteRustRootPathBuf>,
    module_names: &HashMap<Module, String>,
) -> String {
    replace_import_names_in_source(&source, |request_string| {
        let import = Module::resolve_module(importing, source_root.clone(), request_string).ok()?;
        module_names.get(&import).cloned()
    })
}

pub(crate) enum ImportResolutionError {
    Cycle {
        cycle_path: Vec<Module>,
    },
    Unresolved {
        requested: String,
        importer: Module,
        searched: HashSet<PathBuf>,
    },
}

impl Display for ImportResolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportResolutionError::Cycle { cycle_path } => {
                write!(f, "found import cycle: \n")?;
                for node in cycle_path {
                    write!(f, "`{}` ->\n", node)?;
                }
                write!(f, "`{}`", cycle_path.first().unwrap())
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
                    importer,
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
    dag: daggy::Dag<Module, ()>,
    node_of_interest: daggy::NodeIndex,
}

impl ImportOrder {
    /// Given a root module, traverses the file system to find all imports
    pub(crate) fn calculate(
        absolute_source_path: AbsoluteWGSLFilePathBuf,
        source_root: Option<&AbsoluteRustRootPathBuf>,
    ) -> Result<Self, ImportResolutionError> {
        let root_import = Module::from_path(absolute_source_path);

        let mut order = daggy::Dag::<Module, ()>::new();
        let mut nodes = HashMap::new();

        // Follow a DFS over imports, detecting cycles using daggy.
        let mut search_front =
            std::collections::VecDeque::from(vec![(Option::<Module>::None, root_import.clone())]);
        while let Some((importing_path, imported)) = search_front.pop_front() {
            // If we haven't seen the dependency before, add it to the record
            let imported_node = match nodes.get(&imported) {
                None => {
                    let node = order.add_node(imported.clone());
                    nodes.insert(imported.clone(), node);
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
            let source = imported.read_to_string();
            for requested in all_imports_in_source(&source) {
                match Module::resolve_module(&imported, source_root, requested) {
                    Ok(import) => search_front.push_back((Some(imported.clone()), import)),
                    Err(err) => {
                        return Err(ImportResolutionError::Unresolved {
                            requested: requested.to_owned(),
                            importer: imported,
                            searched: err.into_iter().collect(),
                        });
                    }
                }
            }
        }

        return Ok(ImportOrder {
            dag: order,
            node_of_interest: nodes[&root_import],
        });
    }

    /// Gives a vector of every node that needs to be imported, in order of import from leaf to the node of interest.
    /// The root node is excluded from the import order.
    fn import_order(mut self) -> Vec<Module> {
        let mut res = Vec::new();

        // Drain dag
        while self.dag.node_count() > 0 {
            let mut removed_one = false;
            for (node, _) in self.dag.node_references() {
                // If no children, import
                if self.dag.children(node).iter(&self.dag).next().is_none() {
                    let import = self
                        .dag
                        .remove_node(node)
                        .expect("iterating over node ids present");

                    // Don't need to import the root node
                    if node != self.node_of_interest {
                        res.push(import);
                    }

                    removed_one = true;
                    break;
                }
            }
            assert!(removed_one, "DAGs must always have a node with no children");
        }

        return res;
    }

    /// Generates versions of the paths referred to by this import set, to deduplicate imports in `naga_oil` which refer to the same file but use a different path.
    pub(crate) fn reduced_names(&self) -> HashMap<Module, String> {
        let mut forwards = HashMap::new();
        let mut backwards = HashMap::new();

        // Assign names by increasing the amount of the path present until distinguished
        // First assign each path just its suffix, without the extension
        for (_, import) in self.dag.node_references() {
            let file_name = import.file_name().to_string();

            forwards.insert(import.clone(), file_name.clone());
            backwards
                .entry(file_name)
                .or_insert(vec![])
                .push((1usize, import.clone()));
        }

        // Then remove from backwards any non-collisions and resolve collisions until no collisions are present
        while let Some(colliding_name) = backwards.keys().next() {
            let colliding_name = colliding_name.clone();
            let collisions = backwards.remove(&colliding_name).expect("just popped key");
            if collisions.len() <= 1 {
                // No collision
                continue;
            }

            for (i, (path_size, import)) in collisions.into_iter().enumerate() {
                forwards.remove(&import);

                let new_name = if let Some(extra_component) = import.nth_path_component(path_size) {
                    colliding_name.clone() + "_" + &extra_component
                } else {
                    colliding_name.clone() + &format!("{}", i)
                };

                forwards.insert(import.clone(), new_name.clone());
                backwards
                    .entry(new_name)
                    .or_insert(vec![])
                    .push((path_size + 1, import.clone()));
            }
        }

        return forwards;
    }

    /// Gives a vector containing every file that needs to be imported, in order of import from leaf to the node of interest,
    /// and the root module.
    pub(crate) fn modules(self) -> (Vec<Module>, Module) {
        let root = self.dag[self.node_of_interest].clone();
        let imports = self.import_order();
        assert!(!imports.contains(&root));
        return (imports, root);
    }
}
