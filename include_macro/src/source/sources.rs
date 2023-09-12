//! This module is responsable for collecting all of the shader files in a project.

use std::{ffi::OsStr, path::PathBuf};

use naga_oil::compose::{ComposableModuleDescriptor, ShaderLanguage};
use regex::Regex;
use thiserror::Error;

use super::defs::ShaderDefs;

lazy_static::lazy_static! {
    static ref RE_POSSIBLE_DEFINITION: Regex = Regex::new(r"#\s*define(\s+([\w|\d|_]+)\s*([-\w|\d]+)?)?").unwrap();
    static ref RE_POSSIBLE_IMPORT_LINES: Regex = Regex::new(r"^.*(#\s*import\s+[^\s]*( as [^\s]*)?).*$").unwrap();
    static ref RE_KEYWORDS: Regex = Regex::new(r"alias|break|case|const|const_assert|continue|continuing|default|diagnostic|discard|else|enable|false|fn|for|if|let|loop|override|requires|return|struct|switch|true|var|while").unwrap();
}

fn line_of_index_in_string(src: &str, index: usize) -> usize {
    let mut i_line = 0;
    for (i_char, char) in src.chars().enumerate() {
        if i_char == index {
            break;
        }
        if char == '\n' {
            i_line += 1;
        }
    }
    return i_line;
}

/// Represents a reason why a shader in the project source tree could not be made into a composable module.
/// Note that this doesn't necessarily constitute an error, unless the user's top level shader aims to
/// use this module.
///
/// The Display implementation of this error is written in such a way that `format!("file {file} {error}")` is grammatically correct.
#[derive(Debug, Error)]
pub(crate) enum ComposableModuleFailure {
    #[error("could not be read: {0}")]
    ReadFileError(std::io::Error),
    #[error("contained definition `{definition_source}` at line {definition_line}, however definition statements are only allowed on top level shaders")]
    ContainedDefinitionError {
        definition_source: String,
        definition_line: usize,
    },
    #[error("contained malformed import statement `{import_found}` at line {line_number}: {msg}")]
    MalformedImport {
        import_found: String,
        line_number: usize,
        msg: String,
    },
    #[error("contained WGSL definition on line {other_item_found_line}, before the last import on line {last_import_line} - imports must occur at the start of a file, before any WGSL objects")]
    ImportAfterOtherItem {
        last_import_line: usize,
        other_item_found_line: usize,
    },
}

/// Returns true if the path is to a `.wgsl` file.
fn is_to_wgsl(path: &PathBuf) -> bool {
    match path.extension().and_then(OsStr::to_str) {
        None => false,
        Some(v) => match v {
            "wgsl" => true,
            _ => false,
        },
    }
}

fn is_in_comment(source: &str, location: usize) -> bool {
    let mut last_char = ' ';
    let mut is_in_line_comment = false;
    let mut multiline_nesting = 0;
    for (i_char, char) in source.chars().enumerate() {
        if i_char == location {
            return !is_in_line_comment && multiline_nesting == 0;
        }

        if char == '\n' {
            is_in_line_comment = false;
        }
        if last_char == '/' && char == '*' {
            multiline_nesting += 1;
        }
        if last_char == '*' && char == '/' {
            multiline_nesting -= 1;
        }

        last_char = char;
    }
    return false;
}

fn check_src_has_no_definitions(source: &str) -> Result<(), ComposableModuleFailure> {
    if let Some(definition) = RE_POSSIBLE_DEFINITION.find(&source) {
        let definition_source = definition.as_str().to_owned();

        if !is_in_comment(&source, definition.start()) {
            let definition_line = line_of_index_in_string(&source, definition.start());
            return Err(ComposableModuleFailure::ContainedDefinitionError {
                definition_source,
                definition_line,
            });
        }
    }
    return Ok(());
}

fn check_src_imports_well_formed(source: &str) -> Result<(), ComposableModuleFailure> {
    // Find all the lines with imports, and check they contain nothing but the import, noting the last line containing an import.
    let mut last_import_line = None;
    for possible_import_line in RE_POSSIBLE_IMPORT_LINES.captures_iter(&source) {
        let import_line = possible_import_line
            .get(0)
            .expect("all captures have a primary group");
        let line_number = line_of_index_in_string(&source, import_line.start());

        let import_part = possible_import_line
            .get(1)
            .expect("import line first subgroup is import part");

        if import_part.as_str() != import_line.as_str()
            || !import_part.as_str().starts_with("#import")
        {
            let msg = if import_line.as_str().starts_with(" ") {
                "import lines must not be indented"
            } else if import_line.as_str().starts_with("#") {
                "there should be no whitespace between the `#` and `import` keyword"
            } else {
                "lines with import statements must start with `#import` and contain nothing but a single import"
            };
            return Err(ComposableModuleFailure::MalformedImport {
                import_found: import_part.as_str().to_owned(),
                line_number,
                msg: msg.to_owned(),
            });
        }

        last_import_line = Some(last_import_line.unwrap_or(line_number).max(line_number));
    }

    // Check that no lines above imports have anything other than imports
    if let Some(last_import_line) = last_import_line {
        for line in source.lines().take(last_import_line) {
            if RE_POSSIBLE_IMPORT_LINES.is_match(line) {
                continue;
            }
            for keyword in RE_KEYWORDS.find_iter(line) {
                if !is_in_comment(source, keyword.start()) {
                    return Err(ComposableModuleFailure::ImportAfterOtherItem {
                        last_import_line,
                        other_item_found_line: line_of_index_in_string(&source, keyword.start()),
                    });
                }
            }
        }
    }

    Ok(())
}

pub(super) struct ShaderInProject {
    absolute_path: PathBuf,
    relative_path: PathBuf,
}

impl ShaderInProject {
    fn new(project_source_root: PathBuf, absolute_path: PathBuf) -> Self {
        assert!(
            absolute_path.exists() && absolute_path.is_file(),
            "shader in project should exist and be a file"
        );
        assert!(
            is_to_wgsl(&absolute_path),
            "shader in project should be a wgsl file"
        );

        Self {
            absolute_path,
            relative_path: absolute_path
                .strip_prefix(&project_source_root)
                .expect("all importable shader paths are children of the project root path")
                .to_path_buf(),
        }
    }

    pub(crate) fn import_name(&self) -> String {
        self.relative_path.to_string_lossy().as_ref().to_owned()
    }

    pub(crate) fn try_create_composable_module(
        &self,
        shader_defs: ShaderDefs,
    ) -> Result<ComposableModuleDescriptor, ComposableModuleFailure> {
        let source = match std::fs::read_to_string(&self.absolute_path) {
            Ok(source) => source,
            Err(e) => return Err(ComposableModuleFailure::ReadFileError(e)),
        };

        // Only top level shaders can have definitions
        check_src_has_no_definitions(&source)?;

        // Imports should come before all else
        check_src_imports_well_formed(&source)?;

        return Ok(ComposableModuleDescriptor {
            source: &source,
            file_path: &self.absolute_path.to_string_lossy(),
            language: ShaderLanguage::Wgsl,
            as_name: Some(self.import_name()),
            additional_imports: &[],
            shader_defs: shader_defs.get_defs(),
        });
    }
}

/// Gives a vector of [`ShaderInProject`]s for each shader in the repository.
pub(super) fn all_shaders_in_project(project_source_root: PathBuf) -> Vec<ShaderInProject> {
    fn all_child_shaders(root: PathBuf, paths: &mut Vec<PathBuf>) {
        let read = match root.read_dir() {
            Ok(fs) => fs,
            Err(e) => panic!(
                "could not read source directory {}: {:?}",
                root.display(),
                e
            ),
        };
        let mut dirs = Vec::new();
        for file in read {
            let file = match file {
                Ok(file) => file,
                Err(e) => panic!("could not read source entry: {}", e),
            };

            let path = file.path();
            if path.is_file() && is_to_wgsl(&path) {
                paths.push(
                    std::fs::canonicalize(path).expect("shader file path canonicalize failure"),
                )
            } else if file.path().is_dir() {
                dirs.push(file.path());
            }
        }
        for dir in dirs {
            all_child_shaders(dir, paths);
        }
    }

    assert!(
        project_source_root.exists() && project_source_root.is_dir(),
        "could not find source directory when composing shader"
    );

    let mut absolute_paths = Vec::new();
    all_child_shaders(project_source_root.clone(), &mut absolute_paths);

    absolute_paths
        .into_iter()
        .map(move |absolute_path| ShaderInProject::new(project_source_root, absolute_path))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_src_has_no_definitions_errors_correctly() {
        let sources = vec![
            "#define",
            "# define",
            "   #     define",
            "\n#define",
            "\r\n\n\r\n#    define a 12",
            "#  define foo",
            "#  define   bar",
        ];
        for source in sources {
            let res = check_src_has_no_definitions(source);
            assert!(matches!(
                res,
                Err(ComposableModuleFailure::ContainedDefinitionError { .. })
            ));
        }
    }

    #[test]
    fn check_src_has_no_definitions_passes_correctly() {
        let sources = vec![
            "#\ndefine",
            "#def",
            "define",
            "#",
            "//#define",
            "/*#define*/",
            "/*\n\n    #define \n\n*/",
            "/*\n/*\n    #define foo\n*/\n*/",
            "/*\n/*\ninner comment*/\n    #define a 12\n*/",
        ];
        for source in sources {
            let res = check_src_has_no_definitions(source);
            assert!(res.is_ok());
        }
    }
}
