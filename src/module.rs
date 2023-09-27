use std::{borrow::Cow, collections::HashMap, fmt::Display, path::PathBuf};

use naga_oil::compose::{
    ComposableModuleDescriptor, NagaModuleDescriptor, ShaderDefValue, ShaderLanguage,
};

use crate::{
    exports,
    files::{AbsoluteRustRootPathBuf, AbsoluteWGSLFilePathBuf},
    imports,
};

pub(crate) struct OwnedComposableModuleDescriptor {
    source: String,
    file_path: String,
    as_name: String,
    shader_defs: HashMap<String, ShaderDefValue>,
}

impl OwnedComposableModuleDescriptor {
    pub(crate) fn borrow_composable_descriptor<'a>(&'a self) -> ComposableModuleDescriptor<'a> {
        ComposableModuleDescriptor {
            source: &self.source,
            file_path: &self.file_path,
            language: ShaderLanguage::Wgsl,
            as_name: Some(self.as_name.clone()),
            additional_imports: &[],
            shader_defs: self.shader_defs.clone(),
        }
    }
}

pub(crate) struct OwnedNagaModuleDescriptor {
    source: String,
    file_path: String,
    shader_defs: HashMap<String, ShaderDefValue>,
}

impl OwnedNagaModuleDescriptor {
    pub(crate) fn borrow_module_descriptor<'a>(&'a self) -> NagaModuleDescriptor<'a> {
        NagaModuleDescriptor {
            source: &self.source,
            file_path: &self.file_path,
            additional_imports: &[],
            shader_defs: self.shader_defs.clone(),
            shader_type: naga_oil::compose::ShaderType::Wgsl,
        }
    }
}

/// A single requested import to a shader.
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub(crate) struct Module {
    path: AbsoluteWGSLFilePathBuf,
}

impl Module {
    /// Given a path to a file and the string given to describe an import, tries to resolve the requested import file.
    pub(crate) fn from_path(path: AbsoluteWGSLFilePathBuf) -> Self {
        Self { path }
    }

    /// Given a path to a file and the string given to describe an import, tries to resolve the requested import file.
    pub(crate) fn resolve_module(
        importing: &Module,
        source_root: Option<&AbsoluteRustRootPathBuf>,
        request_string: &str,
    ) -> Result<Self, Vec<PathBuf>> {
        let mut tried_paths = Vec::new();

        // Try interpret as relative to importing file
        let parent = importing
            .path
            .parent()
            .expect("every absolute path to a file has a parent");
        let relative = parent.join(request_string);
        tried_paths.push(relative.clone());
        if relative.is_file() {
            let path = relative.canonicalize().unwrap();
            return Ok(Self {
                path: AbsoluteWGSLFilePathBuf::new(path),
            });
        }

        // Try interpret as relative to source root
        if let Some(source_root) = source_root {
            let relative = source_root.join(request_string);
            tried_paths.push(relative.clone());
            if relative.is_file() {
                let path = relative.canonicalize().unwrap();
                return Ok(Self {
                    path: AbsoluteWGSLFilePathBuf::new(path),
                });
            }
        }

        return Err(tried_paths);
    }

    pub(crate) fn to_composable_module_descriptor(
        &self,
        module_names: &HashMap<Module, String>,
        source_root: Option<&AbsoluteRustRootPathBuf>,
        definitions: HashMap<String, ShaderDefValue>,
    ) -> Result<OwnedComposableModuleDescriptor, Vec<String>> {
        let source = self.read_to_string();

        if source.contains("#define") {
            return Err(vec![format!(
                "imported shader file `{}` contained a `#define` statement \
                - only top-level files may contain preprocessor definitions",
                self.path.display()
            )]);
        }

        // Replace `@export` directives with equivalent whitespace
        let (source, _) = exports::strip_exports(&source);

        // Replace `#import` names with substitutions
        let source = imports::replace_imports_in_source(&source, &self, source_root, module_names);

        let name = &module_names[&self];
        Ok(OwnedComposableModuleDescriptor {
            source,
            file_path: self.path.to_string_lossy().to_string(),
            as_name: name.clone(),
            shader_defs: definitions,
        })
    }

    pub(crate) fn to_naga_module_descriptor(
        &self,
        module_names: &HashMap<Module, String>,
        source_root: Option<&AbsoluteRustRootPathBuf>,
        definitions: HashMap<String, ShaderDefValue>,
    ) -> Result<OwnedNagaModuleDescriptor, Vec<String>> {
        let source = self.read_to_string();

        // Replace `@export` directives with equivalent whitespace
        let (source, _) = exports::strip_exports(&source);

        // Replace `#import` names with substitutions
        let source = imports::replace_imports_in_source(&source, &self, source_root, module_names);

        Ok(OwnedNagaModuleDescriptor {
            source,
            file_path: self.path.to_string_lossy().to_string(),
            shader_defs: definitions,
        })
    }

    pub(crate) fn path(&self) -> AbsoluteWGSLFilePathBuf {
        self.path.clone()
    }

    pub(crate) fn read_to_string(&self) -> String {
        std::fs::read_to_string(&*self.path).expect(&format!(
            "file `{}` exists but could not be read",
            self.path.display()
        ))
    }

    /// Gets the name of the file, without the `.wgsl` extension.
    pub(crate) fn file_name(&self) -> String {
        let name = self.path.file_name().unwrap().to_string_lossy();
        assert!(name.ends_with(".wgsl"));
        return name[..(name.len() - 4)].to_owned();
    }

    pub(crate) fn nth_path_component<'a>(&'a self, i: usize) -> Option<Cow<'a, str>> {
        Some(
            self.path
                .components()
                .rev()
                .skip(i)
                .next()?
                .as_os_str()
                .to_string_lossy(),
        )
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path.display())
    }
}
