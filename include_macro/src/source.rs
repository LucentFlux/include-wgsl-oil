mod defs;
mod sources;
// This file needs a refactor. Data dependencies aren't obvious and it's soon going to become spaghetti.
// Also, the root of the import system needs correcting for the examples, and the error system needs to become enums not strings

use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use naga_oil::compose::{
    ComposableModuleDescriptor, Composer, ComposerError, ComposerErrorInner, NagaModuleDescriptor,
};
use regex::Regex;

use crate::result::ShaderResult;

fn try_read_alternate_path(
    result: &mut std::io::Result<(String, PathBuf)>,
    alternate_path: PathBuf,
) {
    match result {
        Ok(_) => {}
        Err(_) => {
            *result = std::fs::read_to_string(&alternate_path).map(move |src| (src, alternate_path))
        }
    };
}

lazy_static::lazy_static! {
    static ref FIND_EXPORTS: Regex = Regex::new(r"@export\s+struct\s+([^\s]+)").unwrap();
}

/// Shader sourcecode generated from the token stream provided
pub(crate) struct Sourcecode {
    src: String,
    exports: HashSet<String>,
    requested_path_input: String,
    source_path: PathBuf,
    invocation_path: PathBuf,
    errors: Vec<String>,
    dependents: Vec<(String, PathBuf)>,

    /// Keep a list of the modules that the user might think they would be able to import, and the reasons they can't.
    invalid_imports: HashMap<String, String>,
}

impl Sourcecode {
    pub(crate) fn new(invocation_path: PathBuf, requested_path_input: String) -> Self {
        let requested_path = std::path::PathBuf::from(requested_path_input.clone());

        // Interpret as absolute
        let mut src =
            std::fs::read_to_string(&requested_path).map(|src| (src, requested_path.clone()));

        // Interpret as relative to invoking file
        let mut search_paths = invocation_path.clone();
        loop {
            try_read_alternate_path(&mut src, search_paths.join(&requested_path));

            search_paths = match search_paths.parent() {
                None => break,
                Some(parent) => parent.to_path_buf(),
            };
        }

        // Interpret as relative to root
        let root =
            std::env::var("CARGO_MANIFEST_DIR").expect("proc macros should be run using cargo");
        try_read_alternate_path(&mut src, std::path::Path::new(&root).join(&requested_path));

        let (src, source_path) = match src {
            Ok(src) => src,
            Err(_) => panic!("failed to find or read shader sourcecode"),
        };

        let mut exports = HashSet::new();
        let src = FIND_EXPORTS
            .replace_all(&src, |group: &regex::Captures<'_>| {
                let name = group.get(1).unwrap().as_str();
                exports.insert(name.to_owned());
                format!(" struct {} ", name)
            })
            .into_owned();

        Self {
            src,
            exports,
            requested_path_input,
            source_path,
            invocation_path,
            errors: Vec::new(),
            dependents: Vec::new(),
            invalid_imports: HashMap::new(),
        }
    }

    fn format_compose_error(&self, e: ComposerError, composer: &Composer) -> String {
        let (source, offset) = match &e.source {
            naga_oil::compose::ErrSource::Module {
                name,
                offset,
                defs: _,
            } => {
                let source = composer
                    .module_sets
                    .get(name)
                    .expect(&format!(
                        "while handling error could not find module {}: {:?}",
                        name, e
                    ))
                    .sanitized_source
                    .clone();
                (source, *offset)
            }
            naga_oil::compose::ErrSource::Constructing {
                source,
                path: _,
                offset,
            } => (source.clone(), *offset),
        };

        let source = " ".repeat(offset) + &source;

        match e.inner {
            ComposerErrorInner::WgslParseError(e) => {
                format!("wgsl parsing error: {}", e.emit_to_string(&source))
            }
            ComposerErrorInner::GlslParseError(e) => format!("glsl parsing error(s): {:?}", e),
            ComposerErrorInner::ShaderValidationError(e) => format!(
                "failed to build a valid final module: {0}",
                e.emit_to_string(&source)
            ),
            _ => format!("{}", e),
        }
    }

    /// Uses naga_oil to process includes
    fn compose(&mut self) -> Option<naga::Module> {
        let mut composer = Composer::default();
        composer.capabilities = naga::valid::Capabilities::all();
        composer.validate = true;

        let mut shader_defs = HashMap::new();
        if cfg!(debug_assertions) {
            shader_defs.insert(
                "__DEBUG".to_string(),
                naga_oil::compose::ShaderDefValue::Bool(true),
            );
        }

        for source_shader in sources::all_shaders_in_project() {
            let source = source.replace("@export", "       ");
            let res = composer.add_composable_module(ComposableModuleDescriptor {
                source: &source,
                file_path: &absolute_path.to_string_lossy(),
                language,
                as_name: Some(name.clone()),
                additional_imports: &[],
                shader_defs: shader_defs.clone(),
            });

            if let Err(e) = res {
                let err = format!(
                    "imported shader file {name} had a composer error: {}",
                    self.format_compose_error(e, &composer)
                );
                self.invalid_imports.insert(name, err);
                continue;
            }

            self.dependents.push((name, absolute_path));
        }

        let res = composer.make_naga_module(NagaModuleDescriptor {
            source: &self.src,
            file_path: &self.source_path.to_string_lossy(),
            shader_type: naga_oil::compose::ShaderType::Wgsl,
            shader_defs,
            additional_imports: &[],
        });

        match res {
            Ok(module) => Some(module),
            Err(e) => self.push_error(self.format_compose_error(e, &composer)),
        }
    }

    pub(crate) fn complete(mut self) -> ShaderResult {
        let module = self.compose().unwrap_or(naga::Module::default());

        ShaderResult::new(self, module)
    }

    pub(crate) fn push_error(&mut self, message: String) {
        self.errors.push(message)
    }

    pub(crate) fn errors(&self) -> impl Iterator<Item = &String> {
        self.errors.iter()
    }

    pub(crate) fn dependents(&self) -> impl Iterator<Item = &(String, PathBuf)> {
        self.dependents.iter()
    }

    pub(crate) fn requested_path(&self) -> &str {
        &self.requested_path_input
    }

    pub(crate) fn invocation_path(&self) -> PathBuf {
        self.invocation_path.clone()
    }

    pub(crate) fn exports(&self) -> &HashSet<String> {
        &self.exports
    }
}
