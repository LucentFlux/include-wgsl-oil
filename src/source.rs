use std::{collections::HashMap, ffi::OsStr, path::PathBuf};

use naga_oil::compose::{
    ComposableModuleDescriptor, Composer, NagaModuleDescriptor, ShaderLanguage,
};

use crate::result::ShaderResult;

fn get_shader_extension(path: &PathBuf) -> Option<ShaderLanguage> {
    match path.extension().and_then(OsStr::to_str) {
        None => None,
        Some(v) => match v {
            "wgsl" => Some(ShaderLanguage::Wgsl),
            "glsl" => Some(ShaderLanguage::Glsl),
            _ => None,
        },
    }
}

fn is_shader_extension(path: &PathBuf) -> bool {
    get_shader_extension(path).is_some()
}

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
        if path.is_file() && is_shader_extension(&path) {
            paths.push(std::fs::canonicalize(path).expect("shader file path canonicalize failure"))
        } else if file.path().is_dir() {
            dirs.push(file.path());
        }
    }
    for dir in dirs {
        all_child_shaders(dir, paths);
    }
}

/// Gives a vector of (Absolute Path, Relative to `src` folder Path) tuples for each shader in the repository.
fn all_shaders_in_project() -> Vec<(PathBuf, PathBuf)> {
    let root = std::env::var("CARGO_MANIFEST_DIR").expect("proc macros should be run using cargo");
    let src_root = std::path::Path::new(&root).join("src");
    let src_root =
        std::fs::canonicalize(src_root).expect("src root should exist for crate being compiled");

    assert!(
        src_root.exists() && src_root.is_dir(),
        "could not find source directory when composing shader"
    );

    let mut paths = Vec::new();
    all_child_shaders(src_root.clone(), &mut paths);

    paths
        .into_iter()
        .map(move |path| {
            let subpath = path
                .strip_prefix(&src_root)
                .expect("all child paths are children")
                .to_path_buf();

            (path, subpath)
        })
        .collect()
}

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

/// Shader sourcecode generated from the token stream provided
pub(crate) struct Sourcecode {
    src: String,
    requested_path_input: String,
    source_path: PathBuf,
    invocation_path: PathBuf,
    errors: Vec<String>,
    dependents: Vec<(String, PathBuf)>,
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

        Self {
            src,
            requested_path_input,
            source_path,
            invocation_path,
            errors: Vec::new(),
            dependents: Vec::new(),
        }
    }

    /// Uses naga_oil to process includes
    fn compose(&mut self) -> Option<naga::Module> {
        let mut composer = Composer::default();
        composer.capabilities = naga::valid::Capabilities::all();
        composer.validate = true;

        for (absolute_path, relative_path) in all_shaders_in_project() {
            let language = match get_shader_extension(&absolute_path) {
                None => continue,
                Some(language) => language,
            };

            let source = match std::fs::read_to_string(&absolute_path) {
                Ok(source) => source,
                Err(_) => continue,
            };

            if source.contains("#define") {
                continue;
            }

            let name = relative_path.to_string_lossy().as_ref().to_owned();
            let res = composer.add_composable_module(ComposableModuleDescriptor {
                source: &source,
                file_path: &absolute_path.to_string_lossy(),
                language,
                as_name: Some(name.clone()),
                additional_imports: &[],
                shader_defs: HashMap::default(),
            });

            self.dependents.push((name, absolute_path));

            if let Err(e) = res {
                self.push_error(crate::error::format_compose_error(e, &composer))
            }
        }

        let res = composer.make_naga_module(NagaModuleDescriptor {
            source: &self.src,
            file_path: &self.source_path.to_string_lossy(),
            shader_type: naga_oil::compose::ShaderType::Wgsl,
            shader_defs: HashMap::new(),
            additional_imports: &[],
        });

        match res {
            Ok(module) => Some(module),
            Err(e) => {
                self.push_error(crate::error::format_compose_error(e, &composer));

                None
            }
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
}
