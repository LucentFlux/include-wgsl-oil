use std::{
    collections::{HashMap, HashSet},
    ffi::OsStr,
};

use naga_oil::compose::{Composer, NagaModuleDescriptor};

use crate::{
    exports::{strip_exports, Export},
    files::{AbsoluteRustFilePathBuf, AbsoluteRustRootPathBuf, AbsoluteWGSLFilePathBuf},
    imports::ImportOrder,
    result::ShaderResult,
};

/// Shader sourcecode generated from the token stream provided
pub(crate) struct Sourcecode {
    exports: HashSet<Export>,
    requested_path_input: String,
    source_path: AbsoluteWGSLFilePathBuf,
    invocation_path: AbsoluteRustFilePathBuf,
    project_root: Option<AbsoluteRustRootPathBuf>,
    errors: Vec<String>,
    dependents: Vec<AbsoluteWGSLFilePathBuf>,
}

impl Sourcecode {
    pub(crate) fn new(
        invocation_path: AbsoluteRustFilePathBuf,
        requested_path_input: String,
    ) -> Self {
        // Interpret as relative to invoking file
        let source_path = invocation_path
            .parent()
            .expect("files have parent directories")
            .join(&requested_path_input);
        if !source_path.is_file() {
            if source_path.exists() {
                panic!(
                    "could not find import `{}`: `{}` exists but is not a file",
                    requested_path_input,
                    source_path.display()
                )
            }
            panic!(
                "could not find import `{}`: `{}` does not exist",
                requested_path_input,
                source_path.display()
            );
        }
        assert!(source_path.is_absolute());

        if source_path.extension() != Some(OsStr::new("wgsl")) {
            panic!(
                "file `{}` does not have the required `.wgsl` extension",
                requested_path_input,
            );
        };

        let source_path = AbsoluteWGSLFilePathBuf::new(source_path);

        // Calculate top level exports
        let root_src = std::fs::read_to_string(&*source_path).expect("asserted was file");
        let (_, exports) = strip_exports(&root_src);

        let project_root = invocation_path.get_source_rust_root();
        Self {
            requested_path_input,
            source_path,
            invocation_path,
            project_root,
            exports,
            errors: Vec::new(),
            dependents: Vec::new(),
        }
    }

    /// Traverses the imports in each file, starting with the file given by this object, to give all of the files required
    /// and the order in which they need to be processed.
    fn find_import_order(&mut self) -> Option<ImportOrder> {
        match ImportOrder::calculate(self.source_path.clone(), self.project_root.as_ref()) {
            Ok(order) => Some(order),
            Err(err) => {
                self.push_error(format!("{}", err));
                None
            }
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

        // Calculate import order
        let import_order = self.find_import_order()?;

        // Calculate names of imports
        let reduced_names = import_order.reduced_names();

        // Add imports in order to naga-oil
        for import in import_order.iter() {
            let res =
                match import.to_composable_module_descriptor(&reduced_names, shader_defs.clone()) {
                    Ok(desc) => composer.add_composable_module(desc.borrow_composable_descriptor()),
                    Err(errors) => {
                        for error in errors {
                            self.push_error(error);
                        }
                        continue;
                    }
                };

            self.dependents.push(import.path());

            if let Err(e) = res {
                self.push_error(crate::error::format_compose_error(e, &composer))
            }
        }

        let root_src =
            std::fs::read_to_string(&*self.source_path).expect("asserted was file in constructor");
        let res = composer.make_naga_module(NagaModuleDescriptor {
            source: &root_src,
            file_path: &self.requested_path_input,
            shader_type: naga_oil::compose::ShaderType::Wgsl,
            shader_defs,
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

    pub(crate) fn dependents(&self) -> impl Iterator<Item = &AbsoluteWGSLFilePathBuf> {
        self.dependents.iter()
    }

    pub(crate) fn requested_path(&self) -> &str {
        &self.requested_path_input
    }

    pub(crate) fn invocation_path(&self) -> &AbsoluteRustFilePathBuf {
        &self.invocation_path
    }

    pub(crate) fn exports(&self) -> &HashSet<Export> {
        &self.exports
    }
}
