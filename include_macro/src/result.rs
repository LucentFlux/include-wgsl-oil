use std::{collections::HashMap, error::Error};

use naga_to_tokenstream::{ModuleToTokens, ModuleToTokensConfig};

use crate::{any_module_identifiers_start_with, source::Sourcecode};

/// The output of the transformations provided by this crate.
pub(crate) struct ShaderResult {
    source: Sourcecode,
    module: naga::Module,
}

impl ShaderResult {
    pub(crate) fn new(source: Sourcecode, module: naga::Module) -> Self {
        Self { source, module }
    }

    pub(crate) fn validate(&mut self) -> Option<naga::valid::ModuleInfo> {
        let mut validator = naga::valid::Validator::new(
            naga::valid::ValidationFlags::all(),
            naga::valid::Capabilities::all(),
        );
        match validator.validate(&self.module) {
            Ok(info) => Some(info),
            Err(e) => {
                let mut e_base: &dyn Error = e.as_inner();
                let mut message = format!("{}", e);
                let mut error_count = 1;
                while let Some(e) = e_base.source() {
                    message = format!("{}: \n{}{}", message, "    ".repeat(error_count), e);
                    e_base = e;
                    error_count += 1;
                }

                self.source.push_error(message);

                None
            }
        }
    }

    pub(crate) fn to_items(&mut self) -> Vec<syn::Item> {
        let mut items = Vec::new();

        // Errors
        for msg in self.source.errors() {
            items.push(syn::parse_quote! {
                compile_error!(#msg);
            });
        }

        // Dependencies, to re-run macro on shader change
        let origin = self
            .source
            .invocation_path()
            .parent()
            .map(|path| path.to_path_buf())
            .expect("source should have a parent directory");
        let mut dedecorated_names = HashMap::new();
        for (dependent_module_name, dependent_path) in self.source.dependents() {
            let sanitized_name =
                naga_oil::compose::Composer::decorated_name(Some(dependent_module_name), "");
            if !any_module_identifiers_start_with(&self.module, &sanitized_name) {
                continue;
            }

            dedecorated_names.insert(
                sanitized_name,
                dependent_module_name.replace("/", "_").replace(".", "_"),
            );

            let dependent = pathdiff::diff_paths(dependent_path, &origin)
                .expect("relative path should be easy");
            let dependent = dependent.to_string_lossy();
            items.push(syn::parse_quote! {
                const _: &[u8] = include_bytes!(#dependent);
            });
        }
        let source = self.source.requested_path();
        items.push(syn::parse_quote! {
            const _: &[u8] = include_bytes!(#source);
        });

        // Convert to info about the module
        let mut module_items = self.module.to_items(ModuleToTokensConfig {
            structs_filter: Some(self.source.exports().clone()),
        });
        items.append(&mut module_items);

        items
    }
}
