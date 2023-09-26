#![doc = include_str!("../README.md")]

mod error;
mod exports;
mod files;
mod imports;
mod result;
mod source;

use std::{fs::File, io::Read, path::PathBuf};

use files::AbsoluteRustFilePathBuf;
use quote::ToTokens;
use regex::Regex;
use source::Sourcecode;
use syn::token::Brace;

lazy_static::lazy_static! {
    static ref IMPORT_CUSTOM_PATH_AS_REGEX: Regex = Regex::new(r"^\s*#\s*import\s+([^\s]+)\s+as\s+([^\s]+)").unwrap();
    static ref IMPORT_CUSTOM_PATH_REGEX: Regex = Regex::new(r"^\s*#\s*import\s+([^\s]+)").unwrap();
    static ref IMPORT_ITEMS_REGEX: Regex = Regex::new(r"^\s*#\s*import\s+([^\s]+)\s+((?:[\w|\d|_]+)(?:\s*,\s*[\w|\d|_]+)*)").unwrap();
}

// Hacky polyfill for `proc_macro::Span::source_file`
fn find_me(root: &str, pattern: &str) -> Option<PathBuf> {
    let mut options = Vec::new();

    for path in glob::glob(&std::path::Path::new(root).join("**/*.rs").to_string_lossy()).unwrap() {
        if let Ok(path) = path {
            if let Ok(mut f) = File::open(&path) {
                let mut contents = String::new();
                f.read_to_string(&mut contents).ok();
                if contents.contains(pattern) {
                    options.push(path.to_owned());
                }
            }
        }
    }

    match options.as_slice() {
        [] => None,
        [v] => Some(v.clone()),
        _ => panic!(
            "found more than one contender for macro invocation location. \
            This won't be an issue once `proc_macro_span` is stabalized, \
            but until then each instance of the `include_wgsl_oil` \
            must be present in the source text, and each must have a unique argument. \
            found locations: {:?}",
            options
                .into_iter()
                .map(|path| format!("`{}`", path.display()))
                .collect::<Vec<String>>()
        ),
    }
}

/*fn any_module_identifiers_start_with(module: &naga::Module, pat: &str) -> bool {
    for (_, ty) in module.types.iter() {
        if let Some(name) = ty.name.as_ref() {
            if name.starts_with(pat) {
                return true;
            }
        }
    }
    for (_, con) in module.constants.iter() {
        if let Some(name) = con.name.as_ref() {
            if name.starts_with(pat) {
                return true;
            }
        }
    }
    for (_, func) in module.functions.iter() {
        if let Some(name) = func.name.as_ref() {
            if name.starts_with(pat) {
                return true;
            }
        }
    }
    for entry in module.entry_points.iter() {
        if entry.name.starts_with(pat) {
            return true;
        }
    }

    return false;
}*/

#[proc_macro_attribute]
pub fn include_wgsl_oil(
    path: proc_macro::TokenStream,
    module: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut module = syn::parse_macro_input!(module as syn::ItemMod);
    if let Some(content) = module.content {
        let item = syn::parse_quote_spanned! {content.0.span=>
            compile_error!(
                "`include_wgsl_oil` expects an empty module into which to inject the shader objects, \
                but found a module body - try replacing these curly braces and everything in them `{ ... }` \
                with a single semicolon `;`.");
        };

        module.semi = None;
        module.content = Some((Brace::default(), vec![item]));
        return module.to_token_stream().into();
    }

    let requested_path = syn::parse_macro_input!(path as syn::LitStr);
    let requested_path = requested_path.value();

    let root = std::env::var("CARGO_MANIFEST_DIR").expect("proc macros should be run using cargo");
    let invocation_path = match find_me(&root, &format!("\"{}\"", requested_path)) {
        Some(invocation_path) => AbsoluteRustFilePathBuf::new(invocation_path),
        None => {
            panic!(
                "could not find invocation point - maybe it was in a macro? This won't be an issue once \
                `proc_macro_span` is stabalized, but until then each instance of the `include_wgsl_oil` \
                must be present in the source text, and each must have a unique argument."
            )
        }
    };

    let sourcecode = Sourcecode::new(invocation_path, requested_path);

    let mut result = sourcecode.complete();

    result.validate();

    // Inject items
    module.semi = None;
    module.content = Some((Brace::default(), result.to_items()));

    return module.to_token_stream().into();
}
