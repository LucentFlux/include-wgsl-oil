#![doc = include_str!("../README.md")]
#![cfg_attr(feature = "unstable", feature(proc_macro_span))]

mod result;
mod source;

use std::{fs::File, io::Read, path::PathBuf};

use quote::ToTokens;
use source::Sourcecode;
use syn::{token::Brace, LitStr};

// If we're on nightly, use `proc_macro::Span::source_file`, else use a hacky polyfill.
#[cfg(feature = "unstable")]
fn find_me(root: &str, pattern: &LitStr) -> Option<PathBuf> {
    return Some(pattern.span().source_file().path());
}
#[cfg(not(feature = "unstable"))]
fn find_me(root: &str, pattern: &LitStr) -> Option<PathBuf> {
    let pattern = format!("include_wgsl_oil(\"{}\")]", pattern.value());

    // Probably best to panic if we find multiple matches
    let mut found_locations = Vec::new();
    for path in glob::glob(&std::path::Path::new(root).join("**/*.rs").to_string_lossy()).unwrap() {
        if let Ok(path) = path {
            if let Ok(mut f) = File::open(&path) {
                let mut contents = String::new();
                f.read_to_string(&mut contents).ok();
                if contents.contains(&pattern) {
                    found_locations.push(path.to_owned());
                }
            }
        }
    }
    if found_locations.len() >= 2 {
        panic!(
            "found more than one location that the macro could have been called from; \
        this is because the `proc_macro_span` feature isn't stable yet, and as a hacky replacement \
        this macro searches for exactly the macro invocation in all of your source files to be \
        able to resolve the relative top level shader path - either rename your shaders to be \
        unique (if they aren't), remove duplicate calls to `include_wgsl_oil`, or use a nightly \
        compiler and enable the `unstable` feature flag on the `include-wgsl-oil` crate"
        )
    }
    return None;
}

fn any_module_identifiers_start_with(module: &naga::Module, pat: &str) -> bool {
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
}

#[proc_macro_attribute]
pub fn include_wgsl_oil(
    module: proc_macro::TokenStream,
    path: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // Get the module we were provided to place the shader information into.
    let module = syn::parse_macro_input!(path as syn::ItemMod);
    if let Some(content) = module.content {
        return quote::quote_spanned! {content.0.span=>
            compile_error!("expected empty module to place shader information in - try removing the braces and inserting a semicolon");
        }.into();
    }
    assert!(module.semi.is_some());
    let semicolon_span = module.semi.unwrap().span;

    // Get the location of the shader file to parse and populate the module with.
    let requested_path = syn::parse_macro_input!(path as syn::LitStr);
    let requested_path = requested_path;

    let root = std::env::var("CARGO_MANIFEST_DIR").expect("proc macros should be run using cargo");
    let invocation_path = match find_me(&root, &requested_path) {
        Some(invocation_path) => invocation_path,
        None => panic!(
            "could not find invocation point - either move the `include_wgsl_oil` attribute \
        such that it is not within another macro, or use a nightly compiler and enable \
        the `unstable` feature flag on the `include-wgsl-oil` crate"
        ),
    };

    let sourcecode = Sourcecode::new(invocation_path, requested_path.value());

    let mut result = sourcecode.complete();

    result.validate();

    // Inject content into module
    let mut module = module;
    module.semi = None;
    let mut brace = Brace::default();
    brace.span = semicolon_span;
    let mut content = proc_macro2::TokenStream::new();
    module.content = Some((brace, result.to_items()));

    module.to_token_stream().into()
}
