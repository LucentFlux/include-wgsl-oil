#![doc = include_str!("../README.md")]

mod error;
mod exports;
mod files;
mod imports;
mod module;
mod result;
mod source;

use std::{fs::File, io::Read, path::PathBuf};

use files::AbsoluteRustFilePathBuf;
use quote::ToTokens;
use source::Sourcecode;
use syn::token::Brace;

// Hacky polyfill for `proc_macro::Span::source_file`
fn find_me(root: &str, pattern: &str) -> Option<PathBuf> {
    let mut options = Vec::new();

    for path in glob::glob(&std::path::Path::new(root).join("**/*.rs").to_string_lossy())
        .unwrap()
        .flatten()
    {
        if let Ok(mut f) = File::open(&path) {
            let mut contents = String::new();
            f.read_to_string(&mut contents).ok();
            if contents.contains(pattern) {
                options.push(path.to_owned());
            }
        }
    }

    match options.as_slice() {
        [] => None,
        [v] => Some(v.clone()),
        _ => panic!(
            "found more than one contender for macro invocation location. \
            This won't be an issue once `proc_macro_span` is stabilized, \
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

#[proc_macro_attribute]
pub fn include_wgsl_oil(
    path: proc_macro::TokenStream,
    module: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // Parse module definitions and error if it contains anything
    let mut module = syn::parse_macro_input!(module as syn::ItemMod);
    if let Some(content) = &mut module.content {
        if !content.1.is_empty() {
            let item = syn::parse_quote_spanned! {content.0.span=>
                compile_error!(
                    "`include_wgsl_oil` expects an empty module into which to inject the shader objects, \
                    but found a module body - try removing everything within the curly braces `{ ... }`.");
            };

            module.content = Some((Brace::default(), vec![item]));
        }
    } else {
        module.content = Some((Brace::default(), vec![]));
    }
    module.semi = None;

    let requested_path = syn::parse_macro_input!(path as syn::LitStr);
    let requested_path = requested_path.value();

    let root = std::env::var("CARGO_MANIFEST_DIR").expect("proc macros should be run using cargo");
    let invocation_path = match find_me(&root, &format!("\"{}\"", requested_path)) {
        Some(invocation_path) => AbsoluteRustFilePathBuf::new(invocation_path),
        None => {
            panic!(
                "could not find invocation point - maybe it was in a macro? This won't be an issue once \
                `proc_macro_span` is stabilized, but until then each instance of the `include_wgsl_oil` \
                must be present in the source text, and each must have a unique argument."
            )
        }
    };

    let sourcecode = Sourcecode::new(invocation_path, requested_path);

    let mut result = sourcecode.complete();

    result.validate();

    // Inject items
    module
        .content
        .as_mut()
        .expect("set to some at start")
        .1
        .append(&mut result.items());

    module.to_token_stream().into()
}
