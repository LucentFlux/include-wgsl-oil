#![doc = include_str!("../README.md")]

mod result;
mod source;

use std::{fs::File, io::Read, path::PathBuf};

use quote::ToTokens;
use source::Sourcecode;

// Hacky polyfill for `proc_macro::Span::source_file`
fn find_me(root: &str, pattern: &str) -> Option<PathBuf> {
    for path in glob::glob(&std::path::Path::new(root).join("**/*.rs").to_string_lossy()).unwrap() {
        if let Ok(path) = path {
            if let Ok(mut f) = File::open(&path) {
                let mut contents = String::new();
                f.read_to_string(&mut contents).ok();
                if contents.contains(pattern) {
                    return Some(path.to_owned());
                }
            }
        }
    }
    None
}

#[proc_macro]
pub fn include_wgsl_oil(path: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let requested_path = syn::parse_macro_input!(path as syn::LitStr);
    let requested_path = requested_path.value();

    let root = std::env::var("CARGO_MANIFEST_DIR").expect("proc macros should be run using cargo");
    let invocation_path = match find_me(&root, &format!("\"{}\"", requested_path)) {
        Some(invocation_path) => invocation_path,
        None => panic!("could not find invocation point - maybe it was in a macro?"),
    };

    let sourcecode = Sourcecode::new(invocation_path, requested_path);

    let mut result = sourcecode.complete();

    result.validate();

    let mut tokens = proc_macro2::TokenStream::new();
    for item in result.to_items() {
        item.to_tokens(&mut tokens);
    }
    tokens.into()
}
