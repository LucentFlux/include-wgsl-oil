use std::collections::HashSet;

use regex::Regex;

lazy_static::lazy_static! {
    static ref EXPORT_STRUCT_REGEX: Regex = Regex::new(r"@export\s+struct\s+([^\s]+)").unwrap();
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
#[non_exhaustive]
pub(crate) enum Export {
    Struct { struct_name: String },
}

/// Removes `@export` statements, replacing them with an equivalent number of spaces so as to not disrupt spans.
pub(crate) fn strip_exports(source: &str) -> (String, HashSet<Export>) {
    let mut exports = HashSet::new();

    let new_src = EXPORT_STRUCT_REGEX.replace_all(&source, |group: &regex::Captures<'_>| {
        let name = group.get(1).unwrap().as_str();
        exports.insert(Export::Struct {
            struct_name: name.to_owned(),
        });
        group.get(0).unwrap().as_str().replace("@export", "       ")
    });

    return (new_src.into_owned(), exports);
}
