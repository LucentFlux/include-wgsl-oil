use std::borrow::Cow;

use naga_oil::compose::{Composer, ComposerError, ComposerErrorInner};
use regex::{Captures, Regex};

lazy_static::lazy_static! {
    static ref UNDECORATE_REGEX: Regex = Regex::new("_naga_oil_mod_([A-Z0-9]*)_member").unwrap();
}

fn demangle_mod_names(source: &str, pad: bool) -> Cow<'_, str> {
    UNDECORATE_REGEX.replace_all(source, |capture: &Captures<'_>| {
        let module = capture.get(1).unwrap();
        let module = String::from_utf8(
            data_encoding::BASE32_NOPAD
                .decode(module.as_str().as_bytes())
                .unwrap(),
        )
        .unwrap();

        if pad {
            let original_len = capture.get(0).unwrap().len();
            format!("{:>len$}::", module, len = original_len - 2)
        } else {
            format!("{}::", module)
        }
    })
}

pub(crate) fn format_compose_error(e: ComposerError, composer: &Composer) -> String {
    let (source_name, source, offset) = match &e.source {
        naga_oil::compose::ErrSource::Module {
            name,
            offset,
            defs: _,
        } => {
            let source = composer
                .module_sets
                .get(name)
                .unwrap_or_else(|| {
                    panic!(
                        "while handling error could not find module {}: {:?}",
                        name, e
                    )
                })
                .sanitized_source
                .clone();
            (name, source, *offset)
        }
        naga_oil::compose::ErrSource::Constructing {
            source,
            path,
            offset,
        } => (path, source.clone(), *offset),
    };

    let source = " ".repeat(offset) + &source;

    match e.inner {
        ComposerErrorInner::WgslParseError(e) => {
            let wgsl_error = e.emit_to_string_with_path(&source, source_name);

            // Demangle first line that probably contains type but not in context, so no padding required
            let (first_line, other_lines) = wgsl_error.split_once('\n').unwrap();
            let first_line = demangle_mod_names(first_line, false);

            // Demangle anything else
            let other_lines = demangle_mod_names(other_lines, true);

            format!("wgsl parsing error: {}\n{}", first_line, other_lines)
        }
        ComposerErrorInner::GlslParseError(e) => format!("glsl parsing error(s): {:?}", e),
        ComposerErrorInner::ShaderValidationError(e) => format!(
            "failed to build a valid final module: {0}",
            e.emit_to_string(&source)
        ),
        _ => format!("{}", e),
    }
}
