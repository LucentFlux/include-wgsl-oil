use naga_oil::compose::{Composer, ComposerError, ComposerErrorInner};

pub(crate) fn format_compose_error(e: ComposerError, composer: &Composer) -> String {
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
