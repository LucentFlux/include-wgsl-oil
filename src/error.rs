use naga_oil::compose::{Composer, ComposerError, ComposerErrorInner};

pub(crate) fn format_compose_error(e: ComposerError, composer: &Composer) -> String {
    let source = match &e.source {
        naga_oil::compose::ErrSource::Module(name, _) => composer
            .module_sets
            .get(name)
            .expect(&format!(
                "while handling error could not find module {}: {:?}",
                name, e
            ))
            .substituted_source
            .clone(),
        naga_oil::compose::ErrSource::Constructing {
            source,
            path: _,
            offset,
        } => vec![' '; *offset]
            .into_iter()
            .chain(source.chars())
            .collect(),
    };

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
