use std::error::Error;

use naga_oil::compose::{Composer, ComposerError, ComposerErrorInner};

pub(crate) fn format_compose_error(e: ComposerError, source: &str, composer: &Composer) -> String {
    match e.inner {
        ComposerErrorInner::WgslParseError(e) => {
            format!("wgsl parsing error: {}", e.emit_to_string(source))
        }
        ComposerErrorInner::GlslParseError(e) => format!("glsl parsing error(s): {:?}", e),
        _ => format!("{}", e),
    }
}
