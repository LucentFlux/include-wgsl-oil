use perfect_derive::perfect_derive;
use std::str::FromStr;
use strum::VariantNames;

use crate::{arena::Arena, join_into_readable_list};

use super::text_spans::{self, Spanned, SpannedLeaf};

#[derive(
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Clone,
    Copy,
    Hash,
    strum::EnumString,
    strum::EnumVariantNames,
    strum::IntoStaticStr,
)]
pub enum SeverityControlName {
    #[strum(serialize = "error")]
    Error,
    #[strum(serialize = "warning")]
    Warning,
    #[strum(serialize = "info")]
    Info,
    #[strum(serialize = "off")]
    Off,
}

lazy_static::lazy_static! {
    static ref SEVERITY_NAMES: String = join_into_readable_list(SeverityControlName::VARIANTS);
}

impl SeverityControlName {
    pub fn parse(name: &str) -> Option<Self> {
        Self::from_str(name).ok()
    }

    pub fn encode(self) -> &'static str {
        <&'static str>::from(self)
    }

    /// Finds a keyword with small edit distance to the given word, to help with diagnostics
    pub fn get_recommended_alternative(incorrect: &str) -> Option<&'static str> {
        crate::get_recommended_alternative(incorrect, SeverityControlName::VARIANTS)
    }

    /// Gets a human-readable list of all possible values that could be fed in to `parse` to get a severity.
    pub fn list_possible() -> &'static str {
        &SEVERITY_NAMES
    }
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Clone,
    Copy,
    Hash,
    strum::EnumString,
    strum::EnumVariantNames,
    strum::IntoStaticStr,
)]
pub enum EnableExtensionName {
    /// 16-bit floating-point values
    #[strum(serialize = "f16")]
    F16,
}

lazy_static::lazy_static! {
    static ref ENABLE_EXTENSION_NAMES: String = join_into_readable_list(EnableExtensionName::VARIANTS);
}

impl EnableExtensionName {
    pub fn parse(name: &str) -> Option<Self> {
        Self::from_str(name).ok()
    }

    pub fn encode(self) -> &'static str {
        <&'static str>::from(self)
    }

    /// Finds a keyword with small edit distance to the given word, to help with diagnostics
    pub fn get_recommended_alternative(incorrect: &str) -> Option<&'static str> {
        crate::get_recommended_alternative(incorrect, EnableExtensionName::VARIANTS)
    }

    /// Gets a human-readable list of all possible values that could be fed in to `parse` to get an enable extension.
    pub fn list_possible() -> &'static str {
        &ENABLE_EXTENSION_NAMES
    }
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Clone,
    Copy,
    Hash,
    strum::EnumString,
    strum::EnumVariantNames,
    strum::IntoStaticStr,
)]
pub enum SoftwareExtensionName {
    // None yet
}

lazy_static::lazy_static! {
    static ref SOFTWARE_EXTENSION_NAMES: String = join_into_readable_list(SoftwareExtensionName::VARIANTS);
}

impl SoftwareExtensionName {
    pub fn parse(name: &str) -> Option<Self> {
        Self::from_str(name).ok()
    }

    pub fn encode(self) -> &'static str {
        <&'static str>::from(self)
    }

    /// Finds a keyword with small edit distance to the given word, to help with diagnostics
    pub fn get_recommended_alternative(incorrect: &str) -> Option<&'static str> {
        crate::get_recommended_alternative(incorrect, SoftwareExtensionName::VARIANTS)
    }

    /// Gets a human-readable list of all possible values that could be fed in to `parse` to get a software extension.
    pub fn list_possible() -> &'static str {
        &SOFTWARE_EXTENSION_NAMES
    }
}

/// A set of directives, occuring before definitions in a module.
#[perfect_derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Directives<S: text_spans::SpanState = text_spans::SpansPresent> {
    pub diagnostics: Arena<SpannedLeaf<SeverityControlName, S>>,
    pub enable_extensions: Arena<SpannedLeaf<EnableExtensionName, S>>,
    pub software_extensions: Arena<SpannedLeaf<SoftwareExtensionName, S>>,
}

impl<S: text_spans::SpanState> Directives<S> {
    pub fn empty() -> Self {
        Self {
            diagnostics: Arena::new(),
            enable_extensions: Arena::new(),
            software_extensions: Arena::new(),
        }
    }
}

impl Spanned for Directives<text_spans::SpansPresent> {
    #[cfg(feature = "span_erasure")]
    type Spanless = Directives<text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        Directives {
            diagnostics: self.diagnostics.erase_spans(),
            enable_extensions: self.enable_extensions.erase_spans(),
            software_extensions: self.software_extensions.erase_spans(),
        }
    }
}

impl<S: text_spans::SpanState> Default for Directives<S> {
    fn default() -> Self {
        Self::empty()
    }
}
