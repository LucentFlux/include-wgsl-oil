use std::{collections::HashSet, str::FromStr};
use strum::VariantNames;

use crate::join_into_readable_list;

use super::spans;

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
#[derive(Debug, Clone, Hash)]
pub struct Directives<S: spans::SpanPair = spans::WithSpans> {
    pub diagnostics: Vec<<S as spans::SpanPair>::Spanned<SeverityControlName>>,
    pub enable_extensions: Vec<<S as spans::SpanPair>::Spanned<EnableExtensionName>>,
    pub software_extensions: Vec<<S as spans::SpanPair>::Spanned<SoftwareExtensionName>>,
}

impl<S: spans::SpanPair> Directives<S> {
    pub fn empty() -> Self {
        Self {
            diagnostics: vec![],
            enable_extensions: vec![],
            software_extensions: vec![],
        }
    }
}

impl Directives<spans::WithSpans> {
    /// Remove all of the span information from these directives. Useful when testing semantic equivalence
    /// of objects:
    ///
    /// ```rust
    /// # use ewgsl::parsing::ParsedModule;
    ///
    /// let mod1 = ParsedModule::parse("diagnostic off;")
    /// let mod2 = ParsedModule::parse("   diagnostic     off   ; ")
    ///
    /// assert!(mod1 != mod2);
    /// assert!(mod1.erase_spans() == mod2.erase_spans());
    /// ```
    pub fn erase_spans(self) -> Directives<spans::WithoutSpans> {
        Directives {
            diagnostics: self
                .diagnostics
                .into_iter()
                .map(spans::WithSpan::erase_span)
                .collect(),
            enable_extensions: self
                .enable_extensions
                .into_iter()
                .map(spans::WithSpan::erase_span)
                .collect(),
            software_extensions: self
                .software_extensions
                .into_iter()
                .map(spans::WithSpan::erase_span)
                .collect(),
        }
    }
}

impl<S: spans::SpanPair> PartialEq for Directives<S> {
    fn eq(&self, other: &Self) -> bool {
        fn vecs_contain_same<T: Clone + std::hash::Hash + Eq>(v1: &Vec<T>, v2: &Vec<T>) -> bool {
            v1.clone().into_iter().collect::<HashSet<_>>()
                == v2.clone().into_iter().collect::<HashSet<_>>()
        }
        vecs_contain_same(&self.diagnostics, &other.diagnostics)
            && vecs_contain_same(&self.enable_extensions, &other.enable_extensions)
            && vecs_contain_same(&self.software_extensions, &other.software_extensions)
    }
}
impl<S: spans::SpanPair> Eq for Directives<S> {}

impl<S: spans::SpanPair> Default for Directives<S> {
    fn default() -> Self {
        Self::empty()
    }
}
