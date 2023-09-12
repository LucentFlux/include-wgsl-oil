use std::{ops::Range, str::FromStr};
use strum::VariantNames;

use crate::join_into_readable_list;

#[derive(Debug, Clone, PartialEq, Eq, Hash, strum::EnumDiscriminants)]
#[strum_discriminants(derive(
    PartialOrd,
    Ord,
    Hash,
    strum::EnumString,
    strum::EnumVariantNames,
    strum::IntoStaticStr,
))]
#[strum_discriminants(name(AttributeIdentifier))]
pub enum Attribute {
    #[strum_discriminants(strum(serialize = "align"))]
    Align,
    #[strum_discriminants(strum(serialize = "binding"))]
    Binding,
    #[strum_discriminants(strum(serialize = "builtin"))]
    Builtin,
    #[strum_discriminants(strum(serialize = "const"))]
    Const,
    #[strum_discriminants(strum(serialize = "diagnostic"))]
    Diagnostic,
    #[strum_discriminants(strum(serialize = "group"))]
    Group,
    #[strum_discriminants(strum(serialize = "id"))]
    Id,
    #[strum_discriminants(strum(serialize = "interpolate"))]
    Interpolate,
    #[strum_discriminants(strum(serialize = "invariant"))]
    Invariant,
    #[strum_discriminants(strum(serialize = "location"))]
    Location,
    #[strum_discriminants(strum(serialize = "must_use"))]
    MustUse,
    #[strum_discriminants(strum(serialize = "size"))]
    Size,
    #[strum_discriminants(strum(serialize = "workgroup_size"))]
    WorkgroupSize,
    #[strum_discriminants(strum(serialize = "vertex"))]
    Vertex,
    #[strum_discriminants(strum(serialize = "fragment"))]
    Fragment,
    #[strum_discriminants(strum(serialize = "compute"))]
    Compute,
}

lazy_static::lazy_static! {
    static ref ATTRIBUTE_IDENTIFIERS: String = join_into_readable_list(AttributeIdentifier::VARIANTS);
}

impl AttributeIdentifier {
    pub fn parse(name: &str) -> Option<Self> {
        Self::from_str(name).ok()
    }

    pub fn encode(self) -> &'static str {
        <&'static str>::from(self)
    }

    /// Finds a keyword with small edit distance to the given word, to help with diagnostics
    pub fn get_recommended_alternative(incorrect: &str) -> Option<&'static str> {
        crate::get_recommended_alternative(incorrect, AttributeIdentifier::VARIANTS)
    }

    /// Gets a human-readable list of all possible values that could be fed in to `parse` to get an attribute identifier.
    pub fn list_possible() -> &'static str {
        &ATTRIBUTE_IDENTIFIERS
    }

    pub(crate) fn argument_count(&self) -> Range<usize> {
        let range_inclusive = match self {
            AttributeIdentifier::Align => 1..=1,
            AttributeIdentifier::Binding => 1..=1,
            AttributeIdentifier::Builtin => 1..=1,
            AttributeIdentifier::Const => 0..=0,
            AttributeIdentifier::Diagnostic => 1..=1,
            AttributeIdentifier::Group => 1..=1,
            AttributeIdentifier::Id => 1..=1,
            AttributeIdentifier::Interpolate => 1..=2,
            AttributeIdentifier::Invariant => 0..=0,
            AttributeIdentifier::Location => 1..=1,
            AttributeIdentifier::MustUse => 0..=0,
            AttributeIdentifier::Size => 1..=1,
            AttributeIdentifier::WorkgroupSize => 1..=3,
            AttributeIdentifier::Vertex => 0..=0,
            AttributeIdentifier::Fragment => 0..=0,
            AttributeIdentifier::Compute => 0..=0,
        };
        *range_inclusive.start()..*range_inclusive.end() + 1
    }
}
