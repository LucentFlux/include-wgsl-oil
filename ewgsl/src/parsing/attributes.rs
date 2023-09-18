use std::{mem::discriminant, ops::Range, str::FromStr};
use strum::VariantNames;

use crate::{
    arena::{Arena, Handle},
    join_into_readable_list,
    spans::{self, Spanned, WithSpan},
    EqIn,
};

use super::{expression::Expression, ParsedModule};

#[derive(Debug, Clone, strum::EnumDiscriminants)]
#[strum_discriminants(derive(
    PartialOrd,
    Ord,
    Hash,
    strum::EnumString,
    strum::EnumVariantNames,
    strum::IntoStaticStr,
))]
#[strum_discriminants(name(AttributeIdentifier))]
pub enum AttributeInner<'a, S: spans::SpanState = spans::SpansPresent> {
    #[strum_discriminants(strum(serialize = "align"))]
    Align(Handle<Expression<'a, S>>),
    #[strum_discriminants(strum(serialize = "binding"))]
    Binding(Handle<Expression<'a, S>>),
    #[strum_discriminants(strum(serialize = "builtin"))]
    Builtin(Handle<Expression<'a, S>>),
    #[strum_discriminants(strum(serialize = "const"))]
    Const,
    #[strum_discriminants(strum(serialize = "diagnostic"))]
    Diagnostic {
        severity: Handle<Expression<'a, S>>,
        trigger: Handle<Expression<'a, S>>,
    },
    #[strum_discriminants(strum(serialize = "group"))]
    Group(Handle<Expression<'a, S>>),
    #[strum_discriminants(strum(serialize = "id"))]
    Id(Handle<Expression<'a, S>>),
    #[strum_discriminants(strum(serialize = "interpolate"))]
    Interpolate {
        inv_type: Handle<Expression<'a, S>>,
        inv_sampling: Option<Handle<Expression<'a, S>>>,
    },
    #[strum_discriminants(strum(serialize = "invariant"))]
    Invariant,
    #[strum_discriminants(strum(serialize = "location"))]
    Location(Handle<Expression<'a, S>>),
    #[strum_discriminants(strum(serialize = "must_use"))]
    MustUse,
    #[strum_discriminants(strum(serialize = "size"))]
    Size(Handle<Expression<'a, S>>),
    #[strum_discriminants(strum(serialize = "workgroup_size"))]
    WorkgroupSize {
        x: Handle<Expression<'a, S>>,
        y: Option<Handle<Expression<'a, S>>>,
        z: Option<Handle<Expression<'a, S>>>,
    },
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

    pub fn argument_count(&self) -> Range<usize> {
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

impl<'a> Spanned for AttributeInner<'a> {
    type Spanless = AttributeInner<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        match self {
            AttributeInner::Align(h) => AttributeInner::Align(h.erase_spans()),
            AttributeInner::Binding(h) => AttributeInner::Binding(h.erase_spans()),
            AttributeInner::Builtin(h) => AttributeInner::Builtin(h.erase_spans()),
            AttributeInner::Const => AttributeInner::Const,
            AttributeInner::Diagnostic { severity, trigger } => AttributeInner::Diagnostic {
                severity: severity.erase_spans(),
                trigger: trigger.erase_spans(),
            },
            AttributeInner::Group(h) => AttributeInner::Group(h.erase_spans()),
            AttributeInner::Id(h) => AttributeInner::Id(h.erase_spans()),
            AttributeInner::Interpolate {
                inv_type,
                inv_sampling,
            } => AttributeInner::Interpolate {
                inv_type: inv_type.erase_spans(),
                inv_sampling: inv_sampling.map(|h| h.erase_spans()),
            },
            AttributeInner::Invariant => AttributeInner::Invariant,
            AttributeInner::Location(h) => AttributeInner::Location(h.erase_spans()),
            AttributeInner::MustUse => AttributeInner::MustUse,
            AttributeInner::Size(h) => AttributeInner::Size(h.erase_spans()),
            AttributeInner::WorkgroupSize { x, y, z } => AttributeInner::WorkgroupSize {
                x: x.erase_spans(),
                y: y.map(|y| y.erase_spans()),
                z: z.map(|z| z.erase_spans()),
            },
            AttributeInner::Vertex => AttributeInner::Vertex,
            AttributeInner::Fragment => AttributeInner::Fragment,
            AttributeInner::Compute => AttributeInner::Compute,
        }
    }
}

#[derive(Debug)]
pub struct Attribute<'a, S: spans::SpanState = spans::SpansPresent> {
    /// The span of the identifier portion of the input.
    pub identifier_span: spans::Span,
    /// The data represented by this attribute.
    pub inner: AttributeInner<'a, S>,
}

impl<'a> Attribute<'a> {
    pub fn try_parse_from(
        module: &ParsedModule<'a>,
        identifier: WithSpan<AttributeIdentifier>,
        expressions: Vec<Handle<Expression<'a>>>,
    ) -> Result<Self, Vec<WithSpan<super::ParseIssue<'a>>>> {
        let identifier_span = identifier.span();
        let identifier = identifier.unwrap();

        // Returns None if insufficient parameter expressions were given
        fn collect_expressions<'a, S: spans::SpanState>(
            identifier: AttributeIdentifier,
            expressions: &mut impl Iterator<Item = Handle<Expression<'a, S>>>,
        ) -> Option<AttributeInner<'a, S>> {
            let inner = match identifier {
                AttributeIdentifier::Align => AttributeInner::Align(expressions.next()?),
                AttributeIdentifier::Binding => AttributeInner::Binding(expressions.next()?),
                AttributeIdentifier::Builtin => AttributeInner::Builtin(expressions.next()?),
                AttributeIdentifier::Const => AttributeInner::Const,
                AttributeIdentifier::Diagnostic => AttributeInner::Diagnostic {
                    severity: expressions.next()?,
                    trigger: expressions.next()?,
                },
                AttributeIdentifier::Group => AttributeInner::Group(expressions.next()?),
                AttributeIdentifier::Id => AttributeInner::Id(expressions.next()?),
                AttributeIdentifier::Interpolate => AttributeInner::Interpolate {
                    inv_type: expressions.next()?,
                    inv_sampling: expressions.next(),
                },
                AttributeIdentifier::Invariant => AttributeInner::Invariant,
                AttributeIdentifier::Location => AttributeInner::Location(expressions.next()?),
                AttributeIdentifier::MustUse => AttributeInner::MustUse,
                AttributeIdentifier::Size => AttributeInner::Size(expressions.next()?),
                AttributeIdentifier::WorkgroupSize => AttributeInner::WorkgroupSize {
                    x: expressions.next()?,
                    y: expressions.next(),
                    z: expressions.next(),
                },
                AttributeIdentifier::Vertex => AttributeInner::Vertex,
                AttributeIdentifier::Fragment => AttributeInner::Fragment,
                AttributeIdentifier::Compute => AttributeInner::Compute,
            };
            return Some(inner);
        }

        let required_arg_count = identifier.argument_count();
        let found_arg_count = expressions.len();

        // Use as many expressions as is required for this argument
        let mut expressions = expressions.into_iter();
        let inner = collect_expressions(identifier, &mut expressions).ok_or_else(|| {
            vec![WithSpan::new(
                super::ParseIssue::InadequateAttributeArgumentCount {
                    attribute_ident: identifier,
                    minimum: required_arg_count.start,
                    found: found_arg_count,
                },
                identifier_span,
            )]
        })?;

        // Check we had none left
        let excess = expressions
            .map(|expr| {
                let inner = super::ParseIssue::ExcessiveAttributeArgumentCount {
                    attribute_ident: identifier,
                    maximum: required_arg_count.end,
                    found: found_arg_count,
                };
                let span = module
                    .expressions
                    .try_get(expr)
                    .expect("handle for active module exists")
                    .span();
                WithSpan::new(inner, span)
            })
            .collect::<Vec<_>>();
        if !excess.is_empty() {
            return Err(excess);
        }

        return Ok(Self {
            identifier_span,
            inner,
        });
    }
}

impl<'a> Spanned for Attribute<'a> {
    type Spanless = Attribute<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        Attribute {
            identifier_span: spans::Span::empty(),
            inner: self.inner.erase_spans(),
        }
    }
}

impl<'a, S: spans::SpanState> EqIn<'a> for Attribute<'a, S> {
    type Context<'b> = Arena<Expression<'a, S>, S> where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if self.identifier_span != other.identifier_span {
            return false;
        }

        match (&self.inner, &other.inner) {
            (AttributeInner::Align(lhs), AttributeInner::Align(rhs)) => {
                lhs.eq_in(own_context, rhs, other_context)
            }
            (AttributeInner::Binding(lhs), AttributeInner::Binding(rhs)) => {
                lhs.eq_in(own_context, rhs, other_context)
            }
            (AttributeInner::Builtin(lhs), AttributeInner::Builtin(rhs)) => {
                lhs.eq_in(own_context, rhs, other_context)
            }
            (
                AttributeInner::Diagnostic {
                    severity: lhs_severity,
                    trigger: lhs_trigger,
                },
                AttributeInner::Diagnostic {
                    severity: rhs_severity,
                    trigger: rhs_trigger,
                },
            ) => {
                lhs_severity.eq_in(own_context, rhs_severity, other_context)
                    && lhs_trigger.eq_in(own_context, rhs_trigger, other_context)
            }
            (AttributeInner::Group(lhs), AttributeInner::Group(rhs)) => {
                lhs.eq_in(own_context, rhs, other_context)
            }
            (AttributeInner::Id(lhs), AttributeInner::Id(rhs)) => {
                lhs.eq_in(own_context, rhs, other_context)
            }
            (
                AttributeInner::Interpolate {
                    inv_type: lhs_inv_type,
                    inv_sampling: lhs_inv_sampling,
                },
                AttributeInner::Interpolate {
                    inv_type: rhs_inv_type,
                    inv_sampling: rhs_inv_sampling,
                },
            ) => {
                lhs_inv_type.eq_in(own_context, rhs_inv_type, other_context)
                    && lhs_inv_sampling.eq_in(own_context, rhs_inv_sampling, other_context)
            }
            (AttributeInner::Location(lhs), AttributeInner::Location(rhs)) => {
                lhs.eq_in(own_context, rhs, other_context)
            }
            (AttributeInner::Size(lhs), AttributeInner::Size(rhs)) => {
                lhs.eq_in(own_context, rhs, other_context)
            }
            (
                AttributeInner::WorkgroupSize {
                    x: lhs_x,
                    y: lhs_y,
                    z: lhs_z,
                },
                AttributeInner::WorkgroupSize {
                    x: rhs_x,
                    y: rhs_y,
                    z: rhs_z,
                },
            ) => {
                lhs_x.eq_in(own_context, rhs_x, other_context)
                    && lhs_y.eq_in(own_context, rhs_y, other_context)
                    && lhs_z.eq_in(own_context, rhs_z, other_context)
            }
            (AttributeInner::Const, AttributeInner::Const)
            | (AttributeInner::Invariant, AttributeInner::Invariant)
            | (AttributeInner::MustUse, AttributeInner::MustUse)
            | (AttributeInner::Vertex, AttributeInner::Vertex)
            | (AttributeInner::Fragment, AttributeInner::Fragment)
            | (AttributeInner::Compute, AttributeInner::Compute) => true,

            (lhs, rhs) if discriminant(lhs) != discriminant(rhs) => return false,
            _ => unimplemented!(),
        }
    }
}
