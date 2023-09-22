use std::ops::Range;

use crate::arena::{Arena, Handle};

use super::{
    attributes::{Attribute, AttributeHandle},
    ident::{Ident, TemplatedIdent},
    statements::StatementHandle,
    text_spans::{self, Spanned, SpannedParent},
    variables::TypeSpecifier,
    ParsedModule,
};

pub type ParameterArena<'a, S = text_spans::SpansPresent> =
    Arena<SpannedParent<Parameter<'a, S>, S>>;
pub type ParameterHandle<'a, S = text_spans::SpansPresent> =
    Handle<SpannedParent<Parameter<'a, S>, S>>;

#[cfg(feature = "eq")]
use crate::EqIn;

#[derive(Debug)]
pub struct Parameter<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub attributes: Range<AttributeHandle<'a, S>>,
    pub ident: Ident<'a, S>,
    pub ty: TypeSpecifier<'a, S>,
}

impl<'a> Spanned for Parameter<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = Parameter<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        Parameter {
            attributes: self.attributes.erase_spans(),
            ident: self.ident.erase_spans(),
            ty: self.ty.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for Parameter<'a, S> {
    type Context<'b> = ParsedModule<'a, S>
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if self.ident != other.ident {
            return false;
        }

        if !self.ty.eq_in(
            &own_context.expressions,
            &other.ty,
            &other_context.expressions,
        ) {
            return false;
        }

        if !Attribute::are_sets_eq_in(
            self.attributes.clone(),
            &own_context.attributes,
            &own_context.expressions,
            other.attributes.clone(),
            &other_context.attributes,
            &other_context.expressions,
        ) {
            return false;
        }

        return true;
    }
}

#[derive(Debug)]
pub struct FunctionResult<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub attributes: Range<AttributeHandle<'a, S>>,
    pub ty: TemplatedIdent<'a, S>,
}

impl<'a> Spanned for FunctionResult<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = FunctionResult<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        FunctionResult {
            attributes: self.attributes.erase_spans(),
            ty: self.ty.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for FunctionResult<'a, S> {
    type Context<'b> = ParsedModule<'a, S>
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if !self.ty.eq_in(
            &own_context.expressions,
            &other.ty,
            &other_context.expressions,
        ) {
            return false;
        }

        if !Attribute::are_sets_eq_in(
            self.attributes.clone(),
            &own_context.attributes,
            &own_context.expressions,
            other.attributes.clone(),
            &other_context.attributes,
            &other_context.expressions,
        ) {
            return false;
        }

        return true;
    }
}

#[derive(Debug)]
pub struct FunctionHeader<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub ident: Ident<'a, S>,
    pub parameters: Range<ParameterHandle<'a, S>>,
    pub result: Option<SpannedParent<FunctionResult<'a, S>, S>>,
}

impl<'a> Spanned for FunctionHeader<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = FunctionHeader<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        FunctionHeader {
            ident: self.ident.erase_spans(),
            parameters: self.parameters.erase_spans(),
            result: self.result.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for FunctionHeader<'a, S> {
    type Context<'b> = ParsedModule<'a, S>
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if self.ident != other.ident {
            return false;
        }

        if !self
            .result
            .eq_in(&own_context, &other.result, &other_context)
        {
            return false;
        }

        let lhs_paramenters = &own_context.parameters[self.parameters.clone()];
        let rhs_paramenters = &other_context.parameters[other.parameters.clone()];
        if lhs_paramenters.len() != rhs_paramenters.len() {
            return false;
        }
        for (lhs, rhs) in lhs_paramenters.into_iter().zip(rhs_paramenters) {
            if !lhs.eq_in(&own_context, rhs, &other_context) {
                return false;
            }
        }

        return true;
    }
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub attributes: Range<AttributeHandle<'a, S>>,
    pub header: SpannedParent<FunctionHeader<'a, S>, S>,
    pub body: Range<StatementHandle<'a, S>>,
}

impl<'a> Spanned for FunctionDeclaration<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = FunctionDeclaration<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        FunctionDeclaration {
            attributes: self.attributes.erase_spans(),
            header: self.header.erase_spans(),
            body: self.body.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for FunctionDeclaration<'a, S> {
    type Context<'b> = ParsedModule<'a, S>
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if !self
            .header
            .eq_in(&own_context, &other.header, &other_context)
        {
            return false;
        }

        if !Attribute::are_sets_eq_in(
            self.attributes.clone(),
            &own_context.attributes,
            &own_context.expressions,
            other.attributes.clone(),
            &other_context.attributes,
            &other_context.expressions,
        ) {
            return false;
        }

        // Exactly equal statements, in order.
        if !self.body.eq_in(own_context, &other.body, other_context) {
            return false;
        }

        return true;
    }
}
