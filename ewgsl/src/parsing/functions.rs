use std::ops::Range;

use crate::{
    arena::Handle,
    spans::{self, Spanned, WithSpan},
};

use super::{
    attributes::Attribute,
    ident::{Ident, TemplatedIdent}, statements::Statement,
};

#[cfg(feature = "eq")]
use crate::EqIn;

#[derive(Debug)]
pub struct Parameter<'a, S: spans::SpanState = spans::SpansPresent> {
    pub attributes: Range<Handle<Attribute<'a, S>>>,
    pub ident: WithSpan<Ident<'a>, S>,
    pub ty: TemplatedIdent<'a, S>,
}

impl<'a> Spanned for Parameter<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = Parameter<'a, spans::SpansErased>;

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
impl<'a, S: spans::SpanState> EqIn<'a> for Parameter<'a, S> {
    type Context<'b> = (&'b crate::arena::Arena<Attribute<'a, S>, S>, &'b crate::arena::Arena<super::expression::Expression<'a, S>, S>)
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

        if !self.ty.eq_in(own_context.1, &other.ty, other_context.1) {
            return false;
        }

        if !Attribute::are_sets_eq_in(
            self.attributes,
            own_context.0,
            own_context.1,
            other.attributes,
            other_context.0,
            other_context.1,
        ) {
            return false;
        }

        return true;
    }
}

#[derive(Debug)]
pub struct FunctionResult<'a, S: spans::SpanState = spans::SpansPresent> {
    pub attributes: Range<Handle<Attribute<'a, S>>>,
    pub ty: TemplatedIdent<'a, S>,
}

impl<'a> Spanned for FunctionResult<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = FunctionResult<'a, spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        FunctionResult {
            attributes: self.attributes.erase_spans(),
            ty: self.ty.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: spans::SpanState> EqIn<'a> for FunctionResult<'a, S> {
    type Context<'b> = (&'b crate::arena::Arena<Attribute<'a, S>, S>, &'b crate::arena::Arena<super::expression::Expression<'a, S>, S>)
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if !self.ty.eq_in(own_context.1, &other.ty, other_context.1) {
            return false;
        }

        if !Attribute::are_sets_eq_in(
            self.attributes,
            own_context.0,
            own_context.1,
            other.attributes,
            other_context.0,
            other_context.1,
        ) {
            return false;
        }

        return true;
    }
}

#[derive(Debug)]
pub struct FunctionHeader<'a, S: spans::SpanState = spans::SpansPresent> {
    pub ident: WithSpan<Ident<'a>, S>,
    pub parameters: Range<Handle<Parameter<'a, S>>>,
    pub result: Option<WithSpan<FunctionResult<'a, S>, S>>,
}

impl<'a> Spanned for FunctionHeader<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = FunctionHeader<'a, spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        FunctionHeader {
            ident: self.ident.erase_spans(),
            parameters: self.parameters.erase_spans(),
            result: self
                .result
                .erase_spans()
                .map(|inner| inner.erase_inner_spans()),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: spans::SpanState> EqIn<'a> for FunctionHeader<'a, S> {
    type Context<'b> = (&'b crate::arena::Arena<Attribute<'a, S>, S>, &'b crate::arena::Arena<super::expression::Expression<'a, S>, S>, &'b crate::arena::Arena<Parameter<'a, S>, S>)
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

        if !self.result.eq_in(
            &(own_context.0, own_context.1),
            &other.result,
            &(other_context.0, other_context.1),
        ) {
            return false;
        }

        let lhs_paramenters = &own_context.2[self.parameters];
        let rhs_paramenters = &other_context.2[other.parameters];
        if lhs_paramenters.len() != rhs_paramenters.len() {
            return false;
        }
        for (lhs, rhs) in lhs_paramenters.into_iter().zip(rhs_paramenters) {
            if !lhs.eq_in(
                &(own_context.0, own_context.1),
                rhs,
                &(other_context.0, other_context.1),
            ) {
                return false;
            }
        }

        return true;
    }
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a, S: spans::SpanState = spans::SpansPresent> {
    pub attributes: Range<Handle<Attribute<'a, S>>>,
    pub header: WithSpan<FunctionHeader<'a, S>, S>,
    pub body: Range<Handle<Statement<'a, S>>>,
}

impl<'a> Spanned for FunctionDeclaration<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = FunctionDeclaration<'a, spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        FunctionDeclaration {
            attributes: self.attributes.erase_spans(),
            header: self.header.erase_spans().erase_inner_spans(),
            body: self.body.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: spans::SpanState> EqIn<'a> for FunctionDeclaration<'a, S> {
    type Context<'b> = (
        &'b crate::arena::Arena<Attribute<'a, S>, S>, 
        &'b crate::arena::Arena<super::expression::Expression<'a, S>, S>, 
        &'b crate::arena::Arena<Parameter<'a, S>, S>, 
        &'b crate::arena::Arena<super::statements::Statement<'a, S>, S>, 
        &'b crate::arena::Arena<super::statements::IfClause<'a, S>, S>
    )
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if !self.header.eq_in(
            &(own_context.0, own_context.1, own_context.2),
            &other.header,
            &(other_context.0, other_context.1, other_context.2),
        ) {
            return false;
        }

        if !Attribute::are_sets_eq_in(
            self.attributes,
            own_context.0,
            own_context.1,
            other.attributes,
            other_context.0,
            other_context.1,
        ) {
            return false;
        }

        // Exactly equal statements, in order.
        let our_body_context = &(own_context.0, own_context.1, own_context.3, own_context.4);
        let other_body_context = &(own_context.0, own_context.1, own_context.3, own_context.4);
        if !self.body.eq_in(our_body_context, &other.body, other_body_context) {
            return false;
        }

        return true;
    }
}
