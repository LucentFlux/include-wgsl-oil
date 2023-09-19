use std::ops::Range;

use crate::{
    arena::{Arena, Handle},
    spans::{self, Spanned, WithSpan},
    EqIn,
};

use super::{
    attributes::Attribute, expression::Expression, ident::Ident, variables::TypeSpecifier,
};

#[derive(Debug)]
pub struct StructMember<'a, S: spans::SpanState = spans::SpansPresent> {
    pub attributes: Range<Handle<Attribute<'a, S>>>,
    pub ident: WithSpan<Ident<'a>, S>,
    pub ty: TypeSpecifier<'a, S>,
}

impl<'a> Spanned for StructMember<'a> {
    type Spanless = StructMember<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        StructMember {
            attributes: self.attributes.erase_spans(),
            ident: self.ident.erase_spans(),
            ty: self.ty.erase_spans(),
        }
    }
}

impl<'a, S: spans::SpanState> EqIn<'a> for StructMember<'a, S> {
    type Context<'b> = (&'b Arena<Attribute<'a, S>, S>, &'b Arena<Expression<'a, S>, S>)
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

        if !Attribute::are_sets_eq_in(
            self.attributes.clone(),
            own_context.0,
            own_context.1,
            other.attributes.clone(),
            other_context.0,
            other_context.1,
        ) {
            return false;
        }

        if !self.ty.eq_in(own_context.1, &other.ty, other_context.1) {
            return false;
        }

        return true;
    }
}

#[derive(Debug)]
pub struct StructDeclaration<'a, S: spans::SpanState = spans::SpansPresent> {
    pub ident: WithSpan<Ident<'a>, S>,
    pub members: WithSpan<Range<Handle<StructMember<'a, S>>>, S>,
}

impl<'a> Spanned for StructDeclaration<'a> {
    type Spanless = StructDeclaration<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        StructDeclaration {
            ident: self.ident.erase_spans(),
            members: self.members.erase_spans().map(Spanned::erase_spans),
        }
    }
}

impl<'a, S: spans::SpanState> EqIn<'a> for StructDeclaration<'a, S> {
    type Context<'b> = (&'b Arena<Attribute<'a, S>, S>, &'b Arena<Expression<'a, S>, S>, &'b Arena<StructMember<'a, S>, S>)
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

        // Order of members matters
        let lhs_members = &own_context.2[self.members.inner().clone()];
        let rhs_members = &other_context.2[other.members.inner().clone()];
        if lhs_members.len() != rhs_members.len() {
            return false;
        }
        for (lhs, rhs) in lhs_members.into_iter().zip(rhs_members) {
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
