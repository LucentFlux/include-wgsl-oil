use std::ops::Range;

use crate::arena::{Arena, Handle};

use super::{
    attributes::{Attribute, AttributeHandle},
    ident::Ident,
    text_spans::{self, Spanned, SpannedParent},
    variables::TypeSpecifier,
};

pub type StructMemberArena<'a, S = text_spans::SpansPresent> =
    Arena<SpannedParent<StructMember<'a, S>, S>>;
pub type StructMemberHandle<'a, S = text_spans::SpansPresent> =
    Handle<SpannedParent<StructMember<'a, S>, S>>;

#[cfg(feature = "eq")]
use crate::EqIn;

#[derive(Debug)]
pub struct StructMember<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub attributes: Range<AttributeHandle<'a, S>>,
    pub ident: Ident<'a, S>,
    pub ty: TypeSpecifier<'a, S>,
}

impl<'a> Spanned for StructMember<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = StructMember<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        StructMember {
            attributes: self.attributes.erase_spans(),
            ident: self.ident.erase_spans(),
            ty: self.ty.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for StructMember<'a, S> {
    type Context<'b> = super::ParsedModule<'a, S>
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
            &own_context.attributes,
            &own_context.expressions,
            other.attributes.clone(),
            &other_context.attributes,
            &other_context.expressions,
        ) {
            return false;
        }

        if !self.ty.eq_in(
            &own_context.expressions,
            &other.ty,
            &other_context.expressions,
        ) {
            return false;
        }

        return true;
    }
}

#[derive(Debug)]
pub struct StructDeclaration<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub ident: Ident<'a, S>,
    pub members: SpannedParent<Range<StructMemberHandle<'a, S>>, S>,
}

impl<'a> Spanned for StructDeclaration<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = StructDeclaration<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        StructDeclaration {
            ident: self.ident.erase_spans(),
            members: self.members.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for StructDeclaration<'a, S> {
    type Context<'b> = super::ParsedModule<'a, S>
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
        let lhs_members = &own_context.members[self.members.inner().clone()];
        let rhs_members = &other_context.members[other.members.inner().clone()];
        if lhs_members.len() != rhs_members.len() {
            return false;
        }
        for (lhs, rhs) in lhs_members.into_iter().zip(rhs_members) {
            if !lhs.eq_in(&own_context, rhs, &other_context) {
                return false;
            }
        }

        return true;
    }
}
