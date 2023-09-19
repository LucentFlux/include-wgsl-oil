use crate::{
    arena::Arena,
    spans::{self, Spanned},
    EqIn,
};

use super::{expression::Expression, ident::Ident, variables::TypeSpecifier};

/// A type alias expression, such as `alias MyVec3 = vec3<u32>;`
#[derive(Debug)]
pub struct TypeAliasDeclaration<'a, S: spans::SpanState = spans::SpansPresent> {
    pub ident: spans::WithSpan<Ident<'a>, S>,
    pub ty: TypeSpecifier<'a, S>,
}

impl<'a> Spanned for TypeAliasDeclaration<'a, spans::SpansPresent> {
    type Spanless = TypeAliasDeclaration<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        TypeAliasDeclaration {
            ident: self.ident.erase_spans(),
            ty: self.ty.erase_spans(),
        }
    }
}

impl<'a, S: spans::SpanState> EqIn<'a> for TypeAliasDeclaration<'a, S> {
    type Context<'b> = Arena<Expression<'a, S>, S>
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

        if !self.ty.eq_in(own_context, &other.ty, other_context) {
            return false;
        }

        return true;
    }
}
