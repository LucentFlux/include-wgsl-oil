use super::{
    ident::Ident,
    text_spans::{self, Spanned},
    variables::TypeSpecifier,
};

#[cfg(feature = "eq")]
use crate::EqIn;

/// A type alias expression, such as `alias MyVec3 = vec3<u32>;`
#[derive(Debug)]
pub struct TypeAliasDeclaration<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub ident: Ident<'a, S>,
    pub ty: TypeSpecifier<'a, S>,
}

#[cfg(feature = "span_erasure")]
impl<'a> Spanned for TypeAliasDeclaration<'a, text_spans::SpansPresent> {
    type Spanless = TypeAliasDeclaration<'a, text_spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        TypeAliasDeclaration {
            ident: self.ident.erase_spans(),
            ty: self.ty.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for TypeAliasDeclaration<'a, S> {
    type Context<'b> = super::expression::ExpressionArena<'a, S>
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
