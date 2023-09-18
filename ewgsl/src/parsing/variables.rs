use std::{borrow::Borrow, ops::Range};

use perfect_derive::perfect_derive;

use crate::{
    arena::Handle,
    spans::{self, Spanned, Wrapper},
};

use super::{attributes::Attribute, expression::Expression, ident};

#[perfect_derive(Debug)]
pub struct TypeSpecifier<'a, S: spans::Spanning = spans::WithSpans>(
    pub ident::TemplatedIdent<'a, S>,
);

impl<'a> Spanned for TypeSpecifier<'a> {
    type Spanless = TypeSpecifier<'a, spans::WithoutSpans>;

    fn erase_spans(self) -> Self::Spanless {
        TypeSpecifier(self.0.erase_spans())
    }
}

#[perfect_derive(Debug)]
pub struct OptionallyTypedIdent<'a, S: spans::Spanning = spans::WithSpans> {
    pub ident: S::Spanned<ident::Ident<'a>>,
    pub ty: Option<TypeSpecifier<'a, S>>,
}

impl<'a> Spanned for OptionallyTypedIdent<'a> {
    type Spanless = OptionallyTypedIdent<'a, spans::WithoutSpans>;

    fn erase_spans(self) -> Self::Spanless {
        OptionallyTypedIdent {
            ident: self.ident.erase_spans(),
            ty: self.ty.map(|ty| ty.erase_spans()),
        }
    }
}
#[perfect_derive(Debug)]
pub struct VariableDeclaration<'a, S: spans::Spanning = spans::WithSpans> {
    pub template_list: Vec<Handle<Expression<'a, S>>>,
    pub ident: OptionallyTypedIdent<'a, S>,
}

impl<'a> Spanned for VariableDeclaration<'a> {
    type Spanless = VariableDeclaration<'a, spans::WithoutSpans>;

    fn erase_spans(self) -> Self::Spanless {
        VariableDeclaration {
            template_list: self.template_list.erase_spans(),
            ident: self.ident.erase_spans(),
        }
    }
}

#[perfect_derive(Debug)]
pub struct GlobalVariableDeclaration<'a, S: spans::Spanning = spans::WithSpans> {
    pub attributes: Range<Handle<Attribute<'a, S>>>,
    pub decl: S::Spanned<VariableDeclaration<'a, S>>,
    pub init: Option<Handle<Expression<'a, S>>>,
}

impl<'a, S: spans::Spanning> GlobalVariableDeclaration<'a, S> {
    pub fn name<'b>(&'b self) -> &'b str {
        self.decl.unwrap().ident.ident.unwrap()
    }
}

impl<'a> Spanned for GlobalVariableDeclaration<'a> {
    type Spanless = GlobalVariableDeclaration<'a, spans::WithoutSpans>;

    fn erase_spans(self) -> Self::Spanless {
        GlobalVariableDeclaration {
            attributes: self.attributes.erase_spans(),
            decl: self.decl.erase_spans().erase_spans(),
            init: self.init.map(|init| init.erase_spans()),
        }
    }
}
