use std::ops::Range;

use perfect_derive::perfect_derive;

use crate::{
    arena::{Arena, Handle},
    spans::{self, Spanned},
    EqIn,
};

use super::{attributes::Attribute, expression::Expression, ident};

#[perfect_derive(Debug)]
pub struct TypeSpecifier<'a, S: spans::SpanState = spans::SpansPresent>(
    pub ident::TemplatedIdent<'a, S>,
);

impl<'a> Spanned for TypeSpecifier<'a> {
    type Spanless = TypeSpecifier<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        TypeSpecifier(self.0.erase_spans())
    }
}

impl<'a, S: spans::SpanState> EqIn<'a> for TypeSpecifier<'a, S> {
    type Context<'b> = Arena<Expression<'a, S>, S> where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        self.0.eq_in(own_context, &other.0, other_context)
    }
}

#[perfect_derive(Debug)]
pub struct OptionallyTypedIdent<'a, S: spans::SpanState = spans::SpansPresent> {
    pub ident: spans::WithSpan<ident::Ident<'a>, S>,
    pub ty: Option<TypeSpecifier<'a, S>>,
}

impl<'a> Spanned for OptionallyTypedIdent<'a> {
    type Spanless = OptionallyTypedIdent<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        OptionallyTypedIdent {
            ident: self.ident.erase_spans(),
            ty: self.ty.map(|ty| ty.erase_spans()),
        }
    }
}

impl<'a, S: spans::SpanState> EqIn<'a> for OptionallyTypedIdent<'a, S> {
    type Context<'b> = Arena<Expression<'a, S>, S> where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if !self.ident.eq(&other.ident) {
            return false;
        }

        if !self.ty.eq_in(own_context, &other.ty, other_context) {
            return false;
        }

        return true;
    }
}

#[perfect_derive(Debug)]
pub struct VariableDeclaration<'a, S: spans::SpanState = spans::SpansPresent> {
    pub template_list: Range<Handle<Expression<'a, S>>>,
    pub ident: OptionallyTypedIdent<'a, S>,
}

impl<'a> Spanned for VariableDeclaration<'a> {
    type Spanless = VariableDeclaration<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        VariableDeclaration {
            template_list: self.template_list.erase_spans(),
            ident: self.ident.erase_spans(),
        }
    }
}

impl<'a, S: spans::SpanState> EqIn<'a> for VariableDeclaration<'a, S> {
    type Context<'b> = Arena<Expression<'a, S>, S> where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        // Templates - assume ordered
        let lhs_templates = &own_context[self.template_list.clone()];
        let rhs_templates = &other_context[other.template_list.clone()];
        if lhs_templates.len() != rhs_templates.len() {
            return false;
        }
        for (lhs, rhs) in lhs_templates.into_iter().zip(rhs_templates) {
            if !lhs.eq_in(own_context, rhs, other_context) {
                return false;
            }
        }

        // Ident
        if !self.ident.eq_in(own_context, &other.ident, other_context) {
            return false;
        }

        return true;
    }
}

#[perfect_derive(Debug)]
pub struct GlobalVariableDeclaration<'a, S: spans::SpanState = spans::SpansPresent> {
    pub attributes: Range<Handle<Attribute<'a, S>>>,
    pub decl: spans::WithSpan<VariableDeclaration<'a, S>, S>,
    pub init: Option<Handle<Expression<'a, S>>>,
}

impl<'a, S: spans::SpanState> GlobalVariableDeclaration<'a, S> {
    pub fn name<'b>(&'b self) -> &'b str {
        self.decl.inner().ident.ident.inner()
    }
}

impl<'a> Spanned for GlobalVariableDeclaration<'a> {
    type Spanless = GlobalVariableDeclaration<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        GlobalVariableDeclaration {
            attributes: self.attributes.erase_spans(),
            decl: self.decl.erase_spans().map(Spanned::erase_spans),
            init: self.init.map(|init| init.erase_spans()),
        }
    }
}

impl<'a, S: spans::SpanState> EqIn<'a> for GlobalVariableDeclaration<'a, S> {
    type Context<'b> = (&'b Arena<Attribute<'a, S>, S>, &'b Arena<Expression<'a, S>, S>) where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        // Check attributes
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

        // Check declaration
        if !self
            .decl
            .eq_in(&own_context.1, &other.decl, &other_context.1)
        {
            return false;
        }

        // Check initialisation
        if !self
            .init
            .eq_in(&own_context.1, &other.init, &other_context.1)
        {
            return false;
        }

        return true;
    }
}

#[perfect_derive(Debug)]
pub struct GlobalConstantDeclaration<'a, S: spans::SpanState = spans::SpansPresent> {
    pub decl: spans::WithSpan<OptionallyTypedIdent<'a, S>, S>,
    pub init: Handle<Expression<'a, S>>,
}

impl<'a, S: spans::SpanState> GlobalConstantDeclaration<'a, S> {
    pub fn name<'b>(&'b self) -> &'b str {
        self.decl.inner().ident.inner()
    }
}

impl<'a> Spanned for GlobalConstantDeclaration<'a> {
    type Spanless = GlobalConstantDeclaration<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        GlobalConstantDeclaration {
            decl: self.decl.erase_spans().map(Spanned::erase_spans),
            init: self.init.erase_spans(),
        }
    }
}

impl<'a, S: spans::SpanState> EqIn<'a> for GlobalConstantDeclaration<'a, S> {
    type Context<'b> = Arena<Expression<'a, S>, S> where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        // Check declaration
        if !self.decl.eq_in(&own_context, &other.decl, &other_context) {
            return false;
        }

        // Check initialisation
        if !self.init.eq_in(&own_context, &other.init, &other_context) {
            return false;
        }

        return true;
    }
}

#[perfect_derive(Debug)]
pub struct GlobalOverrideDeclaration<'a, S: spans::SpanState = spans::SpansPresent> {
    pub attributes: Range<Handle<Attribute<'a, S>>>,
    pub decl: spans::WithSpan<OptionallyTypedIdent<'a, S>, S>,
    pub init: Option<Handle<Expression<'a, S>>>,
}

impl<'a, S: spans::SpanState> GlobalOverrideDeclaration<'a, S> {
    pub fn name<'b>(&'b self) -> &'b str {
        self.decl.inner().ident.inner()
    }
}

impl<'a> Spanned for GlobalOverrideDeclaration<'a> {
    type Spanless = GlobalOverrideDeclaration<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        GlobalOverrideDeclaration {
            attributes: self.attributes.erase_spans(),
            decl: self.decl.erase_spans().map(Spanned::erase_spans),
            init: self.init.map(|init| init.erase_spans()),
        }
    }
}

impl<'a, S: spans::SpanState> EqIn<'a> for GlobalOverrideDeclaration<'a, S> {
    type Context<'b> = (&'b Arena<Attribute<'a, S>, S>, &'b Arena<Expression<'a, S>, S>) where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        // Check attributes
        // Ranges are generated by parsing code so should be valid
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

        // Check declaration
        if !self
            .decl
            .eq_in(&own_context.1, &other.decl, &other_context.1)
        {
            return false;
        }

        // Check initialisation
        if !self
            .init
            .eq_in(&own_context.1, &other.init, &other_context.1)
        {
            return false;
        }

        return true;
    }
}
