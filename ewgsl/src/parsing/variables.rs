use std::ops::Range;

use perfect_derive::perfect_derive;

use super::{
    attributes::{Attribute, AttributeHandle},
    expression::{Expression, ExpressionHandle},
    ident,
    text_spans::{self, Spanned, SpannedParent},
};

#[cfg(feature = "eq")]
use crate::EqIn;

#[perfect_derive(Debug)]
pub struct TypeSpecifier<'a, S: text_spans::SpanState = text_spans::SpansPresent>(
    pub ident::TemplatedIdent<'a, S>,
);

impl<'a> Spanned for TypeSpecifier<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = TypeSpecifier<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        TypeSpecifier(self.0.erase_spans())
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for TypeSpecifier<'a, S> {
    type Context<'b> = <Expression<'a, S> as EqIn<'a>>::Context<'b> where 'a: 'b;

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
pub struct OptionallyTypedIdent<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub ident: ident::Ident<'a, S>,
    pub ty: Option<TypeSpecifier<'a, S>>,
}

impl<'a> Spanned for OptionallyTypedIdent<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = OptionallyTypedIdent<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        OptionallyTypedIdent {
            ident: self.ident.erase_spans(),
            ty: self.ty.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for OptionallyTypedIdent<'a, S> {
    type Context<'b> = <Expression<'a, S> as EqIn<'a>>::Context<'b> where 'a: 'b;

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
pub struct VariableDeclaration<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub template_list: Range<ExpressionHandle<'a, S>>,
    pub ident: OptionallyTypedIdent<'a, S>,
}

impl<'a> Spanned for VariableDeclaration<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = VariableDeclaration<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        VariableDeclaration {
            template_list: self.template_list.erase_spans(),
            ident: self.ident.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for VariableDeclaration<'a, S> {
    type Context<'b> = <Expression<'a, S> as EqIn<'a>>::Context<'b> where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        // Templates
        if !self
            .template_list
            .eq_in(own_context, &other.template_list, other_context)
        {
            return false;
        }

        // Ident
        if !self.ident.eq_in(own_context, &other.ident, other_context) {
            return false;
        }

        return true;
    }
}

#[perfect_derive(Debug)]
pub struct GlobalVariableDeclaration<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub attributes: Range<AttributeHandle<'a, S>>,
    pub decl: SpannedParent<VariableDeclaration<'a, S>, S>,
    pub init: Option<ExpressionHandle<'a, S>>,
}

impl<'a, S: text_spans::SpanState> GlobalVariableDeclaration<'a, S> {
    pub fn name<'b>(&'b self) -> &'b str {
        self.decl.inner().ident.ident.as_ref()
    }
}

impl<'a> Spanned for GlobalVariableDeclaration<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = GlobalVariableDeclaration<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        GlobalVariableDeclaration {
            attributes: self.attributes.erase_spans(),
            decl: self.decl.erase_spans(),
            init: self.init.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for GlobalVariableDeclaration<'a, S> {
    type Context<'b> = (&'b super::attributes::AttributeArena<'a, S>, &'b super::expression::ExpressionArena<'a, S>) where 'a: 'b;

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
pub struct GlobalConstantDeclaration<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub decl: SpannedParent<OptionallyTypedIdent<'a, S>, S>,
    pub init: ExpressionHandle<'a, S>,
}

impl<'a, S: text_spans::SpanState> GlobalConstantDeclaration<'a, S> {
    pub fn name<'b>(&'b self) -> &'b str {
        self.decl.inner().ident.as_ref()
    }
}

impl<'a> Spanned for GlobalConstantDeclaration<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = GlobalConstantDeclaration<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        GlobalConstantDeclaration {
            decl: self.decl.erase_spans(),
            init: self.init.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for GlobalConstantDeclaration<'a, S> {
    type Context<'b> = <Expression<'a, S> as EqIn<'a>>::Context<'b> where 'a: 'b;

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
pub struct GlobalOverrideDeclaration<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub attributes: Range<AttributeHandle<'a, S>>,
    pub decl: SpannedParent<OptionallyTypedIdent<'a, S>, S>,
    pub init: Option<ExpressionHandle<'a, S>>,
}

impl<'a, S: text_spans::SpanState> GlobalOverrideDeclaration<'a, S> {
    pub fn name<'b>(&'b self) -> &'b str {
        self.decl.inner().ident.as_ref()
    }
}

impl<'a> Spanned for GlobalOverrideDeclaration<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = GlobalOverrideDeclaration<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        GlobalOverrideDeclaration {
            attributes: self.attributes.erase_spans(),
            decl: self.decl.erase_spans(),
            init: self.init.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for GlobalOverrideDeclaration<'a, S> {
    type Context<'b> = (&'b super::attributes::AttributeArena<'a, S>, &'b super::expression::ExpressionArena<'a, S>) where 'a: 'b;

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
