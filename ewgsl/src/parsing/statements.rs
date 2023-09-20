use std::ops::Range;

use crate::{
    arena::Handle,
    spans::{self, Spanned},
};

use super::{attributes::Attribute, expression::Expression};

#[cfg(feature = "eq")]
use crate::EqIn;

#[derive(Debug)]
pub struct IfClause<'a, S: spans::SpanState = spans::SpansPresent> {
    pub condition: Handle<Expression<'a, S>>,
    pub body: Range<Handle<Statement<'a, S>>>,
}

impl<'a> Spanned for IfClause<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = IfClause<'a, spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        IfClause {
            condition: self.condition.erase_spans(),
            body: self.body.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: spans::SpanState> EqIn<'a> for IfClause<'a, S> {
    type Context<'b> = <Statement<'a, S> as EqIn<'a>>::Context<'b>
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if !self.condition.eq_in(own_context.1, &other.condition, other_context.1) {
            return false;
        }

        if !self.body.eq_in(own_context, &other.body, other_context){
            return false;
        }

        return true;
    }
}

#[derive(Debug)]
pub enum Statement<'a, S: spans::SpanState = spans::SpansPresent> {
    Return(Option<Handle<Expression<'a, S>>>),
    If {
        attributes: Range<Handle<Attribute<'a, S>>>,
        /// A sequence of if bodies
        ifs: Range<Handle<IfClause<'a, S>>>,
        else_body: Option<spans::WithSpan<Range<Handle<Statement<'a, S>>>, S>>,
    },
}

impl<'a> Spanned for Statement<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = Statement<'a, spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        match self {
            Statement::Return(r) => Statement::Return(r.erase_spans()),
            Statement::If {
                attributes,
                ifs,
                else_body,
            } => Statement::If {
                attributes: attributes.erase_spans(),
                ifs: ifs.erase_spans(),
                else_body: else_body.erase_spans().map(|else_body| else_body.erase_inner_spans()),
            },
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: spans::SpanState> EqIn<'a> for Statement<'a, S> {
    type Context<'b> = (
        &'b crate::arena::Arena<super::attributes::Attribute<'a, S>, S>, 
        &'b crate::arena::Arena<super::expression::Expression<'a, S>, S>, 
        &'b crate::arena::Arena<Statement<'a, S>, S>, 
        &'b crate::arena::Arena<IfClause<'a, S>, S>
    )
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return false;
        }

        match (self, other) {
            (Statement::Return(lhs), Statement::Return(rhs)) => {
                lhs.eq_in(own_context.1, rhs, other_context.1)
            }
            (
                Statement::If {
                    attributes: lhs_attributes,
                    ifs: lhs_ifs,
                    else_body: lhs_else,
                },
                Statement::If {
                    attributes: rhs_attributes,
                    ifs: rhs_ifs,
                    else_body: rhs_else,
                },
            ) => {
                if !Attribute::are_sets_eq_in(
                    lhs_attributes.clone(),
                    own_context.0,
                    own_context.1,
                    rhs_attributes.clone(),
                    other_context.0,
                    other_context.1,
                ) {
                    return false;
                }

                if !lhs_else.eq_in(own_context, rhs_else, other_context) {
                    return false;
                }

                let lhs_ifs = &own_context.3[lhs_ifs.clone()];
                let rhs_ifs = &other_context.3[rhs_ifs.clone()];
                if lhs_ifs.len() != rhs_ifs.len() {
                    return false;
                }
                for (lhs, rhs) in lhs_ifs.into_iter().zip(rhs_ifs) {
                    if !lhs.eq_in(own_context, rhs, other_context) {
                        return false;
                    }
                }

                return true;
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: spans::SpanState> EqIn<'a> for Handle<Statement<'a, S>> {
    type Context<'b> = <Statement<'a, S> as EqIn<'a>>::Context<'b>
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        let lhs = &own_context.2[*self];
        let rhs = &other_context.2[*other];
        return lhs.eq_in(own_context, rhs, other_context);
    }
}

#[cfg(feature = "eq")]
impl<'a, S: spans::SpanState> EqIn<'a> for Range<Handle<Statement<'a, S>>> {
    type Context<'b> = <Statement<'a, S> as EqIn<'a>>::Context<'b>
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        let lhs_args = &own_context.2[self.clone()];
        let rhs_args = &other_context.2[other.clone()];
        if lhs_args.len() != rhs_args.len() {
            return false;
        }

        for (lhs, rhs) in lhs_args.into_iter().zip(rhs_args) {
            if !lhs.eq_in(own_context, rhs, other_context) {
                return false;
            }
        }

        return true;
    }
}
