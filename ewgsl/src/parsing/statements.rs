use std::ops::Range;

use crate::{
    arena::Handle,
    spans::{self, Spanned},
};

use super::{attributes::Attribute, expression::Expression, ParsedModule};

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
        if !self.condition.eq_in(
            &own_context.expressions,
            &other.condition,
            &other_context.expressions,
        ) {
            return false;
        }

        if !self.body.eq_in(own_context, &other.body, other_context) {
            return false;
        }

        return true;
    }
}

#[derive(Debug)]
pub struct SwitchClause<'a, S: spans::SpanState = spans::SpansPresent> {
    pub default_keyword: Option<spans::Span>,
    pub selectors: Range<Handle<Expression<'a, S>>>,
    pub body: Range<Handle<Statement<'a, S>>>,
}

impl<'a> Spanned for SwitchClause<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = SwitchClause<'a, spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        SwitchClause {
            default_keyword: self.default_keyword.map(|_| spans::Span::empty()),
            selectors: self.selectors.erase_spans(),
            body: self.body.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: spans::SpanState> EqIn<'a> for SwitchClause<'a, S> {
    type Context<'b> = <Statement<'a, S> as EqIn<'a>>::Context<'b>
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if self.default_keyword != other.default_keyword {
            return false;
        }

        if !self.selectors.eq_in(
            &own_context.expressions,
            &other.selectors,
            &other_context.expressions,
        ) {
            return false;
        }

        if !self.body.eq_in(own_context, &other.body, other_context) {
            return false;
        }

        return true;
    }
}

#[derive(Debug)]
pub struct ContinuingStatement<'a, S: spans::SpanState = spans::SpansPresent> {
    pub continuing_keyword_span: spans::Span,
    pub body: Range<Handle<Statement<'a, S>>>,
    pub break_if: Option<Handle<Expression<'a, S>>>,
}

impl<'a> Spanned for ContinuingStatement<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = ContinuingStatement<'a, spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        ContinuingStatement {
            continuing_keyword_span: spans::Span::empty(),
            body: self.body.erase_spans(),
            break_if: self.break_if.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: spans::SpanState> EqIn<'a> for ContinuingStatement<'a, S> {
    type Context<'b> = <Statement<'a, S> as EqIn<'a>>::Context<'b>
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if self.continuing_keyword_span != other.continuing_keyword_span {
            return false;
        }

        if !self.body.eq_in(own_context, &other.body, other_context) {
            return false;
        }

        if !self.break_if.eq_in(
            &own_context.expressions,
            &other.break_if,
            &other_context.expressions,
        ) {
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
    Switch {
        attributes: Range<Handle<Attribute<'a, S>>>,
        expression: Handle<Expression<'a, S>>,
        clauses: Range<Handle<SwitchClause<'a, S>>>,
    },
    Loop {
        attributes: Range<Handle<Attribute<'a, S>>>,
        body: Range<Handle<Statement<'a, S>>>,
        continuing: Option<spans::WithSpan<ContinuingStatement<'a, S>, S>>,
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
                else_body: else_body
                    .erase_spans()
                    .map(|else_body| else_body.erase_inner_spans()),
            },
            Statement::Switch {
                attributes,
                clauses,
                expression,
            } => Statement::Switch {
                attributes: attributes.erase_spans(),
                expression: expression.erase_spans(),
                clauses: clauses.erase_spans(),
            },
            Statement::Loop {
                attributes,
                body,
                continuing,
            } => Statement::Loop {
                attributes: attributes.erase_spans(),
                body: body.erase_spans(),
                continuing: continuing
                    .erase_spans()
                    .map(|inner| inner.erase_inner_spans()),
            },
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: spans::SpanState> EqIn<'a> for Statement<'a, S> {
    type Context<'b> = ParsedModule<'a, S>
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
                return lhs.eq_in(&own_context.expressions, rhs, &other_context.expressions);
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
                    &own_context.attributes,
                    &own_context.expressions,
                    rhs_attributes.clone(),
                    &other_context.attributes,
                    &other_context.expressions,
                ) {
                    return false;
                }

                if !lhs_else.eq_in(own_context, rhs_else, other_context) {
                    return false;
                }

                let lhs_ifs = &own_context.if_clauses[lhs_ifs.clone()];
                let rhs_ifs = &other_context.if_clauses[rhs_ifs.clone()];
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
            (
                Statement::Switch {
                    attributes: lhs_attributes,
                    expression: lhs_expression,
                    clauses: lhs_clauses,
                },
                Statement::Switch {
                    attributes: rhs_attributes,
                    expression: rhs_expression,
                    clauses: rhs_clauses,
                },
            ) => {
                if !Attribute::are_sets_eq_in(
                    lhs_attributes.clone(),
                    &own_context.attributes,
                    &own_context.expressions,
                    rhs_attributes.clone(),
                    &other_context.attributes,
                    &other_context.expressions,
                ) {
                    return false;
                }

                if !lhs_expression.eq_in(
                    &own_context.expressions,
                    rhs_expression,
                    &other_context.expressions,
                ) {
                    return false;
                }

                let lhs_clauses = &own_context.switch_clauses[lhs_clauses.clone()];
                let rhs_clauses = &other_context.switch_clauses[rhs_clauses.clone()];

                if lhs_clauses.len() != rhs_clauses.len() {
                    return false;
                }
                for (lhs, rhs) in lhs_clauses.into_iter().zip(rhs_clauses) {
                    if !lhs.eq_in(own_context, rhs, other_context) {
                        return false;
                    }
                }

                return true;
            }
            (
                Statement::Loop {
                    attributes: lhs_attributes,
                    body: lhs_body,
                    continuing: lhs_continuing,
                },
                Statement::Loop {
                    attributes: rhs_attributes,
                    body: rhs_body,
                    continuing: rhs_continuing,
                },
            ) => {
                if !Attribute::are_sets_eq_in(
                    lhs_attributes.clone(),
                    &own_context.attributes,
                    &own_context.expressions,
                    rhs_attributes.clone(),
                    &other_context.attributes,
                    &other_context.expressions,
                ) {
                    return false;
                }

                if !lhs_body.eq_in(own_context, rhs_body, other_context) {
                    return false;
                }

                if !lhs_continuing.eq_in(own_context, rhs_continuing, other_context) {
                    return false;
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
        let lhs = &own_context.statements[*self];
        let rhs = &other_context.statements[*other];
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
        let lhs_args = &own_context.statements[self.clone()];
        let rhs_args = &other_context.statements[other.clone()];
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
