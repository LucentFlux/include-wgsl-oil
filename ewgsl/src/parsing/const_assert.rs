use crate::{
    arena::{Arena, Handle},
    spans::{self, Spanned},
    EqIn,
};

use super::expression::Expression;

#[derive(Debug)]
pub struct ConstAssertStatement<'a, S: spans::SpanState = spans::SpansPresent> {
    pub expr: Handle<Expression<'a, S>>,
}

impl<'a> Spanned for ConstAssertStatement<'a> {
    type Spanless = ConstAssertStatement<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        ConstAssertStatement {
            expr: self.expr.erase_spans(),
        }
    }
}

impl<'a, S: spans::SpanState> EqIn<'a> for ConstAssertStatement<'a, S> {
    type Context<'b> = Arena<Expression<'a, S>, S>
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        return self.expr.eq_in(own_context, &other.expr, other_context);
    }
}
