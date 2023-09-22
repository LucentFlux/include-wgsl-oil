use super::{
    expression::ExpressionHandle,
    text_spans::{self, Spanned},
};

#[cfg(feature = "eq")]
use crate::EqIn;

#[derive(Debug)]
pub struct ConstAssertStatement<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub expr: ExpressionHandle<'a, S>,
}

impl<'a> Spanned for ConstAssertStatement<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = ConstAssertStatement<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        ConstAssertStatement {
            expr: self.expr.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for ConstAssertStatement<'a, S> {
    type Context<'b> = super::expression::ExpressionArena<'a, S>
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
