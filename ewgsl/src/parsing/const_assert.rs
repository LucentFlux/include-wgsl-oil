use crate::{
    arena::Handle,
    spans::{self, Spanned},
};

use super::expression::Expression;

#[cfg(feature = "eq")]
use crate::EqIn;

#[derive(Debug)]
pub struct ConstAssertStatement<'a, S: spans::SpanState = spans::SpansPresent> {
    pub expr: Handle<Expression<'a, S>>,
}

impl<'a> Spanned for ConstAssertStatement<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = ConstAssertStatement<'a, spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        ConstAssertStatement {
            expr: self.expr.erase_spans(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: spans::SpanState> EqIn<'a> for ConstAssertStatement<'a, S> {
    type Context<'b> = crate::arena::Arena<super::expression::Expression<'a, S>, S>
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
