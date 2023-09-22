use std::ops::Range;

use perfect_derive::perfect_derive;

use crate::arena::{Arena, Handle};

use super::{
    ident::TemplatedIdent,
    text_spans::{self, Spanned, SpannedLeaf, SpannedParent},
};

pub type ExpressionArena<'a, S = text_spans::SpansPresent> =
    Arena<SpannedParent<Expression<'a, S>, S>>;
pub type ExpressionHandle<'a, S = text_spans::SpansPresent> =
    Handle<SpannedParent<Expression<'a, S>, S>>;

#[cfg(feature = "eq")]
use crate::EqIn;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum BinaryOperator {
    And,
    Or,
    Xor,
    LessThanEqual,
    GreaterThanEqual,
    LessThan,
    GreaterThan,
    EqualTo,
    NotEqualTo,
    ShortCircuitAnd,
    ShortCircuitOr,
    ShiftLeft,
    ShiftRight,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum UnaryOperator {
    /// `-`
    Minus,
    /// `!`
    Not,
    /// `~`
    Invert,
    /// `&`
    Reference,
    /// `*`
    Dereference,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Literal<'a> {
    Int(&'a str),
    Float(&'a str),
    Boolean(bool),
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum SwizzleComponent {
    First,
    Second,
    Third,
    Fourth,
}
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum SwizzleComponents {
    Len1(SwizzleComponent),
    Len2(SwizzleComponent, SwizzleComponent),
    Len3(SwizzleComponent, SwizzleComponent, SwizzleComponent),
    Len4(
        SwizzleComponent,
        SwizzleComponent,
        SwizzleComponent,
        SwizzleComponent,
    ),
}
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum SwizzleVariant {
    XYZW,
    RGBA,
}
/// A vector swizzle, like `xxyy` or `xyw` or `bgr`.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Swizzle {
    pub components: SwizzleComponents,
    pub variant: SwizzleVariant,
}

impl Swizzle {
    pub fn try_parse(swizzle: &str) -> Option<Self> {
        if swizzle.len() > 4 {
            return None;
        }
        let mut components = vec![];
        let mut variant = None;
        for c in swizzle.chars() {
            let component = match c {
                'r' | 'x' => SwizzleComponent::First,
                'g' | 'y' => SwizzleComponent::Second,
                'b' | 'z' => SwizzleComponent::Third,
                'a' | 'w' => SwizzleComponent::Fourth,
                _ => return None,
            };
            components.push(component);

            let found_variant = match c {
                'r' | 'g' | 'b' | 'a' => SwizzleVariant::RGBA,
                'x' | 'y' | 'z' | 'w' => SwizzleVariant::XYZW,
                _ => return None,
            };

            if variant.is_none() {
                variant = Some(found_variant);
            }
            if let Some(variant) = variant {
                if variant != found_variant {
                    return None;
                }
            }
        }

        let components = match components[..] {
            [a] => SwizzleComponents::Len1(a),
            [a, b] => SwizzleComponents::Len2(a, b),
            [a, b, c] => SwizzleComponents::Len3(a, b, c),
            [a, b, c, d] => SwizzleComponents::Len4(a, b, c, d),
            _ => return None,
        };

        let variant = match variant {
            Some(variant) => variant,
            None => return None,
        };

        return Some(Self {
            components,
            variant,
        });
    }
}

#[perfect_derive(Debug, Clone)]
pub struct CallPhrase<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub ident: TemplatedIdent<'a, S>,
    pub args: Range<ExpressionHandle<'a, S>>,
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for CallPhrase<'a, S> {
    type Context<'b> = ExpressionArena<'a, S>
    where
        'a: 'b;

    fn eq_in(
        &self,
        lhs_arena: &ExpressionArena<'a, S>,
        rhs: &Self,
        rhs_arena: &ExpressionArena<'a, S>,
    ) -> bool {
        if !self.ident.eq_in(lhs_arena, &rhs.ident, rhs_arena) {
            return false;
        }

        let lhs_args = &lhs_arena[self.args.clone()];
        let rhs_args = &rhs_arena[rhs.args.clone()];
        if lhs_args.len() != rhs_args.len() {
            return false;
        }
        for (lhs, rhs) in lhs_args.into_iter().zip(rhs_args) {
            if !lhs.eq_in(lhs_arena, rhs, rhs_arena) {
                return false;
            }
        }

        return true;
    }
}

impl<'a> Spanned for CallPhrase<'a> {
    #[cfg(feature = "span_erasure")]
    type Spanless = CallPhrase<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        CallPhrase {
            ident: self.ident.erase_spans(),
            args: self.args.erase_spans(),
        }
    }
}

#[perfect_derive(Debug, Clone)]
pub enum Expression<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    Literal {
        value: Literal<'a>,
    },
    Identifier {
        ident: TemplatedIdent<'a, S>,
    },
    Call(CallPhrase<'a, S>),
    Unary {
        op: SpannedLeaf<UnaryOperator, S>,
        expr: ExpressionHandle<'a, S>,
    },
    Binary {
        lhs: ExpressionHandle<'a, S>,
        op: SpannedLeaf<BinaryOperator, S>,
        rhs: ExpressionHandle<'a, S>,
    },
    Index {
        base: ExpressionHandle<'a, S>,
        index: ExpressionHandle<'a, S>,
    },
    Swizzle {
        base: ExpressionHandle<'a, S>,
        swizzle: SpannedLeaf<Swizzle, S>,
    },
    MemberAccess {
        base: ExpressionHandle<'a, S>,
        member: SpannedLeaf<&'a str, S>,
    },
    Parenthesized(ExpressionHandle<'a, S>),
}

const _: () = {
    // Check Expression is Clone
    fn assert_clone<T: Clone>() {}
    fn assert_all() {
        assert_clone::<Expression<'static, text_spans::SpansPresent>>();
        #[cfg(feature = "span_erasure")]
        assert_clone::<Expression<'static, text_spans::SpansErased>>();
    }
};

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for Expression<'a, S> {
    type Context<'b> = ExpressionArena<'a, S> where 'a: 'b;

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
            (Self::Literal { value: lhs }, Self::Literal { value: rhs }) => lhs == rhs,
            (Self::Identifier { ident: lhs }, Self::Identifier { ident: rhs }) => {
                lhs.eq_in(own_context, rhs, other_context)
            }
            (Self::Call(lhs), Self::Call(rhs)) => lhs.eq_in(own_context, rhs, other_context),
            (
                Self::Unary {
                    op: lhs_op,
                    expr: lhs_expr,
                },
                Self::Unary {
                    op: rhs_op,
                    expr: rhs_expr,
                },
            ) => lhs_op == rhs_op && lhs_expr.eq_in(own_context, rhs_expr, other_context),
            (
                Self::Binary {
                    lhs: lhs_expr_of_lhs,
                    op: lhs_op,
                    rhs: rhs_expr_of_lhs,
                },
                Self::Binary {
                    lhs: lhs_expr_of_rhs,
                    op: rhs_op,
                    rhs: rhs_expr_of_rhs,
                },
            ) => {
                lhs_op == rhs_op
                    && lhs_expr_of_lhs.eq_in(own_context, lhs_expr_of_rhs, other_context)
                    && rhs_expr_of_lhs.eq_in(own_context, rhs_expr_of_rhs, other_context)
            }
            (
                Self::Index {
                    base: lhs_base,
                    index: lhs_index,
                },
                Self::Index {
                    base: rhs_base,
                    index: rhs_index,
                },
            ) => {
                lhs_base.eq_in(own_context, rhs_base, other_context)
                    && lhs_index.eq_in(own_context, rhs_index, other_context)
            }
            (
                Self::Swizzle {
                    base: lhs_base,
                    swizzle: lhs_swizzle,
                },
                Self::Swizzle {
                    base: rhs_base,
                    swizzle: rhs_swizzle,
                },
            ) => lhs_base.eq_in(own_context, rhs_base, other_context) && lhs_swizzle == rhs_swizzle,
            (
                Self::MemberAccess {
                    base: lhs_base,
                    member: lhs_member,
                },
                Self::MemberAccess {
                    base: rhs_base,
                    member: rhs_member,
                },
            ) => lhs_member == rhs_member && lhs_base.eq_in(own_context, rhs_base, other_context),
            (Self::Parenthesized(lhs), Self::Parenthesized(rhs)) => {
                lhs.eq_in(own_context, rhs, other_context)
            }
            _ => unimplemented!(),
        }
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for ExpressionHandle<'a, S> {
    type Context<'b> = ExpressionArena<'a, S> where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        let lhs = &own_context[*self];
        let rhs = &other_context[*other];
        return lhs.eq_in(own_context, rhs, other_context);
    }
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for Range<ExpressionHandle<'a, S>> {
    type Context<'b> = ExpressionArena<'a, S>
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        let lhs_args = &own_context[self.clone()];
        let rhs_args = &other_context[other.clone()];
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

#[cfg(feature = "span_erasure")]
impl<'a> Spanned for Expression<'a> {
    type Spanless = Expression<'a, text_spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        match self {
            Expression::Literal { value } => Expression::Literal { value },
            Expression::Identifier { ident } => Expression::Identifier {
                ident: ident.erase_spans(),
            },
            Expression::Call(phrase) => Expression::Call(phrase.erase_spans()),
            Expression::Unary { op, expr } => Expression::Unary {
                op: op.erase_spans(),
                expr: expr.erase_spans(),
            },
            Expression::Binary { lhs, op, rhs } => Expression::Binary {
                lhs: lhs.erase_spans(),
                op: op.erase_spans(),
                rhs: rhs.erase_spans(),
            },
            Expression::Index { base, index } => Expression::Index {
                base: base.erase_spans(),
                index: index.erase_spans(),
            },
            Expression::Swizzle { base, swizzle } => Expression::Swizzle {
                base: base.erase_spans(),
                swizzle: swizzle.erase_spans(),
            },
            Expression::MemberAccess { base, member } => Expression::MemberAccess {
                base: base.erase_spans(),
                member: member.erase_spans(),
            },
            Expression::Parenthesized(v) => Expression::Parenthesized(v.erase_spans()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn swizzle_parse_1() {
        let swzl = Swizzle::try_parse("xyzw").unwrap();
        assert_eq!(
            swzl,
            Swizzle {
                components: SwizzleComponents::Len4(
                    SwizzleComponent::First,
                    SwizzleComponent::Second,
                    SwizzleComponent::Third,
                    SwizzleComponent::Fourth
                ),
                variant: SwizzleVariant::XYZW
            }
        )
    }
    #[test]
    fn swizzle_parse_2() {
        let swzl = Swizzle::try_parse("rgba").unwrap();
        assert_eq!(
            swzl,
            Swizzle {
                components: SwizzleComponents::Len4(
                    SwizzleComponent::First,
                    SwizzleComponent::Second,
                    SwizzleComponent::Third,
                    SwizzleComponent::Fourth
                ),
                variant: SwizzleVariant::RGBA
            }
        )
    }
    #[test]
    fn swizzle_parse_3() {
        let swzl = Swizzle::try_parse("wxz").unwrap();
        assert_eq!(
            swzl,
            Swizzle {
                components: SwizzleComponents::Len3(
                    SwizzleComponent::Fourth,
                    SwizzleComponent::First,
                    SwizzleComponent::Third
                ),
                variant: SwizzleVariant::XYZW
            }
        )
    }
    #[test]
    fn swizzle_parse_4() {
        let swzl = Swizzle::try_parse("a").unwrap();
        assert_eq!(
            swzl,
            Swizzle {
                components: SwizzleComponents::Len1(SwizzleComponent::Fourth),
                variant: SwizzleVariant::RGBA
            }
        )
    }
    #[test]
    fn swizzle_parse_5() {
        let swzl = Swizzle::try_parse("gb").unwrap();
        assert_eq!(
            swzl,
            Swizzle {
                components: SwizzleComponents::Len2(
                    SwizzleComponent::Second,
                    SwizzleComponent::Third
                ),
                variant: SwizzleVariant::RGBA
            }
        )
    }
}
