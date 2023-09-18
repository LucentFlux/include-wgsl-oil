use std::mem::{discriminant, Discriminant};

use perfect_derive::perfect_derive;

use crate::{
    arena::{Arena, Handle},
    spans::{self, MaybeSpanned, Spanned},
};

use super::ident::{Ident, TemplatedIdent};

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

#[derive(Debug, Hash, PartialEq, Eq)]
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

#[perfect_derive(Debug)]
pub struct CallPhrase<'a, S: spans::Spanning = spans::WithSpans> {
    pub ident: TemplatedIdent<'a, S>,
    pub args: Vec<Handle<Expression<'a, S>>>,
}

impl<'a, S: spans::Spanning> CallPhrase<'a, S>
where
    S::Spanned<Ident<'a>>: PartialEq,
    S::Spanned<Expression<'a, S>>: PartialEq,
{
    // Checks if this call phrase is equal to another, given two (possibly different) arenas of expressions.
    pub fn eq_in(
        &self,
        lhs_arena: &Arena<Expression<'a, S>, S>,
        rhs: &Self,
        rhs_arena: &Arena<Expression<'a, S>, S>,
    ) -> bool {
        return self.ident.eq_in(lhs_arena, &rhs.ident, rhs_arena)
            && self.args.len() == rhs.args.len()
            && self.args.iter().zip(&rhs.args).all(|(lhs, rhs)| {
                match (lhs_arena.try_get(*lhs), rhs_arena.try_get(*rhs)) {
                    (Ok(lhs), Ok(rhs)) => lhs == rhs,
                    (Err(_), Err(_)) => true,
                    _ => false,
                }
            });
    }
}

impl<'a> Spanned for CallPhrase<'a> {
    type Spanless = CallPhrase<'a, spans::WithoutSpans>;

    fn erase_spans(self) -> Self::Spanless {
        CallPhrase {
            ident: self.ident.erase_spans(),
            args: self.args.erase_spans(),
        }
    }
}

#[perfect_derive(Debug, Clone)]
pub enum Expression<'a, S: spans::Spanning = spans::WithSpans> {
    Literal {
        value: Literal<'a>,
    },
    Identifier {
        ident: TemplatedIdent<'a, S>,
    },
    Call(CallPhrase<'a, S>),
    Unary {
        op: S::Spanned<UnaryOperator>,
        expr: Handle<Expression<'a, S>>,
    },
    Binary {
        lhs: Handle<Expression<'a, S>>,
        op: S::Spanned<BinaryOperator>,
        rhs: Handle<Expression<'a, S>>,
    },
    Index {
        base: Handle<Expression<'a, S>>,
        index: Handle<Expression<'a, S>>,
    },
    Swizzle {
        base: Handle<Expression<'a, S>>,
        swizzle: S::Spanned<Swizzle>,
    },
    MemberAccess {
        base: Handle<Expression<'a, S>>,
        member: S::Spanned<&'a str>,
    },
    Parenthesized(Handle<Expression<'a, S>>),
}

impl<'a, S: spans::Spanning> Handle<Expression<'a, S>>
where
    // Are all true all of the time, but rust's typechecking currently doesn't know it.
    S::Spanned<&'a str>: PartialEq,
    S::Spanned<Ident<'a>>: PartialEq,
    S::Spanned<UnaryOperator>: PartialEq,
    S::Spanned<BinaryOperator>: PartialEq,
    S::Spanned<Swizzle>: PartialEq,
{
    pub fn eq_in(
        &self,
        lhs_arena: &Arena<Expression<'a, S>, S>,
        rhs: &Self,
        rhs_arena: &Arena<Expression<'a, S>, S>,
    ) -> bool {
        match (lhs_arena.try_get(*self), rhs_arena.try_get(*rhs)) {
            (Ok(lhs), Ok(rhs)) => {
                lhs.span() == rhs.span() && lhs.inner().eq_in(lhs_arena, rhs.inner(), rhs_arena)
            }
            (Err(_), Err(_)) => true,
            _ => false,
        }
    }
}

impl<'a, S: spans::Spanning> Expression<'a, S>
where
    S::Spanned<&'a str>: PartialEq,
    S::Spanned<Ident<'a>>: PartialEq,
    S::Spanned<UnaryOperator>: PartialEq,
    S::Spanned<BinaryOperator>: PartialEq,
    S::Spanned<Swizzle>: PartialEq,
{
    // Checks if this expression is equal to another, given two (possibly different) arenas.
    pub fn eq_in(
        &self,
        lhs_arena: &Arena<Expression<'a, S>, S>,
        rhs: &Self,
        rhs_arena: &Arena<Expression<'a, S>, S>,
    ) -> bool {
        if discriminant(self) != discriminant(rhs) {
            return false;
        }

        match (self, rhs) {
            (Self::Literal { value: lhs }, Self::Literal { value: rhs }) => lhs == rhs,
            (Self::Identifier { ident: lhs }, Self::Identifier { ident: rhs }) => {
                lhs.eq_in(lhs_arena, rhs, rhs_arena)
            }
            (Self::Call(lhs), Self::Call(rhs)) => lhs.eq_in(lhs_arena, rhs, rhs_arena),
            (
                Self::Unary {
                    op: lhs_op,
                    expr: lhs_expr,
                },
                Self::Unary {
                    op: rhs_op,
                    expr: rhs_expr,
                },
            ) => lhs_op == rhs_op && lhs_expr.eq_in(lhs_arena, rhs_expr, rhs_arena),
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
                    && lhs_expr_of_lhs.eq_in(lhs_arena, lhs_expr_of_rhs, rhs_arena)
                    && rhs_expr_of_lhs.eq_in(lhs_arena, rhs_expr_of_rhs, rhs_arena)
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
                lhs_base.eq_in(lhs_arena, rhs_base, rhs_arena)
                    && lhs_index.eq_in(lhs_arena, rhs_index, rhs_arena)
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
            ) => lhs_base.eq_in(lhs_arena, rhs_base, rhs_arena) && lhs_swizzle == rhs_swizzle,
            (
                Self::MemberAccess {
                    base: lhs_base,
                    member: lhs_member,
                },
                Self::MemberAccess {
                    base: rhs_base,
                    member: rhs_member,
                },
            ) => lhs_member == rhs_member && lhs_base.eq_in(lhs_arena, rhs_base, rhs_arena),
            (Self::Parenthesized(lhs), Self::Parenthesized(rhs)) => {
                lhs.eq_in(lhs_arena, rhs, rhs_arena)
            }
            _ => unimplemented!(),
        }
    }
}

impl<'a> Spanned for Expression<'a, spans::WithSpans> {
    type Spanless = Expression<'a, spans::WithoutSpans>;

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
