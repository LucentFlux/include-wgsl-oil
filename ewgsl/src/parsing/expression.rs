use perfect_derive::perfect_derive;

use crate::{
    arena::Handle,
    spans::{self, Spanned},
};

use super::ident::TemplatedIdent;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum BinaryOperator {
    And,
    Or,
    Xor,
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

#[derive(Debug)]
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
        index: S::Spanned<Handle<Expression<'a, S>>>,
    },
    Swizzle {
        base: Handle<Expression<'a, S>>,
        swizzle: S::Spanned<Swizzle>,
    },
    MemberAccess {
        base: Handle<Expression<'a, S>>,
        member: S::Spanned<&'a str>,
    },
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
                index: index.erase_spans().erase_spans(),
            },
            Expression::Swizzle { base, swizzle } => Expression::Swizzle {
                base: base.erase_spans(),
                swizzle: swizzle.erase_spans(),
            },
            Expression::MemberAccess { base, member } => Expression::MemberAccess {
                base: base.erase_spans(),
                member: member.erase_spans(),
            },
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
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
