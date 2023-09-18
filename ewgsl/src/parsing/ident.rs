use std::ops::Deref;

use perfect_derive::perfect_derive;

use crate::{
    arena::{Arena, Handle},
    spans::{self, Spanned},
};

use super::expression::Expression;

const KEYWORDS: [&'static str; 26] = [
    "alias",
    "break",
    "case",
    "const",
    "const_assert",
    "continue",
    "continuing",
    "default",
    "diagnostic",
    "discard",
    "else",
    "enable",
    "false",
    "fn",
    "for",
    "if",
    "let",
    "loop",
    "override",
    "requires",
    "return",
    "struct",
    "switch",
    "true",
    "var",
    "while",
];

pub fn is_keyword(kwd: &str) -> bool {
    KEYWORDS.binary_search(&kwd).is_ok()
}

const RESERVED_WORDS: [&'static str; 145] = [
    "NULL",
    "Self",
    "abstract",
    "active",
    "alignas",
    "alignof",
    "as",
    "asm",
    "asm_fragment",
    "async",
    "attribute",
    "auto",
    "await",
    "become",
    "binding_array",
    "cast",
    "catch",
    "class",
    "co_await",
    "co_return",
    "co_yield",
    "coherent",
    "column_major",
    "common",
    "compile",
    "compile_fragment",
    "concept",
    "const_cast",
    "consteval",
    "constexpr",
    "constinit",
    "crate",
    "debugger",
    "decltype",
    "delete",
    "demote",
    "demote_to_helper",
    "do",
    "dynamic_cast",
    "enum",
    "explicit",
    "export",
    "extends",
    "extern",
    "external",
    "fallthrough",
    "filter",
    "final",
    "finally",
    "friend",
    "from",
    "fxgroup",
    "get",
    "goto",
    "groupshared",
    "highp",
    "impl",
    "implements",
    "import",
    "inline",
    "instanceof",
    "interface",
    "layout",
    "lowp",
    "macro",
    "macro_rules",
    "match",
    "mediump",
    "meta",
    "mod",
    "module",
    "move",
    "mut",
    "mutable",
    "namespace",
    "new",
    "nil",
    "noexcept",
    "noinline",
    "nointerpolation",
    "noperspective",
    "null",
    "nullptr",
    "of",
    "operator",
    "package",
    "packoffset",
    "partition",
    "pass",
    "patch",
    "pixelfragment",
    "precise",
    "precision",
    "premerge",
    "priv",
    "protected",
    "pub",
    "public",
    "readonly",
    "ref",
    "regardless",
    "register",
    "reinterpret_cast",
    "require",
    "resource",
    "restrict",
    "self",
    "set",
    "shared",
    "sizeof",
    "smooth",
    "snorm",
    "static",
    "static_assert",
    "static_cast",
    "std",
    "subroutine",
    "super",
    "target",
    "template",
    "this",
    "thread_local",
    "throw",
    "trait",
    "try",
    "type",
    "typedef",
    "typeid",
    "typename",
    "typeof",
    "union",
    "unless",
    "unorm",
    "unsafe",
    "unsized",
    "use",
    "using",
    "varying",
    "virtual",
    "volatile",
    "wgsl",
    "where",
    "with",
    "writeonly",
    "yield",
];

pub fn is_reserved_word(kwd: &str) -> bool {
    RESERVED_WORDS.binary_search(&kwd).is_ok()
}

/// One of many reasons that a string might not be an identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InvalidIdentifierReason {
    WasSingleUnderscore,
    StartedWithDoubleUnderscore,
    WasKeyword,
    WasReservedWord,
}

/// Some single word without spaces or punctuation, which isn't a keyword or reserved word,
/// and which doesn't start with "__".
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Ident<'a> {
    ident: &'a str,
}

impl<'a> Ident<'a> {
    pub fn try_parse(ident: &'a str) -> Result<Self, InvalidIdentifierReason> {
        if ident == "_" {
            return Err(InvalidIdentifierReason::WasSingleUnderscore);
        }
        if ident.starts_with("__") {
            return Err(InvalidIdentifierReason::StartedWithDoubleUnderscore);
        }
        if is_keyword(ident) {
            return Err(InvalidIdentifierReason::WasKeyword);
        }
        if is_reserved_word(ident) {
            return Err(InvalidIdentifierReason::WasReservedWord);
        }

        return Ok(Self { ident: ident });
    }
}

impl<'a> AsRef<str> for Ident<'a> {
    fn as_ref(&self) -> &str {
        self.ident
    }
}

impl<'a> Deref for Ident<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.ident
    }
}

#[perfect_derive(Debug)]
pub struct TemplatedIdent<'a, S: spans::Spanning = spans::WithSpans> {
    pub ident: S::Spanned<Ident<'a>>,
    pub args: Vec<Handle<Expression<'a, S>>>,
}

impl<'a, S: spans::Spanning> TemplatedIdent<'a, S>
where
    S::Spanned<Ident<'a>>: PartialEq,
    S::Spanned<Expression<'a, S>>: PartialEq,
{
    // Checks if this templated ident is equal to another, given two (possibly different) arenas of expressions.
    pub fn eq_in(
        &self,
        lhs_arena: &Arena<Expression<'a, S>, S>,
        rhs: &Self,
        rhs_arena: &Arena<Expression<'a, S>, S>,
    ) -> bool {
        return self.ident == rhs.ident
            && self.args.len() == rhs.args.len()
            && self
                .args
                .iter()
                .zip(&rhs.args)
                .all(|(lhs, rhs)| lhs.eq_in(lhs_arena, rhs, rhs_arena));
    }
}

impl<'a> Spanned for TemplatedIdent<'a, spans::WithSpans> {
    type Spanless = TemplatedIdent<'a, spans::WithoutSpans>;

    fn erase_spans(self) -> Self::Spanless {
        TemplatedIdent {
            ident: self.ident.erase_spans(),
            args: self.args.erase_spans(),
        }
    }
}
