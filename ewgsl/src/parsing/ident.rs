use std::ops::Deref;

use perfect_derive::perfect_derive;

use crate::{
    arena::{Arena, Handle},
    spans::{self, Spanned},
    EqIn,
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
pub struct Ident<'a>(&'a str);

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

        return Ok(Self(ident));
    }
}

impl<'a> AsRef<str> for Ident<'a> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl<'a> Deref for Ident<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

#[perfect_derive(Debug)]
pub struct TemplatedIdent<'a, S: spans::SpanState = spans::SpansPresent> {
    pub ident: spans::WithSpan<Ident<'a>, S>,
    pub args: Vec<Handle<Expression<'a, S>>>,
}

impl<'a, S: spans::SpanState> EqIn<'a> for TemplatedIdent<'a, S> {
    type Context<'b> = Arena<Expression<'a, S>, S> where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if self.ident != other.ident {
            return false;
        }

        if self.args.len() != other.args.len() {
            return false;
        }

        for (lhs, rhs) in self.args.iter().zip(&other.args) {
            if !lhs.eq_in(own_context, rhs, other_context) {
                panic!("{lhs:?} != {rhs:?} in \n{own_context:#?} and \n{other_context:#?}");
                return false;
            }
        }

        return true;
    }
}

impl<'a> Spanned for TemplatedIdent<'a, spans::SpansPresent> {
    type Spanless = TemplatedIdent<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        TemplatedIdent {
            ident: self.ident.erase_spans(),
            args: self.args.erase_spans(),
        }
    }
}
