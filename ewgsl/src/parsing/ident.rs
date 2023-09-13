use perfect_derive::perfect_derive;

use crate::{
    arena::Handle,
    spans::{self, Span, Spanned, WithSpan},
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
#[derive(Debug)]
pub struct Ident<'a, S: spans::Spanning = spans::WithSpans> {
    ident: S::Spanned<&'a str>,
}

impl<'a> Ident<'a> {
    pub fn try_parse(ident: &'a str, span: Span) -> Result<Self, InvalidIdentifierReason> {
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

        return Ok(Self {
            ident: WithSpan { inner: ident, span },
        });
    }
}

impl<'a> Spanned for Ident<'a> {
    type Spanless = Ident<'a, spans::WithoutSpans>;

    fn erase_spans(self) -> Self::Spanless {
        Ident {
            ident: self.ident.erase_spans(),
        }
    }
}

impl<'a> AsRef<str> for Ident<'a> {
    fn as_ref(&self) -> &str {
        self.ident.inner
    }
}

#[perfect_derive(Debug)]
pub struct TemplatedIdent<'a, S: spans::Spanning = spans::WithSpans> {
    pub ident: Ident<'a, S>,
    pub args: Vec<Handle<Expression<'a, S>>>,
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
