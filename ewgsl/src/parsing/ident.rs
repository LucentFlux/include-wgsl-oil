use std::{
    fmt::Display,
    ops::{Deref, Range},
};

use perfect_derive::perfect_derive;

use super::{
    expression::{ExpressionArena, ExpressionHandle},
    text_spans::{self, Spanned, SpannedLeaf},
};

#[cfg(feature = "eq")]
use crate::EqIn;

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

/// A keyword in EWGSL. Is either a keyword or reserved word in core WGSL.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Keyword(usize);

impl Keyword {
    pub fn try_parse(kwd: &str) -> Option<Self> {
        let id = KEYWORDS.binary_search(&kwd).ok()?;
        Some(Self(id))
    }
}

impl Deref for Keyword {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        KEYWORDS[self.0]
    }
}

impl std::fmt::Debug for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Keyword").field(&*self).finish()
    }
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

/// A reserved word in EWGSL
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ReservedWord(usize);

impl ReservedWord {
    pub fn try_parse(kwd: &str) -> Option<Self> {
        let id = RESERVED_WORDS.binary_search(&kwd).ok()?;
        Some(Self(id))
    }
}

impl Deref for ReservedWord {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        RESERVED_WORDS[self.0]
    }
}

impl std::fmt::Debug for ReservedWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ReservedWord").field(&*self).finish()
    }
}

/// One of many reasons that a string might not be an identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InvalidIdentifierReason {
    WasEmpty,
    WasSingleUnderscore,
    StartedWithDoubleUnderscore,
    WasKeyword { keyword: Keyword },
    WasReservedWord { reserved_word: ReservedWord },
    InvalidStartCharacter { char: char },
    InvalidContainedCharacter { char: char, char_index: usize },
}

impl Display for InvalidIdentifierReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InvalidIdentifierReason::WasSingleUnderscore => {
                write!(f, "`_` is not a valid identifier in this context")
            }
            InvalidIdentifierReason::StartedWithDoubleUnderscore => {
                write!(f, "identifiers may not start with `__`")
            }
            InvalidIdentifierReason::WasKeyword { keyword } => {
                write!(
                    f,
                    "`{}` is a keyword and so cannot be used as an identifier",
                    keyword.deref()
                )
            }
            InvalidIdentifierReason::WasReservedWord { reserved_word } => {
                write!(
                    f,
                    "`{}` is a reserved word and so cannot be used as an identifier",
                    reserved_word.deref()
                )
            }
            InvalidIdentifierReason::InvalidStartCharacter { char } => write!(
                f,
                "`{}` is an invalid start character for an identifier",
                char.escape_debug()
            ),
            InvalidIdentifierReason::InvalidContainedCharacter { char, char_index } => write!(
                f,
                "`{}` is an invalid character for an identifier at index {char_index}",
                char.escape_debug(),
            ),
            InvalidIdentifierReason::WasEmpty => {
                write!(f, "empty string is not a valid identifier",)
            }
        }
    }
}

/// Some single word without spaces or punctuation, which isn't a keyword or reserved word,
/// and which doesn't start with "__".
#[perfect_derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Ident<'a, S: text_spans::SpanState = text_spans::SpansPresent>(SpannedLeaf<&'a str, S>);

impl<'a, S: text_spans::SpanState> Ident<'a, S> {
    fn validate(ident: &'a str) -> Result<&'a str, InvalidIdentifierReason> {
        if ident == "" {
            return Err(InvalidIdentifierReason::WasEmpty);
        }
        if ident == "_" {
            return Err(InvalidIdentifierReason::WasSingleUnderscore);
        }
        if ident.starts_with("__") {
            return Err(InvalidIdentifierReason::StartedWithDoubleUnderscore);
        }
        if let Some(keyword) = Keyword::try_parse(ident) {
            return Err(InvalidIdentifierReason::WasKeyword { keyword });
        }
        if let Some(reserved_word) = ReservedWord::try_parse(ident) {
            return Err(InvalidIdentifierReason::WasReservedWord { reserved_word });
        }
        if !ident.starts_with(|c: char| c == '\u{005F}' || unicode_ident::is_xid_start(c)) {
            return Err(InvalidIdentifierReason::InvalidStartCharacter {
                char: ident
                    .chars()
                    .next()
                    .expect("if started with, then has first"),
            });
        }
        for (char_index, char) in ident.chars().enumerate().skip(1) {
            if char != '\u{005F}' && !unicode_ident::is_xid_continue(char) {
                return Err(InvalidIdentifierReason::InvalidContainedCharacter {
                    char,
                    char_index,
                });
            }
        }

        return Ok(ident);
    }

    /// Tries to parse a string into a valid EWGLS identifier, erroring if the provided string is not a valid identifier.
    pub fn try_parse(ident: SpannedLeaf<&'a str, S>) -> Result<Self, InvalidIdentifierReason> {
        Self::validate(ident.inner())?;
        return Ok(Self(ident));
    }
}

#[cfg(feature = "span_erasure")]
impl<'a> Spanned for Ident<'a> {
    type Spanless = Ident<'a, text_spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        Ident(self.0.erase_spans())
    }
}

impl<'a, S: text_spans::SpanState> AsRef<str> for Ident<'a, S> {
    fn as_ref(&self) -> &str {
        self.0.inner()
    }
}

impl<'a> Deref for Ident<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.inner()
    }
}

#[perfect_derive(Debug, Clone)]
pub struct TemplatedIdent<'a, S: text_spans::SpanState = text_spans::SpansPresent> {
    pub ident: Ident<'a, S>,
    pub args: Range<ExpressionHandle<'a, S>>,
}

#[cfg(feature = "eq")]
impl<'a, S: text_spans::SpanState> EqIn<'a> for TemplatedIdent<'a, S> {
    type Context<'b> = ExpressionArena<'a, S> where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if self.ident != other.ident {
            return false;
        }

        if !self.args.eq_in(own_context, &other.args, other_context) {
            return false;
        }

        return true;
    }
}

impl<'a> Spanned for TemplatedIdent<'a, text_spans::SpansPresent> {
    #[cfg(feature = "span_erasure")]
    type Spanless = TemplatedIdent<'a, text_spans::SpansErased>;

    #[cfg(feature = "span_erasure")]
    fn erase_spans(self) -> Self::Spanless {
        TemplatedIdent {
            ident: self.ident.erase_spans(),
            args: self.args.erase_spans(),
        }
    }
}
