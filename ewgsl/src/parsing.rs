pub mod alias;
pub mod attributes;
pub mod const_assert;
pub mod directives;
pub mod expression;
pub mod ident;
pub mod structs;
pub mod variables;

use crate::{
    arena::{Arena, Handle, HandleRange},
    parsing::{alias::TypeAliasDeclaration, variables::GlobalVariableDeclaration},
    spans::{self, Spanned, WithSpan},
    EqIn,
};

use std::{
    fmt::{Debug, Display},
    mem::discriminant,
    num::NonZeroUsize,
    ops::Range,
};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, NoColor, StandardStream},
    },
};
use perfect_derive::perfect_derive;
use pest::{iterators::Pair, Parser};
use thiserror::Error;

use self::{
    attributes::{Attribute, AttributeIdentifier},
    const_assert::ConstAssertStatement,
    expression::Expression,
    ident::{Ident, TemplatedIdent},
    structs::{StructDeclaration, StructMember},
    variables::{GlobalConstantDeclaration, GlobalOverrideDeclaration},
};

mod parser {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "parsing/ewgsl.pest"]
    pub struct EwgslParser;
}

impl Display for parser::Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            parser::Rule::EOI => "end of file",
            parser::Rule::NEWLINE => "newline",
            parser::Rule::WHITESPACE => "whitespace",
            parser::Rule::LINE_COMMENT => "line comment",
            parser::Rule::MULTILINE_COMMENT => "multiline comment",
            parser::Rule::COMMENT => "comment",
            parser::Rule::BOOLEAN_LITERAL => "boolean literal",
            parser::Rule::DECIMAL_INT_LITERAL => "decimal int literal",
            parser::Rule::HEX_INT_LITERAL => "hex int literal",
            parser::Rule::INT_LITERAL => "int literal",
            parser::Rule::DECIMAL_FLOAT_LITERAL => "decimal float literal",
            parser::Rule::HEX_FLOAT_LITERAL => "hex float literal",
            parser::Rule::FLOAT_LITERAL => "float literal",
            parser::Rule::LITERAL => "literal",
            parser::Rule::START => "start",
            parser::Rule::CONTINUE => "continue",
            parser::Rule::IDENT => "identifier",
            parser::Rule::TEMPLATE_ARG_EXPRESSION => "template arg expression",
            parser::Rule::TEMPLATE_LIST => "template list",
            parser::Rule::ATTRIBUTE_INNER => "attribute inner",
            parser::Rule::ATTRIBUTE => "attribute",
            parser::Rule::ATTRIBUTE_SET => "collection of attributes",
            parser::Rule::DIAGNOSTIC_DIRECTIVE => "diagnostic directive",
            parser::Rule::ENABLE_EXTENSION_NAME => "enable extension name",
            parser::Rule::ENABLE_EXTENSION_LIST => "enable extension list",
            parser::Rule::ENABLE_KEYWORD => "`enable` keyword",
            parser::Rule::ENABLE_DIRECTIVE => "enable directive",
            parser::Rule::SOFTWARE_EXTENSION_NAME => "software extension name",
            parser::Rule::SOFTWARE_EXTENSION_LIST => "software extension list",
            parser::Rule::REQUIRES_KEYWORD => "`requires` keyword",
            parser::Rule::REQUIRES_DIRECTIVE => "requires directive",
            parser::Rule::GLOBAL_DIRECTIVE => "global directive",
            parser::Rule::TEMPLATE_ELABORATED_IDENT => "template elaborated identifier",
            parser::Rule::TYPE_SPECIFIER => "type specifier",
            parser::Rule::STRUCT_MEMBER => "struct member",
            parser::Rule::STRUCT_BODY_DECL => "struct body declaration",
            parser::Rule::STRUCT_KEYWORD => "`struct` keyword",
            parser::Rule::STRUCT_DECL => "struct declaration",
            parser::Rule::ALIAS_KEYWORD => "`alias` keyword",
            parser::Rule::TYPE_ALIAS_DECL => "type alias declaration",
            parser::Rule::OPTIONALLY_TYPED_IDENT => "optionally typed ident",
            parser::Rule::VARIABLE_DECL => "variable declaration",
            parser::Rule::VARIABLE_OR_VALUE_STATEMENT => "variable or value statement",
            parser::Rule::GLOBAL_VARIABLE_DECL => "global variable declaration",
            parser::Rule::GLOBAL_VALUE_DECL => "global value declaration",
            parser::Rule::CALL_PHRASE => "call phrase",
            parser::Rule::CALL_EXPRESSION => "call expression",
            parser::Rule::FUNC_CALL_STATEMENT => "func call statement",
            parser::Rule::PAREN_EXPRESSION => "paren expression",
            parser::Rule::PRIMARY_EXPRESSION => "primary expression",
            parser::Rule::RGBA_SWIZZLE_NAME => "rgba swizzle name",
            parser::Rule::XYZW_SWIZZLE_NAME => "xyzw swizzle name",
            parser::Rule::SWIZZLE_NAME => "swizzle name",
            parser::Rule::COMPONENT_OR_SWIZZLE_SPECIFIER => "component or swizzle specifier",
            parser::Rule::SINGULAR_EXPRESSION => "singular expression",
            parser::Rule::UNARY_EXPRESSION => "unary expression",
            parser::Rule::LHS_EXPRESSION => "lhs expression",
            parser::Rule::CORE_LHS_EXPRESSION => "core lhs expression",
            parser::Rule::MULTIPLICATIVE_OPERATOR => "multiplicative operator",
            parser::Rule::MULTIPLICATIVE_EXPRESSION => "multiplicative expression",
            parser::Rule::ADDITIVE_OPERATOR => "additive operator",
            parser::Rule::ADDITIVE_EXPRESSION => "additive expression",
            parser::Rule::_SHIFT_LEFT => "`<<`",
            parser::Rule::_SHIFT_RIGHT => "`>>`",
            parser::Rule::SHIFT_EXPRESSION => "shift expression",
            parser::Rule::TEMPLATE_SHIFT_EXPRESSION => "template shift expression",
            parser::Rule::_LESS_THAN => "`<`",
            parser::Rule::_GREATER_THAN => "`>`",
            parser::Rule::_LESS_THAN_EQUAL => "`<=`",
            parser::Rule::_GREATER_THAN_EQUAL => "`>=`",
            parser::Rule::RELATIONAL_EXPRESSION => "relational expression",
            parser::Rule::TEMPLATE_RELATIONAL_EXPRESSION => "template relational expression",
            parser::Rule::BITWISE_EXPRESSION => "bitwise expression",
            parser::Rule::EXPRESSION => "expression",
            parser::Rule::TEMPLATE_EXPRESSION => "template expression",
            parser::Rule::COMPOUND_STATEMENT => "compound statement",
            parser::Rule::ASSIGNMENT_STATEMENT => "assignment statement",
            parser::Rule::COMPOUND_ASSIGNMENT_OPERATOR => "compound assignment operator",
            parser::Rule::INCREMENT_STATEMENT => "increment statement",
            parser::Rule::DECREMENT_STATEMENT => "decrement statement",
            parser::Rule::IF_CLAUSE => "if clause",
            parser::Rule::ELSE_IF_CLAUSE => "else if clause",
            parser::Rule::ELSE_CLAUSE => "else clause",
            parser::Rule::IF_STATEMENT => "if statement",
            parser::Rule::DEFAULT_ALONE_CLAUSE => "default alone clause",
            parser::Rule::CASE_SELECTOR => "case selector",
            parser::Rule::CASE_SELECTORS => "case selectors",
            parser::Rule::CASE_KEYWORD => "`case` keyword",
            parser::Rule::CASE_CLAUSE => "case clause",
            parser::Rule::SWITCH_CLAUSE => "switch clause",
            parser::Rule::SWITCH_BODY => "switch body",
            parser::Rule::SWITCH_KEYWORD => "`switch` keyword",
            parser::Rule::SWITCH_STATEMENT => "switch statement",
            parser::Rule::LOOP_STATEMENT => "loop statement",
            parser::Rule::FOR_INIT => "for init",
            parser::Rule::FOR_UPDATE => "for update",
            parser::Rule::FOR_HEADER => "for header",
            parser::Rule::FOR_STATEMENT => "for statement",
            parser::Rule::WHILE_STATEMENT => "while statement",
            parser::Rule::BREAK_KEYWORD => "`break` keyword",
            parser::Rule::BREAK_STATEMENT => "break statement",
            parser::Rule::CONTINUE_STATEMENT => "continue statement",
            parser::Rule::BREAK_IF_STATEMENT => "break if statement",
            parser::Rule::CONTINUING_COMPOUND_STATEMENT => "continuing compound statement",
            parser::Rule::CONTINUING_STATEMENT => "continuing statement",
            parser::Rule::RETURN_STATEMENT => "return statement",
            parser::Rule::CONST_ASSERT_STATEMENT => "const assert statement",
            parser::Rule::VARIABLE_UPDATING_STATEMENT => "variable updating statement",
            parser::Rule::STATEMENT => "statement",
            parser::Rule::PARAM_LIST => "parameter list",
            parser::Rule::PARAM => "parameter",
            parser::Rule::FUNCTION_HEADER => "function header",
            parser::Rule::FUNCTION_DECL => "function declaration",
            parser::Rule::GLOBAL_DECL => "global declaration",
            parser::Rule::TRANSLATION_UNIT => "translation unit",
            parser::Rule::DIAGNOSTIC_KEYWORD => "`diagnostic` keyword",
            parser::Rule::CONST_KEYWORD => "`const` keyword",
            parser::Rule::OVERRIDE_KEYWORD => "`override` keyword",
            parser::Rule::_PLUS => "`+`",
            parser::Rule::_HYPHEN => "`-`",
            parser::Rule::_EXCLAMATION_MARK => "`!`",
            parser::Rule::_TILDE => "`~`",
            parser::Rule::_ASTERISK => "`*`",
            parser::Rule::_AMPERSAND => "`&`",
            parser::Rule::_PIPE => "`|`",
            parser::Rule::_FORWARD_SLASH => "`/`",
            parser::Rule::_PERCENT => "`%`",
            parser::Rule::_EQUAL_TO => "`==`",
            parser::Rule::_NOT_EQUAL_TO => "`!=`",
            parser::Rule::_DOUBLE_AND => "`&&`",
            parser::Rule::_DOUBLE_OR => "`||`",
            parser::Rule::_UPTICK => "`^`",
            parser::Rule::SHORT_CIRCUIT_EXPRESSION => "short circuit (binary) expression",
            parser::Rule::TEMPLATE_SHORT_CIRCUIT_EXPRESSION => {
                "template short circuit (binary) expression"
            }
            parser::Rule::VAR_KEYWORD => "`var` keyword",
            parser::Rule::LET_KEYWORD => "`let` keyword",
            parser::Rule::IF_KEYWORD => "`if` keyword",
            parser::Rule::ELSE_KEYWORD => "`else` keyword",
            parser::Rule::DEFAULT_KEYWORD => "`default` keyword",
            parser::Rule::LOOP_KEYWORD => "`loop` keyword",
            parser::Rule::FOR_KEYWORD => "`for` keyword",
            parser::Rule::WHILE_KEYWORD => "`while` keyword",
            parser::Rule::CONTINUE_KEYWORD => "`continue` keyword",
            parser::Rule::CONTINUING_KEYWORD => "`continuing` keyword",
            parser::Rule::RETURN_KEYWORD => "`return` keyword",
            parser::Rule::CONST_ASSERT_KEYWORD => "`const_assert` keyword",
            parser::Rule::FUNCTION_KEYWORD => "`fn` keyword",
            parser::Rule::INT_LITERAL_SUFFIX => "int literal suffix - `u` or `i`",
            parser::Rule::FLOAT_LITERAL_SUFFIX => "float literal suffix - `h` or `f`",
            parser::Rule::INDEX_EXPRESSION => "indexing expression",
        };
        write!(f, "{}", name)
    }
}

/// Takes a collection of rules, either found or expected, and allows them to be queried and formatted.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct RuleSet<'a> {
    rules: Vec<parser::Rule>,
    text: &'a str,
}

impl<'a> RuleSet<'a> {
    /// Gives a human-readable description of what would be required for the rules given by this set to be parsed.
    pub fn required(&self) -> String {
        format!("{}", self)
    }
}

impl<'a> Display for RuleSet<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.rules.len() == 0 {
            write!(f, "`{}`", self.text)?;
            return Ok(());
        }

        if self.rules.len() > 1 {
            write!(f, "one of ")?;
        }

        let mut is_first = true;
        for rule in &self.rules {
            if !is_first {
                write!(f, ", ")?;
            }
            write!(f, "`{}`", rule)?;
            is_first = false;
        }

        Ok(())
    }
}

/// Represents a single issue encountered while parsing the ewgsl file.
/// A parse error is composed of many issues.
#[derive(Debug, Error, Clone, Hash, PartialEq, Eq)]
pub enum ParseIssue<'a> {
    UnexpectedRule {
        expected: RuleSet<'a>,
        found: RuleSet<'a>,
    },
    UnknownSeverity {
        found: String,
    },
    UnknownEnableExtension {
        found: String,
    },
    UnknownSoftwareExtension {
        found: String,
    },
    UnknownAttributeIdentifier {
        found: String,
    },
    ExcessiveAttributeArgumentCount {
        attribute_ident: AttributeIdentifier,
        maximum: usize,
        found: usize,
    },
    InadequateAttributeArgumentCount {
        attribute_ident: AttributeIdentifier,
        minimum: usize,
        found: usize,
    },
    InvalidIdentifier {
        inner: ident::InvalidIdentifierReason,
        found: &'a str,
    },
}

impl<'a> Display for ParseIssue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseIssue::UnexpectedRule { expected, found } => {
                write!(f, "expected {} but found {found}", expected.required())
            }
            ParseIssue::UnknownSeverity { found } => write!(
                f,
                "invalid severity `{found}`: expected {}",
                directives::SeverityControlName::list_possible()
            ),
            ParseIssue::UnknownEnableExtension { found } => write!(
                f,
                "invalid enable extension `{found}`: expected {}",
                directives::EnableExtensionName::list_possible()
            ),
            ParseIssue::UnknownSoftwareExtension { found } => write!(
                f,
                "invalid software extension `{found}`: expected {}",
                directives::SoftwareExtensionName::list_possible()
            ),
            ParseIssue::UnknownAttributeIdentifier { found } => write!(
                f,
                "invalid attribute `{found}`: expected {}",
                attributes::AttributeIdentifier::list_possible()
            ),
            ParseIssue::ExcessiveAttributeArgumentCount {
                attribute_ident,
                maximum,
                found,
            } => {
                let attribute_ident = attribute_ident.encode();
                if *maximum == 0 {
                    write!(
                        f,
                        "attribute `{attribute_ident}` takes no arguments but found {found}",
                    )
                } else {
                    write!(
                        f,
                        "attribute `{attribute_ident}` takes {maximum} arguments but found {found}"
                    )
                }
            }
            ParseIssue::InadequateAttributeArgumentCount {
                attribute_ident,
                minimum,
                found,
            } => {
                let attribute_ident = attribute_ident.encode();
                if *found == 0 {
                    write!(
                        f,
                        "attribute `{attribute_ident}` takes {minimum} arguments but found none"
                    )
                } else {
                    write!(
                        f,
                        "attribute `{attribute_ident}` takes {minimum} arguments but found {found}"
                    )
                }
            }
            ParseIssue::InvalidIdentifier { inner, found } => match inner {
                ident::InvalidIdentifierReason::WasSingleUnderscore => {
                    write!(f, "`_` is not a valid identifier in this context")
                }
                ident::InvalidIdentifierReason::StartedWithDoubleUnderscore => {
                    write!(f, "identifiers may not start with `__`")
                }
                ident::InvalidIdentifierReason::WasKeyword => {
                    write!(
                        f,
                        "{found} is a keyword and so cannot be used as an identifier"
                    )
                }
                ident::InvalidIdentifierReason::WasReservedWord => {
                    write!(
                        f,
                        "{found} is a reserved word and so cannot be used as an identifier"
                    )
                }
            },
        }
    }
}

impl<'a> ParseIssue<'a> {
    pub fn recommend_alternative(&self) -> Option<&'static str> {
        match self {
            ParseIssue::UnknownSeverity { found } => {
                directives::SeverityControlName::get_recommended_alternative(found)
            }
            ParseIssue::UnknownEnableExtension { found } => {
                directives::EnableExtensionName::get_recommended_alternative(found)
            }
            ParseIssue::UnknownSoftwareExtension { found } => {
                directives::SoftwareExtensionName::get_recommended_alternative(found)
            }
            ParseIssue::UnknownAttributeIdentifier { found } => {
                attributes::AttributeIdentifier::get_recommended_alternative(found)
            }
            _ => None,
        }
    }
}

/// A vec with a length that isn't 0.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonEmptyVec<T>(Vec<T>);

impl<T> NonEmptyVec<T> {
    /// Creates a new NonEmptyVec, returning None if the provided vec is empty.
    pub fn new(v: Vec<T>) -> Option<Self> {
        if v.is_empty() {
            return None;
        }
        return Some(Self(v));
    }

    /// Generates a vec of size one from a single item
    fn of_one(v: T) -> Self {
        Self(vec![v])
    }

    /// Returns the underlying vec, which has at least one item.
    pub fn get(self) -> Vec<T> {
        self.0
    }

    pub fn len(&self) -> NonZeroUsize {
        // SAFETY: On construction we check the vector's length, and no methods allow mutation of the vec.
        unsafe { NonZeroUsize::new_unchecked(self.0.len()) }
    }
}

impl<T> AsRef<Vec<T>> for NonEmptyVec<T> {
    fn as_ref(&self) -> &Vec<T> {
        &self.0
    }
}

/// One or more issues encountered while parsing the module, and the module that was able to be parsed despite the issues.
#[derive(Debug)]
pub struct ParseError<'a> {
    /// The set of issues encountered while parsing the module. Guaranteed to not be empty.
    pub issues: NonEmptyVec<WithSpan<ParseIssue<'a>, spans::SpansPresent>>,
    /// The module that was able to be parsed. This is useful to extract definitions from to help debug errors in
    /// modules that import this one.
    pub partial_module: ParsedModule<'a>,
    /// The self-reported file name used when parsing the file
    pub file_name: &'a str,
    /// The sourcecode used when trying to parse the module.
    pub source_code: &'a str,
}

impl<'a> ParseError<'a> {
    /// Gives a set of format-able or output-able human readable details about the set of issues.
    pub fn diagnostics<'b>(&'b self) -> ErrorDiagnostics<'b> {
        let mut files = codespan_reporting::files::SimpleFiles::new();
        let file_id = files.add(self.file_name, self.source_code);

        let mut diagnostics = Vec::new();
        for issue in self.issues.as_ref().iter() {
            let label = format!("{}", issue.inner());

            // Notes go at the bottom of the message
            let mut notes = vec![];
            if let Some(alternative) = issue.inner().recommend_alternative() {
                notes.push(format!("help: did you mean `{}`?", alternative));
            }

            let diagnostic = Diagnostic::error()
                .with_message("failed to parse EWGSL source")
                .with_labels(vec![Label::primary(
                    file_id,
                    issue.span().start()..issue.span().end(),
                )
                .with_message(label)])
                .with_notes(notes);

            diagnostics.push(diagnostic);
        }

        return ErrorDiagnostics { files, diagnostics };
    }
}

/// A set of diagnostics generated from a [`ParseError`]. Use one of the `emit` methods, or format this object to
/// obtain a human-readable description of all of the parse errors.
pub struct ErrorDiagnostics<'a> {
    files: SimpleFiles<&'a str, &'a str>,
    diagnostics: Vec<Diagnostic<usize>>,
}

impl<'a> ErrorDiagnostics<'a> {
    /// Emits the set of errors contained in this diagnostic to the stream given.
    pub fn emit(&self, stream: &StandardStream) -> Result<(), std::io::Error> {
        let config = codespan_reporting::term::Config::default();

        for diagnostic in &self.diagnostics {
            if let Err(e) = term::emit(&mut stream.lock(), &config, &self.files, diagnostic) {
                // IO errors should be bubbled up - everything else we assume is a logic error on our part so don't expose
                match e {
                    codespan_reporting::files::Error::Io(io_error) => return Err(io_error),
                    _ => panic!("failed to emit error: {e:?}"),
                }
            }
        }

        return Ok(());
    }

    pub fn emit_to_stderr(&self) -> Result<(), std::io::Error> {
        let stream = StandardStream::stderr(ColorChoice::Always);
        self.emit(&stream)
    }

    pub fn emit_to_stdout(&self) -> Result<(), std::io::Error> {
        let stream = StandardStream::stdout(ColorChoice::Always);
        self.emit(&stream)
    }
}

impl<'a> Display for ErrorDiagnostics<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = Vec::new();
        let config = codespan_reporting::term::Config::default();

        for diagnostic in &self.diagnostics {
            if let Err(e) = term::emit(
                &mut NoColor::new(&mut buffer),
                &config,
                &self.files,
                diagnostic,
            ) {
                // IO errors should be bubbled up - everything else we assume is a logic error on our part so don't expose
                match e {
                    codespan_reporting::files::Error::Io(_) => return Err(std::fmt::Error),
                    _ => panic!("failed to emit error: {e:?}"),
                }
            }

            let as_string = std::str::from_utf8(&buffer)
                .expect("codespan_reporting should always generate valid UTF-8 strings");
            write!(f, "{}", as_string)?;

            buffer.clear();
        }

        return Ok(());
    }
}

pub type ParseResult<'a> = Result<ParsedModule<'a>, ParseError<'a>>;

#[derive(Debug)]
pub enum GlobalDeclaration<'a, S: spans::SpanState = spans::SpansPresent> {
    Variable(GlobalVariableDeclaration<'a, S>),
    Constant(GlobalConstantDeclaration<'a, S>),
    Override(GlobalOverrideDeclaration<'a, S>),
    Alias(TypeAliasDeclaration<'a, S>),
    Struct(StructDeclaration<'a, S>),
    ConstAssert(ConstAssertStatement<'a, S>),
}

impl<'a> Spanned for GlobalDeclaration<'a> {
    type Spanless = GlobalDeclaration<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        match self {
            GlobalDeclaration::Variable(v) => GlobalDeclaration::Variable(v.erase_spans()),
            GlobalDeclaration::Constant(c) => GlobalDeclaration::Constant(c.erase_spans()),
            GlobalDeclaration::Override(o) => GlobalDeclaration::Override(o.erase_spans()),
            GlobalDeclaration::Alias(a) => GlobalDeclaration::Alias(a.erase_spans()),
            GlobalDeclaration::Struct(s) => GlobalDeclaration::Struct(s.erase_spans()),
            GlobalDeclaration::ConstAssert(s) => GlobalDeclaration::ConstAssert(s.erase_spans()),
        }
    }
}

impl<'a, S: spans::SpanState> EqIn<'a> for GlobalDeclaration<'a, S> {
    type Context<'b> = (&'b Arena<Attribute<'a, S>, S>, &'b Arena<Expression<'a, S>, S>, &'b Arena<StructMember<'a, S>, S>)
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        if discriminant(self) != discriminant(other) {
            return false;
        }

        match (self, other) {
            (GlobalDeclaration::Variable(lhs), GlobalDeclaration::Variable(rhs)) => lhs.eq_in(
                &(own_context.0, own_context.1),
                rhs,
                &(other_context.0, other_context.1),
            ),
            (GlobalDeclaration::Constant(lhs), GlobalDeclaration::Constant(rhs)) => {
                lhs.eq_in(own_context.1, rhs, other_context.1)
            }
            (GlobalDeclaration::Override(lhs), GlobalDeclaration::Override(rhs)) => lhs.eq_in(
                &(own_context.0, own_context.1),
                rhs,
                &(other_context.0, other_context.1),
            ),
            (GlobalDeclaration::Alias(lhs), GlobalDeclaration::Alias(rhs)) => {
                lhs.eq_in(own_context.1, rhs, other_context.1)
            }
            (GlobalDeclaration::Struct(lhs), GlobalDeclaration::Struct(rhs)) => lhs.eq_in(
                &(own_context.0, own_context.1, own_context.2),
                rhs,
                &(other_context.0, other_context.1, other_context.2),
            ),
            (GlobalDeclaration::ConstAssert(lhs), GlobalDeclaration::ConstAssert(rhs)) => {
                lhs.eq_in(own_context.1, rhs, other_context.1)
            }
            _ => unreachable!(),
        }
    }
}

/// A single module (source file), many of which can be unified into a shader.
#[perfect_derive(Debug)]
pub struct ParsedModule<'a, S: spans::SpanState = spans::SpansPresent> {
    pub directives: directives::Directives<S>,
    pub attributes: Arena<Attribute<'a, S>, S>,
    pub expressions: Arena<Expression<'a, S>, S>,
    pub members: Arena<StructMember<'a, S>, S>,
    pub declarations: Arena<GlobalDeclaration<'a, S>, S>,
}

impl<'a, S: spans::SpanState> ParsedModule<'a, S> {
    /// A module with no content. Equal to `ParsedModule::parse("")`.
    pub fn empty() -> Self {
        Self {
            directives: directives::Directives::empty(),
            attributes: Arena::new(),
            expressions: Arena::new(),
            members: Arena::new(),
            declarations: Arena::new(),
        }
    }
}

impl<'a> ParsedModule<'a, spans::SpansPresent> {
    pub fn parse(file_name: &'a str, source_code: &'a str) -> ParseResult<'a> {
        // Lex and process errors
        let parse_res = parser::EwgslParser::parse(parser::Rule::TRANSLATION_UNIT, source_code);
        let mut translation_units = match parse_res {
            Ok(pairs) => pairs,
            Err(parse_error) => {
                let span = spans::Span::from(parse_error.location);

                let text = if span.start() >= source_code.len() {
                    "end of file"
                } else {
                    &source_code[span.start()..span.end()]
                };

                let error = match parse_error.variant {
                    pest::error::ErrorVariant::ParsingError {
                        positives,
                        negatives,
                    } => ParseIssue::UnexpectedRule {
                        expected: RuleSet {
                            rules: positives,
                            text,
                        },
                        found: RuleSet {
                            rules: negatives,
                            text,
                        },
                    },
                    // We dont use custom errors
                    pest::error::ErrorVariant::CustomError { message: _ } => unreachable!(),
                };
                return Err(ParseError {
                    issues: NonEmptyVec::of_one(WithSpan::new(error, span)),
                    partial_module: ParsedModule::empty(),
                    file_name,
                    source_code,
                });
            }
        };

        // We should always have exactly one translation unit
        let translation_unit = translation_units
            .next()
            .expect("one translation unit per str");
        assert!(translation_units.next().is_none());

        return Self::parse_translation_unit(translation_unit, file_name, source_code);
    }

    // A translation unit is the root object. This method takes a parsed root and maps it into our module arenas object.
    fn parse_translation_unit(
        translation_unit: Pair<'a, parser::Rule>,
        file_name: &'a str,
        source_code: &'a str,
    ) -> ParseResult<'a> {
        debug_assert_eq!(translation_unit.as_rule(), parser::Rule::TRANSLATION_UNIT);

        let mut module = ParsedModule::empty();

        let mut rules = translation_unit.into_inner().peekable();

        // We try to parse as much as we can, and bundle errors together.
        let mut issues = vec![];

        // First directives
        while rules
            .peek()
            .is_some_and(|inner| inner.as_rule() == parser::Rule::GLOBAL_DIRECTIVE)
        {
            Self::parse_global_directive(
                &mut module,
                &mut issues,
                rules.next().expect("checked was some"),
            );
        }

        // Then declarations
        while rules
            .peek()
            .is_some_and(|inner| inner.as_rule() == parser::Rule::GLOBAL_DECL)
        {
            Self::parse_global_decl(
                &mut module,
                &mut issues,
                rules.next().expect("checked was some"),
            );
        }

        // At this point, nothing else should follow. The parser makes sure of this, so this keeps parity with the parser.
        assert!(rules
            .next()
            .is_some_and(|pair| pair.as_rule() == parser::Rule::EOI));
        if !issues.is_empty() {
            return Err(ParseError {
                issues: NonEmptyVec::new(issues).expect("just checked not empty"),
                partial_module: module,
                file_name,
                source_code,
            });
        }
        return Ok(module);
    }

    // Directives are things like `enable`, `require` and `diagnostics` statements.
    fn parse_global_directive(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        global_directive: Pair<'_, parser::Rule>,
    ) {
        debug_assert_eq!(global_directive.as_rule(), parser::Rule::GLOBAL_DIRECTIVE);

        let mut rules = global_directive.into_inner();
        let inner_directive = rules.next().expect("global directive is a switch type");
        assert!(rules.next().is_none());

        match inner_directive.as_rule() {
            // Of the form `diagnostic [ident];`
            parser::Rule::DIAGNOSTIC_DIRECTIVE => {
                let span = inner_directive.as_span().into();
                let name = inner_directive
                    .into_inner()
                    .skip(1) // Skip the keyword
                    .next()
                    .expect("diagnostics have names");
                debug_assert_eq!(name.as_rule(), parser::Rule::IDENT);

                let severity = match directives::SeverityControlName::parse(name.as_str()) {
                    Some(severity) => severity,
                    None => {
                        issues.push(WithSpan::new(
                            ParseIssue::UnknownSeverity {
                                found: name.as_str().to_owned(),
                            },
                            name.as_span().into(),
                        ));
                        return;
                    }
                };

                module
                    .directives
                    .diagnostics
                    .append(WithSpan::new(severity, span));
            }
            // Of the form `enable [ident1], [ident2]...;`
            parser::Rule::ENABLE_DIRECTIVE => {
                let names = inner_directive
                    .into_inner()
                    .skip(1) // Skip the keyword
                    .next()
                    .unwrap();
                debug_assert_eq!(names.as_rule(), parser::Rule::ENABLE_EXTENSION_LIST);

                for name in names.into_inner() {
                    debug_assert_eq!(name.as_rule(), parser::Rule::ENABLE_EXTENSION_NAME);

                    let extension = match directives::EnableExtensionName::parse(name.as_str()) {
                        Some(extension) => extension,
                        None => {
                            issues.push(WithSpan::new(
                                ParseIssue::UnknownEnableExtension {
                                    found: name.as_str().to_owned(),
                                },
                                name.as_span().into(),
                            ));
                            continue;
                        }
                    };

                    module
                        .directives
                        .enable_extensions
                        .append(WithSpan::new(extension, name.as_span().into()));
                }
            }
            // Of the form `requires [ident1], [ident2]...;`
            parser::Rule::REQUIRES_DIRECTIVE => {
                let names = inner_directive
                    .into_inner()
                    .skip(1) // Skip the keyword
                    .next()
                    .unwrap();
                debug_assert_eq!(names.as_rule(), parser::Rule::SOFTWARE_EXTENSION_LIST);

                for name in names.into_inner() {
                    debug_assert_eq!(name.as_rule(), parser::Rule::SOFTWARE_EXTENSION_NAME);

                    let extension = match directives::SoftwareExtensionName::parse(name.as_str()) {
                        Some(extension) => extension,
                        None => {
                            issues.push(WithSpan::new(
                                ParseIssue::UnknownSoftwareExtension {
                                    found: name.as_str().to_owned(),
                                },
                                name.as_span().into(),
                            ));
                            continue;
                        }
                    };

                    module
                        .directives
                        .software_extensions
                        .append(WithSpan::new(extension, name.as_span().into()));
                }
            }
            _ => unreachable!(),
        }
    }

    // Declarations are constants, functions and types, and whatever else isn't a directive.
    fn parse_global_decl(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        global_decl: Pair<'a, parser::Rule>,
    ) {
        debug_assert_eq!(global_decl.as_rule(), parser::Rule::GLOBAL_DECL);

        // Declarations can be empty
        if global_decl.as_str() == ";" {
            return;
        }

        let inner_rule = global_decl
            .into_inner()
            .next()
            .expect("every non-empty declaration has a single sub-rule to switch on");
        match inner_rule.as_rule() {
            parser::Rule::GLOBAL_VARIABLE_DECL => {
                Self::parse_global_variable_decl(module, issues, inner_rule);
            }
            parser::Rule::GLOBAL_VALUE_DECL => {
                Self::parse_global_value_decl(module, issues, inner_rule);
            }
            parser::Rule::TYPE_ALIAS_DECL => {
                Self::parse_type_alias_decl(module, issues, inner_rule);
            }
            parser::Rule::STRUCT_DECL => {
                Self::parse_struct_decl(module, issues, inner_rule);
            }
            parser::Rule::FUNCTION_DECL => {
                Self::parse_function_decl(module, issues, inner_rule);
            }
            parser::Rule::CONST_ASSERT_STATEMENT => {
                Self::parse_const_assert_decl(module, issues, inner_rule);
            }
            _ => unreachable!(),
        }
    }

    // An identifier. Just some characters without any templating or anything else.
    fn parse_ident(
        _module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        ident: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Ident<'a>>, ()> {
        debug_assert_eq!(ident.as_rule(), parser::Rule::IDENT);
        let span = ident.as_span().into();

        let res = Ident::try_parse(ident.as_str());
        return match res {
            Err(e) => {
                issues.push(WithSpan::new(
                    ParseIssue::InvalidIdentifier {
                        inner: e,
                        found: ident.as_str(),
                    },
                    span,
                ));
                return Err(());
            }
            Ok(res) => Ok(WithSpan::new(res, span)),
        };
    }

    // A collection of template parameters, separated by commas
    fn parse_template_list(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        template_list: Pair<'a, parser::Rule>,
    ) -> Result<Vec<WithSpan<Expression<'a>>>, ()> {
        debug_assert_eq!(template_list.as_rule(), parser::Rule::TEMPLATE_LIST);

        let mut expressions = Vec::new();
        for arg in template_list.into_inner() {
            debug_assert_eq!(arg.as_rule(), parser::Rule::TEMPLATE_ARG_EXPRESSION);

            let expression = arg.into_inner().next().unwrap();
            let expression = Self::parse_expression(module, issues, expression);
            expressions.push(expression);
        }

        // Bubble None to top
        return expressions.into_iter().collect();
    }

    // An identifier possibly followed by some <template, args>
    fn parse_templated_ident(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        ident: Pair<'a, parser::Rule>,
    ) -> Result<TemplatedIdent<'a>, ()> {
        debug_assert_eq!(ident.as_rule(), parser::Rule::TEMPLATE_ELABORATED_IDENT);

        let mut inner = ident.into_inner();

        // First the identifier
        let ident = inner.next().unwrap();
        let ident = Self::parse_ident(module, issues, ident)?;

        // Then, optionally, the template args
        let args = if let Some(template_list) = inner.next() {
            Self::parse_template_list(module, issues, template_list)?
        } else {
            vec![]
        };

        let args = module.expressions.append_all(args);

        return Ok(TemplatedIdent { ident, args });
    }

    /// This method abstracts over expressions of the form
    /// ```pest
    /// EXPR = {
    ///     A
    ///     | B
    ///     | C
    ///     | ...
    /// }
    /// ```
    ///
    fn parse_switch_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
        parse_options: impl FnOnce(
            parser::Rule,
        ) -> Box<
            dyn FnOnce(
                &mut ParsedModule<'a>,
                &mut Vec<WithSpan<ParseIssue<'a>>>,
                Pair<'a, parser::Rule>,
            ) -> Result<WithSpan<Expression<'a>>, ()>,
        >,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        let inner_rule = expression.into_inner().next().unwrap();

        let parse_method = parse_options(inner_rule.as_rule());
        return parse_method(module, issues, inner_rule);
    }

    /// This method abstracts over expressions of the form
    /// ```pest
    /// EXPR = {
    ///       (FOO ~ OP1 ~ FOO)
    ///     | (FOO ~ OP2 ~ FOO)
    ///     | ...
    ///     | FOO
    /// }
    /// ```
    fn parse_singular_binary_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
        mut parse_sides: impl FnMut(
            &mut ParsedModule<'a>,
            &mut Vec<WithSpan<ParseIssue<'a>>>,
            Pair<'a, parser::Rule>,
        ) -> Result<WithSpan<Expression<'a>>, ()>,
        operator: impl FnOnce(parser::Rule) -> expression::BinaryOperator,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        let expression_span = expression.as_span().into();

        let mut inner = expression.into_inner();

        // Always a lhs, optionally a rhs
        let lhs = inner.next().unwrap();
        let mut lhs = parse_sides(module, issues, lhs);

        if let Some(op) = inner.next() {
            // Parse operator
            let op_span = op.as_span().into();
            let op = operator(op.as_rule());

            let op = WithSpan::new(op, op_span);

            // Parse rhs
            let rhs = inner.next().unwrap();
            let rhs = parse_sides(module, issues, rhs)?;
            let rhs_handle = module.expressions.append(rhs);

            if let Ok(lhs) = lhs.as_mut() {
                let lhs_handle = module.expressions.append(lhs.clone());
                *lhs = WithSpan::new(
                    Expression::Binary {
                        lhs: lhs_handle,
                        op,
                        rhs: rhs_handle,
                    },
                    expression_span,
                );
            }
        }

        return lhs;
    }

    /// This method abstracts over expressions of the form
    /// ```pest
    /// EXPR = {
    ///     FOO ~ ((_OP1 ~ FOO)+ | (_OP2 ~ FOO)+ | (_OP3 ~ FOO)+)
    /// }
    /// ```
    fn parse_iterated_binary_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
        mut parse_sides: impl FnMut(
            &mut ParsedModule<'a>,
            &mut Vec<WithSpan<ParseIssue<'a>>>,
            Pair<'a, parser::Rule>,
        ) -> Result<WithSpan<Expression<'a>>, ()>,
        operator: impl FnOnce(parser::Rule) -> expression::BinaryOperator,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        let expression_span = expression.as_span().into();

        let mut inner = expression.into_inner();

        let lhs = inner.next().unwrap();
        let lhs = parse_sides(module, issues, lhs);

        let op = inner.next().unwrap();
        let op_span = op.as_span().into();
        let op = operator(op.as_rule());
        let op = WithSpan::new(op, op_span);

        let mut rhs_list = vec![];
        while let Some(rhs) = inner.next() {
            let rhs = parse_sides(module, issues, rhs);
            rhs_list.push(rhs);

            // Skip next op, if there is one
            inner.next();
        }

        // Only now escape if something errored, so that we still got the errors of everything else
        let mut lhs = lhs?;
        let rhs_list = rhs_list.into_iter().collect::<Result<Vec<_>, ()>>()?;

        // Fold
        for rhs in rhs_list {
            let lhs_handle = module.expressions.append(lhs);
            let rhs_handle = module.expressions.append(rhs);
            let lhs_expr = Expression::Binary {
                lhs: lhs_handle,
                op,
                rhs: rhs_handle,
            };
            lhs = WithSpan::new(lhs_expr, expression_span);
        }

        return Ok(lhs);
    }

    /// This method abstracts over expressions of the form
    /// ```pest
    /// OP = {
    ///     OP1 | OP2
    /// }
    /// EXPR = {
    ///     FOO ~ (OP ~ FOO)*
    /// }
    /// ```
    fn parse_iterated_switch_binary_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
        mut parse_sides: impl FnMut(
            &mut ParsedModule<'a>,
            &mut Vec<WithSpan<ParseIssue<'a>>>,
            Pair<'a, parser::Rule>,
        ) -> Result<WithSpan<Expression<'a>>, ()>,
        mut operator: impl FnMut(parser::Rule) -> expression::BinaryOperator,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        let expression_span = expression.as_span().into();

        let mut inner = expression.into_inner();

        let lhs = inner.next().unwrap();
        let lhs = parse_sides(module, issues, lhs);

        let mut rhs_list = vec![];
        while let Some(op) = inner.next() {
            let op = op.into_inner().next().unwrap(); // Unswitch
            let op_span = op.as_span().into();
            let op = operator(op.as_rule());
            let op = WithSpan::new(op, op_span);

            // Then get rhs
            let rhs = inner.next().unwrap();
            let rhs = parse_sides(module, issues, rhs);
            rhs_list.push(rhs.map(|rhs| (op, rhs)));
        }

        // Only now escape if something errored, so that we still got the errors of everything else
        let mut lhs = lhs?;
        let rhs_list = rhs_list.into_iter().collect::<Result<Vec<_>, ()>>()?;

        // Fold
        for (op, rhs) in rhs_list {
            let lhs_handle = module.expressions.append(lhs);
            let rhs_handle = module.expressions.append(rhs);
            let lhs_expr = Expression::Binary {
                lhs: lhs_handle,
                op,
                rhs: rhs_handle,
            };
            lhs = WithSpan::new(lhs_expr, expression_span);
        }

        return Ok(lhs);
    }

    // Something of the form `foo<a, b>(x, y)`
    fn parse_call_prase(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        call_phrase: Pair<'a, parser::Rule>,
    ) -> Result<expression::CallPhrase<'a>, ()> {
        debug_assert_eq!(call_phrase.as_rule(), parser::Rule::CALL_PHRASE);

        let mut inner = call_phrase.into_inner();

        // First the identifier
        let ident = inner.next().unwrap();
        let ident = Self::parse_templated_ident(module, issues, ident)?;

        // Then any number of arguments
        // Try to parse them all still, even when one errors.
        let mut args = vec![];
        for arg in inner {
            let arg = Self::parse_expression(module, issues, arg);
            args.push(arg);
        }
        let args = args.into_iter().collect::<Result<Vec<_>, ()>>()?;

        let args = module.expressions.append_all(args);

        return Ok(expression::CallPhrase { ident, args });
    }

    fn parse_literal_expression(
        _module: &mut ParsedModule<'a>,
        _issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert_eq!(expression.as_rule(), parser::Rule::LITERAL);
        let expression_span = expression.as_span().into();

        let literal = expression.into_inner().next().unwrap();
        let literal = match literal.as_rule() {
            parser::Rule::INT_LITERAL => expression::Literal::Int(literal.as_str()),
            parser::Rule::FLOAT_LITERAL => expression::Literal::Float(literal.as_str()),
            parser::Rule::BOOLEAN_LITERAL => {
                expression::Literal::Boolean(literal.as_str() == "true")
            }
            _ => unreachable!(),
        };

        return Ok(WithSpan::new(
            Expression::Literal { value: literal },
            expression_span,
        ));
    }

    fn parse_paren_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert_eq!(expression.as_rule(), parser::Rule::PAREN_EXPRESSION);
        let expression_span = expression.as_span().into();

        let mut expression = expression.into_inner();

        let inner_expr = expression.next().unwrap();
        let inner_expr = Self::parse_expression(module, issues, inner_expr)?;
        debug_assert!(expression.next().is_none());

        let inner_expr = module.expressions.append(inner_expr);
        return Ok(WithSpan::new(
            Expression::Parenthesized(inner_expr),
            expression_span,
        ));
    }

    fn parse_call_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert_eq!(expression.as_rule(), parser::Rule::CALL_EXPRESSION);
        let expression_span = expression.as_span().into();

        let mut expression = expression.into_inner();
        let call_phrase = Self::parse_call_prase(module, issues, expression.next().unwrap())?;
        debug_assert!(expression.next().is_none());

        return Ok(WithSpan::new(
            Expression::Call(call_phrase),
            expression_span,
        ));
    }

    fn parse_ident_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert_eq!(
            expression.as_rule(),
            parser::Rule::TEMPLATE_ELABORATED_IDENT
        );
        let expression_span = expression.as_span().into();

        let ident = Self::parse_templated_ident(module, issues, expression)?;

        return Ok(WithSpan::new(
            Expression::Identifier { ident },
            expression_span,
        ));
    }

    fn parse_primary_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert_eq!(expression.as_rule(), parser::Rule::PRIMARY_EXPRESSION);

        Self::parse_switch_expression(module, issues, expression, |rule| match rule {
            parser::Rule::TEMPLATE_ELABORATED_IDENT => Box::new(Self::parse_ident_expression),
            parser::Rule::CALL_EXPRESSION => Box::new(Self::parse_call_expression),
            parser::Rule::LITERAL => Box::new(Self::parse_literal_expression),
            parser::Rule::PAREN_EXPRESSION => Box::new(Self::parse_paren_expression),
            _ => unreachable!(),
        })
    }

    // Accessors are swizzles, members or index expressions, and can be stacked on top of an expression.
    fn parse_accessor_on_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        accessor: Pair<'a, parser::Rule>,
        base_expression: Handle<Expression<'a>>,
        composed_span: spans::Span,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        assert_eq!(
            accessor.as_rule(),
            parser::Rule::COMPONENT_OR_SWIZZLE_SPECIFIER
        );
        let accessor_span = accessor.as_span().into();

        let mut accessor = accessor.into_inner();

        let inner = accessor.next().unwrap();
        let accessed = match inner.as_rule() {
            parser::Rule::INDEX_EXPRESSION => {
                let index_expression = inner.into_inner().next().unwrap();
                let index_expression = Self::parse_expression(module, issues, index_expression)?;
                let index_expression = module.expressions.append(index_expression);
                Expression::Index {
                    base: base_expression,
                    index: index_expression,
                }
            }
            parser::Rule::SWIZZLE_NAME => {
                let swizzle = expression::Swizzle::try_parse(inner.as_str())
                    .expect("swizzle pattern only matches valid swizzles");
                Expression::Swizzle {
                    base: base_expression,
                    swizzle: WithSpan::new(swizzle, accessor_span),
                }
            }
            parser::Rule::IDENT => {
                let member_ident = inner.as_str();
                Expression::MemberAccess {
                    base: base_expression,
                    member: WithSpan::new(member_ident, accessor_span),
                }
            }
            _ => unreachable!(),
        };
        debug_assert!(accessor.next().is_none());

        return Ok(WithSpan::new(accessed, composed_span));
    }

    fn parse_singular_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert_eq!(expression.as_rule(), parser::Rule::SINGULAR_EXPRESSION);

        let mut inner = expression.into_inner();

        // A primary expression followed by 1 or more accessors
        let inner_expr = inner.next().unwrap();
        let mut inner_span: spans::Span = inner_expr.as_span().into();
        let mut inner_expr = Self::parse_primary_expression(module, issues, inner_expr)?;

        // If we find accessors, hand off to `parse_accessor_on_expression`
        for accessor in inner {
            inner_span = inner_span.union(accessor.as_span().into());
            let base_expression = module.expressions.append(inner_expr);
            inner_expr = Self::parse_accessor_on_expression(
                module,
                issues,
                accessor,
                base_expression,
                inner_span,
            )?;
        }

        return Ok(inner_expr);
    }

    fn parse_unary_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert_eq!(expression.as_rule(), parser::Rule::UNARY_EXPRESSION);
        let expression_span = expression.as_span().into();

        let mut inner = expression.into_inner();

        // We have either 0 or 1 preceeding unary operators
        let first = inner.next().unwrap();
        let op_span = first.as_span().into();
        let op = match first.as_rule() {
            parser::Rule::SINGULAR_EXPRESSION => {
                return Self::parse_singular_expression(module, issues, first)
            }
            // Else it will be a unary op
            parser::Rule::_HYPHEN => expression::UnaryOperator::Minus,
            parser::Rule::_EXCLAMATION_MARK => expression::UnaryOperator::Not,
            parser::Rule::_TILDE => expression::UnaryOperator::Invert,
            parser::Rule::_AMPERSAND => expression::UnaryOperator::Reference,
            parser::Rule::_ASTERISK => expression::UnaryOperator::Dereference,
            _ => unreachable!(),
        };
        let op = WithSpan::new(op, op_span);

        // Then the expression - recurse
        let expression = inner.next().unwrap();
        debug_assert!(inner.next().is_none());
        let expr = Self::parse_unary_expression(module, issues, expression)?;

        // Create and return
        let expr = module.expressions.append(expr);
        let expr = Expression::Unary { op, expr };
        return Ok(WithSpan::new(expr, expression_span));
    }

    fn parse_bitwise_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert_eq!(expression.as_rule(), parser::Rule::BITWISE_EXPRESSION);

        Self::parse_iterated_binary_expression(
            module,
            issues,
            expression,
            Self::parse_unary_expression,
            |op| match op {
                parser::Rule::_AMPERSAND => expression::BinaryOperator::And,
                parser::Rule::_PIPE => expression::BinaryOperator::Or,
                parser::Rule::_UPTICK => expression::BinaryOperator::Xor,
                _ => unreachable!(),
            },
        )
    }

    fn parse_short_circuit_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert!(
            expression.as_rule() == parser::Rule::SHORT_CIRCUIT_EXPRESSION
                || expression.as_rule() == parser::Rule::TEMPLATE_SHORT_CIRCUIT_EXPRESSION
        );

        Self::parse_iterated_binary_expression(
            module,
            issues,
            expression,
            Self::parse_shift_expression,
            |op| match op {
                parser::Rule::_DOUBLE_AND => expression::BinaryOperator::ShortCircuitAnd,
                parser::Rule::_DOUBLE_OR => expression::BinaryOperator::ShortCircuitOr,
                _ => unreachable!(),
            },
        )
    }

    fn parse_multiplicative_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert_eq!(
            expression.as_rule(),
            parser::Rule::MULTIPLICATIVE_EXPRESSION
        );

        Self::parse_iterated_switch_binary_expression(
            module,
            issues,
            expression,
            Self::parse_unary_expression,
            |rule| match rule {
                parser::Rule::_ASTERISK => expression::BinaryOperator::Multiply,
                parser::Rule::_FORWARD_SLASH => expression::BinaryOperator::Divide,
                parser::Rule::_PERCENT => expression::BinaryOperator::Modulo,
                _ => unreachable!(),
            },
        )
    }

    fn parse_additive_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert_eq!(expression.as_rule(), parser::Rule::ADDITIVE_EXPRESSION);

        Self::parse_iterated_switch_binary_expression(
            module,
            issues,
            expression,
            Self::parse_multiplicative_expression,
            |rule| match rule {
                parser::Rule::_PLUS => expression::BinaryOperator::Add,
                parser::Rule::_HYPHEN => expression::BinaryOperator::Subtract,
                _ => unreachable!(),
            },
        )
    }

    fn parse_shift_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert!(
            expression.as_rule() == parser::Rule::SHIFT_EXPRESSION
                || expression.as_rule() == parser::Rule::TEMPLATE_SHIFT_EXPRESSION
        );
        let expression_span = expression.as_span().into();

        let mut inner = expression.into_inner();

        // Can either be an addative expression, or a shift of two unary
        let lhs = inner.next().unwrap();
        let lhs = match lhs.as_rule() {
            parser::Rule::ADDITIVE_EXPRESSION => {
                return Self::parse_additive_expression(module, issues, lhs)
            }
            parser::Rule::UNARY_EXPRESSION => Self::parse_unary_expression(module, issues, lhs),
            _ => unreachable!(),
        };

        // ASSERT: Not an additive expr
        // Get op
        let op = inner.next().unwrap();
        let op_span = op.as_span().into();
        let op = match op.as_rule() {
            parser::Rule::_SHIFT_LEFT => expression::BinaryOperator::ShiftLeft,
            parser::Rule::_SHIFT_RIGHT => expression::BinaryOperator::ShiftRight,
            _ => unreachable!(),
        };
        let op = WithSpan::new(op, op_span);

        // Get rhs
        let rhs = Self::parse_unary_expression(module, issues, inner.next().unwrap())?;
        let lhs = lhs?;

        let lhs = module.expressions.append(lhs);
        let rhs = module.expressions.append(rhs);
        return Ok(WithSpan::new(
            Expression::Binary { lhs, op, rhs },
            expression_span,
        ));
    }

    fn parse_relational_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert!(
            expression.as_rule() == parser::Rule::RELATIONAL_EXPRESSION
                || expression.as_rule() == parser::Rule::TEMPLATE_RELATIONAL_EXPRESSION
        );

        Self::parse_singular_binary_expression(
            module,
            issues,
            expression,
            Self::parse_shift_expression,
            |op| match op {
                parser::Rule::_LESS_THAN_EQUAL => expression::BinaryOperator::LessThanEqual,
                parser::Rule::_GREATER_THAN_EQUAL => expression::BinaryOperator::GreaterThanEqual,
                parser::Rule::_LESS_THAN => expression::BinaryOperator::LessThan,
                parser::Rule::_GREATER_THAN => expression::BinaryOperator::GreaterThan,
                parser::Rule::_EQUAL_TO => expression::BinaryOperator::EqualTo,
                parser::Rule::_NOT_EQUAL_TO => expression::BinaryOperator::NotEqualTo,
                _ => unreachable!(),
            },
        )
    }

    fn parse_expression(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Expression<'a>>, ()> {
        debug_assert!(
            expression.as_rule() == parser::Rule::EXPRESSION
                || expression.as_rule() == parser::Rule::TEMPLATE_EXPRESSION
        );

        Self::parse_switch_expression(module, issues, expression, |rule| match rule {
            parser::Rule::BITWISE_EXPRESSION => Box::new(Self::parse_bitwise_expression),
            parser::Rule::SHORT_CIRCUIT_EXPRESSION
            | parser::Rule::TEMPLATE_SHORT_CIRCUIT_EXPRESSION => {
                Box::new(Self::parse_short_circuit_expression)
            }
            parser::Rule::RELATIONAL_EXPRESSION | parser::Rule::TEMPLATE_RELATIONAL_EXPRESSION => {
                Box::new(Self::parse_relational_expression)
            }
            _ => unreachable!(),
        })
    }

    /// Parses a single Rule::ATTRIBUTE
    fn parse_attribute(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        attribute: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<Attribute<'a>>, ()> {
        debug_assert_eq!(attribute.as_rule(), parser::Rule::ATTRIBUTE);
        let attribute_span = attribute.as_span().into();

        let attribute_inner = attribute
            .into_inner()
            .next()
            .expect("Rule::ATTRIBUTE has a single inner Rule::ATTRIBUTE_INNER");
        debug_assert_eq!(attribute_inner.as_rule(), parser::Rule::ATTRIBUTE_INNER);

        let mut attribute_inner = attribute_inner.into_inner();

        // Extract the keyword (after the `@`, before the parentheses)
        let keyword = attribute_inner
            .next()
            .expect("all attributes start with a keyword");
        debug_assert_eq!(keyword.as_rule(), parser::Rule::IDENT);
        let keyword_span = keyword.as_span().into();
        let identifier = match attributes::AttributeIdentifier::parse(keyword.as_str()) {
            Some(identifier) => WithSpan::new(identifier, keyword_span),
            None => {
                issues.push(WithSpan::new(
                    ParseIssue::UnknownAttributeIdentifier {
                        found: keyword.as_str().to_owned(),
                    },
                    keyword_span,
                ));
                return Err(());
            }
        };

        // Extract any arguments
        let mut expressions = Vec::new();
        while let Some(expression) = attribute_inner.next() {
            let expression = Self::parse_expression(module, issues, expression);
            expressions.push(expression);
        }
        let expressions = expressions.into_iter().collect::<Result<Vec<_>, ()>>();
        let expressions = match expressions {
            Ok(expressions) => expressions,
            Err(()) => return Err(()),
        };
        let expressions = expressions
            .into_iter()
            .map(|expression| module.expressions.append(expression))
            .collect::<Vec<_>>();

        // build and validate at the same time
        let attribute = match attributes::Attribute::try_parse_from(
            &*module,
            identifier,
            expressions.into_iter(),
        ) {
            Ok(attribute) => attribute,
            Err(mut attribute_issues) => {
                issues.append(&mut attribute_issues);
                return Err(());
            }
        };

        return Ok(WithSpan::new(attribute, attribute_span));
    }

    fn parse_attribute_set(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        attributes: Pair<'a, parser::Rule>,
    ) -> HandleRange<Attribute<'a>> {
        debug_assert_eq!(attributes.as_rule(), parser::Rule::ATTRIBUTE_SET);

        let mut found_attributes = vec![];
        for attribute in attributes.into_inner() {
            let attribute = Self::parse_attribute(module, issues, attribute);
            if let Ok(attribute) = attribute {
                found_attributes.push(attribute);
            }
        }

        return module.attributes.append_all(found_attributes);
    }

    fn parse_type_specifier(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        ty: Pair<'a, parser::Rule>,
    ) -> Result<variables::TypeSpecifier<'a>, ()> {
        assert_eq!(ty.as_rule(), parser::Rule::TYPE_SPECIFIER);

        let mut inner = ty.into_inner();
        let ident = Self::parse_templated_ident(module, issues, inner.next().unwrap())?;
        debug_assert!(inner.next().is_none());

        return Ok(variables::TypeSpecifier(ident));
    }

    fn parse_optionally_typed_ident(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        ident: Pair<'a, parser::Rule>,
    ) -> Result<variables::OptionallyTypedIdent<'a>, ()> {
        assert_eq!(ident.as_rule(), parser::Rule::OPTIONALLY_TYPED_IDENT);

        let mut inner = ident.into_inner();

        let ident = Self::parse_ident(module, issues, inner.next().unwrap());

        let mut ty = None;
        if let Some(ty_rule) = inner.next() {
            ty = Some(Self::parse_type_specifier(module, issues, ty_rule)?);
        }

        debug_assert!(inner.next().is_none());

        let ident = ident?;
        return Ok(variables::OptionallyTypedIdent { ident, ty });
    }

    fn parse_variable_decl(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        variable_decl: Pair<'a, parser::Rule>,
    ) -> Result<variables::VariableDeclaration<'a>, ()> {
        assert_eq!(variable_decl.as_rule(), parser::Rule::VARIABLE_DECL);

        let mut variable_decl = variable_decl.into_inner();

        // var (<...>)? (name (:ty)?)
        let _var_keyword = variable_decl.next();

        let mut template_list = Ok(vec![]);
        if variable_decl.peek().unwrap().as_rule() == parser::Rule::TEMPLATE_LIST {
            template_list =
                Self::parse_template_list(module, issues, variable_decl.next().unwrap());
        }

        let ident = variable_decl.next().unwrap();
        let ident = Self::parse_optionally_typed_ident(module, issues, ident);

        debug_assert!(variable_decl.next().is_none());

        let template_list = template_list?;
        let ident = ident?;

        let template_list = module.expressions.append_all(template_list);

        Ok(variables::VariableDeclaration {
            template_list,
            ident,
        })
    }

    // Global variable declarations are global objects that use the `var` keyword, like bindings or workgroup memory.
    fn parse_global_variable_decl(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        global_variable_decl: Pair<'a, parser::Rule>,
    ) {
        assert_eq!(
            global_variable_decl.as_rule(),
            parser::Rule::GLOBAL_VARIABLE_DECL
        );
        let global_variable_decl_span = global_variable_decl.as_span().into();

        let mut inner = global_variable_decl.into_inner();

        // attribute* lhs (= rhs)?
        let attributes = Self::parse_attribute_set(module, issues, inner.next().unwrap());
        let lhs = inner.next().unwrap();
        let lhs_span = lhs.as_span().into();
        let lhs = Self::parse_variable_decl(module, issues, lhs);
        let rhs = match inner.next() {
            None => None,
            Some(rhs) => match Self::parse_expression(module, issues, rhs) {
                Ok(rhs) => Some(rhs),
                Err(()) => return,
            },
        };
        debug_assert!(inner.next().is_none());

        // Try recover what we can from errors
        let lhs = match lhs {
            Ok(lhs) => lhs,
            Err(()) => return,
        };

        // Add to module
        let rhs = rhs.map(|rhs| module.expressions.append(rhs));
        let global_var_decl = GlobalVariableDeclaration {
            attributes,
            decl: WithSpan::new(lhs, lhs_span),
            init: rhs,
        };
        module.declarations.append(WithSpan::new(
            GlobalDeclaration::Variable(global_var_decl),
            global_variable_decl_span,
        ));
    }

    // Global value declarations are either `const` or `override` expressions.
    fn parse_global_value_decl(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        global_value_decl: Pair<'a, parser::Rule>,
    ) {
        debug_assert_eq!(global_value_decl.as_rule(), parser::Rule::GLOBAL_VALUE_DECL);
        let global_value_decl_span = global_value_decl.as_span().into();

        let mut inner = global_value_decl.into_inner();

        let is_const = inner.peek().unwrap().as_rule() == parser::Rule::CONST_KEYWORD;
        let attributes = if is_const {
            // Constant keyword
            debug_assert_eq!(inner.next().unwrap().as_rule(), parser::Rule::CONST_KEYWORD);

            module.attributes.append_all(vec![])
        } else {
            // Override expression
            let attributes = inner.next().unwrap();
            let attributes = Self::parse_attribute_set(module, issues, attributes);

            // Override keyword
            debug_assert_eq!(
                inner.next().unwrap().as_rule(),
                parser::Rule::OVERRIDE_KEYWORD
            );

            attributes
        };

        // Decl and init are shared by both constants and overrides
        let decl = inner.next().unwrap();
        let decl_span = decl.as_span().into();
        let decl = Self::parse_optionally_typed_ident(module, issues, decl);

        let init = inner
            .next()
            .map(|init| Self::parse_expression(module, issues, init));

        let decl = match decl {
            Ok(decl) => WithSpan::new(decl, decl_span),
            Err(()) => return,
        };
        let init = match init {
            None => None,
            Some(Ok(init)) => Some(init),
            Some(Err(())) => return,
        };

        if is_const {
            debug_assert_eq!(attributes.start.index(), attributes.end.index());
            let init = module
                .expressions
                .append(init.expect("all constants have an initial value"));
            let const_decl = variables::GlobalConstantDeclaration { decl, init };
            module.declarations.append(WithSpan::new(
                GlobalDeclaration::Constant(const_decl),
                global_value_decl_span,
            ));
        } else {
            let init = init.map(|init| module.expressions.append(init));
            let override_decl = variables::GlobalOverrideDeclaration {
                attributes,
                decl,
                init,
            };
            module.declarations.append(WithSpan::new(
                GlobalDeclaration::Override(override_decl),
                global_value_decl_span,
            ));
        }
    }

    // Type alias declarations use the `alias` keyword.
    fn parse_type_alias_decl(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        type_alias_decl: Pair<'a, parser::Rule>,
    ) {
        debug_assert_eq!(type_alias_decl.as_rule(), parser::Rule::TYPE_ALIAS_DECL);
        let alias_span = type_alias_decl.as_span().into();

        let mut inner = type_alias_decl.into_inner();

        debug_assert_eq!(inner.next().unwrap().as_rule(), parser::Rule::ALIAS_KEYWORD);

        let lhs = inner.next().unwrap();
        let lhs = Self::parse_ident(module, issues, lhs);

        let rhs = inner.next().unwrap();
        let rhs = Self::parse_type_specifier(module, issues, rhs);

        debug_assert!(inner.next().is_none());

        let lhs = match lhs {
            Ok(lhs) => lhs,
            Err(()) => return,
        };
        let rhs = match rhs {
            Ok(rhs) => rhs,
            Err(()) => return,
        };

        let alias_decl = TypeAliasDeclaration {
            ident: lhs,
            ty: rhs,
        };

        module.declarations.append(WithSpan::new(
            GlobalDeclaration::Alias(alias_decl),
            alias_span,
        ));
    }

    // A member of a struct definition
    fn parse_struct_member(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        struct_member: Pair<'a, parser::Rule>,
    ) -> Result<WithSpan<StructMember<'a>>, ()> {
        debug_assert_eq!(struct_member.as_rule(), parser::Rule::STRUCT_MEMBER);
        let struct_member_span = struct_member.as_span().into();

        let mut inner = struct_member.into_inner();

        let attributes = Self::parse_attribute_set(module, issues, inner.next().unwrap());
        let ident = Self::parse_ident(module, issues, inner.next().unwrap());
        let ty = Self::parse_type_specifier(module, issues, inner.next().unwrap());

        debug_assert!(inner.next().is_none());

        let ident = ident?;
        let ty = ty?;

        return Ok(WithSpan::new(
            StructMember {
                attributes,
                ident,
                ty,
            },
            struct_member_span,
        ));
    }

    /// The body of a struct, between the two `{` `}`.
    /// Drops invalid members, reporting issues and returning the set of valid members.
    fn parse_struct_body(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        struct_body: Pair<'a, parser::Rule>,
    ) -> WithSpan<Range<Handle<StructMember<'a>>>> {
        debug_assert_eq!(struct_body.as_rule(), parser::Rule::STRUCT_BODY_DECL);
        let struct_body_span = struct_body.as_span().into();

        let mut members = vec![];
        for member in struct_body.into_inner() {
            if let Ok(member) = Self::parse_struct_member(module, issues, member) {
                members.push(member);
            }
        }

        let members = module.members.append_all(members);
        return WithSpan::new(members, struct_body_span);
    }

    // Struct declarations use the `struct` keyword.
    fn parse_struct_decl(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        struct_decl: Pair<'a, parser::Rule>,
    ) {
        debug_assert_eq!(struct_decl.as_rule(), parser::Rule::STRUCT_DECL);
        let struct_span = struct_decl.as_span().into();

        let mut inner = struct_decl.into_inner();

        debug_assert_eq!(
            inner.next().unwrap().as_rule(),
            parser::Rule::STRUCT_KEYWORD
        );

        // Struct name
        let ident = Self::parse_ident(module, issues, inner.next().unwrap());

        // Struct fields
        let members = Self::parse_struct_body(module, issues, inner.next().unwrap());

        debug_assert!(inner.next().is_none());

        let ident = match ident {
            Ok(ident) => ident,
            Err(()) => return,
        };

        let struct_decl = StructDeclaration { ident, members };

        module.declarations.append(WithSpan::new(
            GlobalDeclaration::Struct(struct_decl),
            struct_span,
        ));
    }

    // Function declarations use the `fn` keyword.
    fn parse_function_decl(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        function_decl: Pair<'a, parser::Rule>,
    ) {
        debug_assert_eq!(function_decl.as_rule(), parser::Rule::FUNCTION_DECL);

        todo!()
    }

    // Const assert declarations use the `const_assert` keyword.
    fn parse_const_assert_decl(
        module: &mut ParsedModule<'a>,
        issues: &mut Vec<WithSpan<ParseIssue<'a>>>,
        const_assert_decl: Pair<'a, parser::Rule>,
    ) {
        assert_eq!(
            const_assert_decl.as_rule(),
            parser::Rule::CONST_ASSERT_STATEMENT
        );
        let const_assert_span = const_assert_decl.as_span().into();

        let mut inner = const_assert_decl.into_inner();

        debug_assert_eq!(
            inner.next().unwrap().as_rule(),
            parser::Rule::CONST_ASSERT_KEYWORD
        );

        let expr = inner.next().unwrap();
        let expr = Self::parse_expression(module, issues, expr);
        let expr = match expr {
            Ok(expr) => expr,
            Err(()) => return,
        };
        let expr = module.expressions.append(expr);

        debug_assert!(inner.next().is_none());

        module.declarations.append(WithSpan::new(
            GlobalDeclaration::ConstAssert(ConstAssertStatement { expr }),
            const_assert_span,
        ));
    }

    /// Remove all of the span information from this module. Useful when testing semantic equivalence
    /// of modules:
    ///
    /// ```rust
    /// # use ewgsl::parsing::ParsedModule;
    ///
    /// let mod1 = ParsedModule::parse("shader1.ewgsl", "const VALUE: i32 = 12;").unwrap();
    /// let mod2 = ParsedModule::parse("shader2.ewgsl", "\tconst     VALUE:   i32 = \n 12    ;").unwrap();
    ///
    /// assert!(mod1 != mod2);
    /// assert!(mod1.erase_spans() == mod2.erase_spans());
    /// ```
    pub fn erase_spans(self) -> ParsedModule<'a, spans::SpansErased> {
        Spanned::erase_spans(self)
    }
}

impl<'a> Spanned for ParsedModule<'a> {
    type Spanless = ParsedModule<'a, spans::SpansErased>;

    fn erase_spans(self) -> Self::Spanless {
        ParsedModule {
            directives: self.directives.erase_spans(),
            attributes: self.attributes.erase_spans().map(|attr| attr.erase_spans()),
            expressions: self
                .expressions
                .erase_spans()
                .map(|expr| expr.erase_spans()),
            members: self
                .members
                .erase_spans()
                .map(|member| member.erase_spans()),
            declarations: self.declarations.erase_spans().map(|dec| dec.erase_spans()),
        }
    }
}

impl<'a, S: spans::SpanState> Default for ParsedModule<'a, S> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<'a, S: spans::SpanState> PartialEq for ParsedModule<'a, S>
where
    directives::Directives<S>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        if self.directives != other.directives {
            return false;
        }
        // Search through declarations, checking each declaration is in both modules
        if self.declarations.len() != other.declarations.len() {
            return false;
        }
        'outer: for (_, lhs) in self.declarations.iter() {
            for (_, rhs) in other.declarations.iter() {
                if lhs.eq_in(
                    &(&self.attributes, &self.expressions, &self.members),
                    rhs,
                    &(&other.attributes, &other.expressions, &other.members),
                ) {
                    continue 'outer;
                }
            }
            return false;
        }

        return true;
    }
}
impl<'a, S: spans::SpanState> Eq for ParsedModule<'a, S> where Self: PartialEq {}

const _: () = {
    // Check ParsedModule is always Debug
    fn assert_debug<T: Debug>() {}
    fn assert_all() {
        assert_debug::<ParsedModule<'static, spans::SpansPresent>>();
        assert_debug::<ParsedModule<'static, spans::SpansErased>>();
    }
};

#[cfg(test)]
mod tests {
    use crate::arena::arena;
    use crate::spans::Span;

    use super::{
        expression::{BinaryOperator, CallPhrase},
        variables::{OptionallyTypedIdent, TypeSpecifier, VariableDeclaration},
        *,
    };

    #[test]
    fn parse_empty_gives_empty_module() {
        let parsed = ParsedModule::parse("empty_module.ewgsl", "").unwrap();
        assert_eq!(parsed, ParsedModule::empty());
    }

    #[rstest::rstest]
    // Invalid directives
    #[case("diagnostic;")]
    #[case("diagnostic error")]
    #[case("diagnosticerror;")]
    #[case("diagnosticinfo;")]
    #[case("info;")]
    #[case("diagnostic; info;")]
    #[case("enable;")]
    #[case("enable ")]
    #[case("enable not_a_real_feature;")]
    #[case("enable f32, not_a_real_feature;")]
    #[case("enable not_a_real_feature, f32;")]
    // Invalid global variables
    #[case("var;")]
    #[case("var __: u32;")]
    #[case("var fn: u32;")]
    #[case("var foo: u32")]
    #[case("varfoo: u32;")]
    #[case("var: u32;")]
    #[case("var NULL: u32;")]
    #[case("var bar: u32 = 1 +;")]
    #[case("@group var v: u32;")]
    #[case("@group(1, 2) var v: u32;")]
    #[case("@const(3) var v: u32;")]
    #[case("@var var v: u32;")]
    #[case("var<,> bar: u32 = 1 + 2;")]
    #[case("var<a,,> bar: u32;")]
    // Invalid constants
    #[case("const bar: u32;")]
    #[case("const bar: u32 =;")]
    #[case("const bar: vec2<bool = vec2(false, true);")]
    #[case("const bar: vec2<bool> vec2(false, true);")]
    #[case("const bar = 12")]
    // Invalid aliases
    #[case("alias foo: u32;")]
    #[case("alias bar = u32 =;")]
    #[case("alias bar = vec2<bool;")]
    #[case("alias bar;")]
    #[case("alias bar = u32")]
    // Invalid structs
    #[case("struct")]
    #[case("struct;")]
    #[case("struct {}")]
    #[case("struct { v1: u32 }")]
    #[case("struct Foo")]
    #[case("struct Foo;")]
    #[case("struct Foo { u32 }")]
    #[case("struct Foo ( u32 )")]
    #[case("struct Foo { v1: 8 }")]
    #[case("struct Foo { ,, }")]
    // Invalid const asserts
    #[case("const_assert")]
    #[case("const_assert;")]
    #[case("const_assert {}")]
    #[case("const_assert true")]
    fn parse_invalid_fails(#[case] invalid: &str) {
        assert!(
            ParsedModule::parse("invalid_module_test.ewgsl", invalid).is_err(),
            "`{}` was not a valid module, but parsed correctly",
            invalid
        )
    }

    fn assert_parse_valid_succeeds<'a>(
        valid: &'a str,
        expected: ParsedModule<'a, spans::SpansErased>,
    ) {
        let res = ParsedModule::parse("valid_module_test.ewgsl", valid);

        match res {
            Err(e) => panic!(
                "`{}` was a valid module, but parsed as invalid: {}",
                valid,
                e.diagnostics()
            ),
            Ok(res) => {
                let no_spans = res.erase_spans();
                if no_spans != expected {
                    panic!("{no_spans:#?} != {expected:#?}\nShader `{valid}` parsed incorrectly")
                }
            }
        }
    }

    fn assert_parse_as_same<'a>(src1: &'a str, src2: &'a str) {
        let res1 = ParsedModule::parse("valid_module_test_1.ewgsl", src1);
        let res1 = match res1 {
            Err(e) => panic!(
                "`{}` was a valid module, but parsed as invalid: {}",
                src1,
                e.diagnostics()
            ),
            Ok(res) => res,
        };

        let res2 = ParsedModule::parse("valid_module_test_2.ewgsl", src2).unwrap();

        assert_eq!(
            res1.erase_spans(),
            res2.erase_spans(),
            "`{}` and `{}` parsed differently",
            src1,
            src2
        )
    }

    fn assert_parse_as_different<'a>(src1: &'a str, src2: &'a str) {
        let res1 = ParsedModule::parse("valid_module_test_1.ewgsl", src1);
        let res1 = match res1 {
            Err(e) => panic!(
                "`{}` was a valid module, but parsed as invalid: {}",
                src1,
                e.diagnostics()
            ),
            Ok(res) => res,
        };

        let res2 = ParsedModule::parse("valid_module_test_2.ewgsl", src2);
        let res2 = match res2 {
            Err(e) => panic!(
                "`{}` was a valid module, but parsed as invalid: {}",
                src2,
                e.diagnostics()
            ),
            Ok(res) => res,
        };

        assert_ne!(
            res1.erase_spans(),
            res2.erase_spans(),
            "`{}` and `{}` parsed as the same, but should be different",
            src1,
            src2
        )
    }

    #[test]
    fn parse_valid_succeeds_case_directives_01() {
        assert_parse_valid_succeeds(
            "diagnostic error;",
            ParsedModule::<spans::SpansErased> {
                directives: directives::Directives {
                    diagnostics: arena![directives::SeverityControlName::Error],
                    ..directives::Directives::default()
                },
                ..ParsedModule::default()
            },
        )
    }
    #[test]
    fn parse_valid_succeeds_case_directives_02() {
        assert_parse_valid_succeeds(
            "\t\tdiagnostic\n info\r\n;",
            ParsedModule::<spans::SpansErased> {
                directives: directives::Directives {
                    diagnostics: arena![directives::SeverityControlName::Info],
                    ..directives::Directives::default()
                },
                ..ParsedModule::default()
            },
        )
    }
    #[test]
    fn parse_valid_succeeds_case_directives_03() {
        assert_parse_valid_succeeds(
            "diagnostic error;\ndiagnostic info;",
            ParsedModule::<spans::SpansErased> {
                directives: directives::Directives {
                    diagnostics: arena![
                        directives::SeverityControlName::Error,
                        directives::SeverityControlName::Info,
                    ],
                    ..directives::Directives::default()
                },
                ..ParsedModule::default()
            },
        )
    }
    #[test]
    fn parse_valid_succeeds_case_directives_04() {
        assert_parse_valid_succeeds(
            "diagnostic warning;\r\nenable f16;",
            ParsedModule::<spans::SpansErased> {
                directives: directives::Directives {
                    diagnostics: arena![directives::SeverityControlName::Warning],
                    enable_extensions: arena![directives::EnableExtensionName::F16],
                    ..directives::Directives::default()
                },
                ..ParsedModule::default()
            },
        )
    }
    #[test]
    fn parse_valid_succeeds_case_directives_05() {
        assert_parse_valid_succeeds(
            r"
            // Enable 16 bit (half) floats
            enable f16;
            /*
             * Also enable diagnostics
             */
            diagnostic warning;
            /*
             * In the future do some more things
             * /*
             *  * requires foo;
             *  */
             */
            // Really make sure F16s are enabled
            enable f16;
            ",
            ParsedModule::<spans::SpansErased> {
                directives: directives::Directives {
                    diagnostics: arena![directives::SeverityControlName::Warning],
                    enable_extensions: arena![
                        directives::EnableExtensionName::F16,
                        directives::EnableExtensionName::F16,
                    ],
                    ..directives::Directives::default()
                },
                ..ParsedModule::default()
            },
        )
    }

    #[test]
    fn parse_valid_succeeds_case_global_var_01() {
        let src = "@group(0) @binding(0) var<storage> foo: array<vec3<f32>>;";

        let mut expected_module = ParsedModule::<spans::SpansErased>::empty();
        let f32_arg = Expression::Identifier {
            ident: TemplatedIdent {
                ident: Ident::try_parse("f32").unwrap().into(),
                args: expected_module.expressions.append_all(vec![]),
            },
        }
        .into();
        let vec_arg = Expression::Identifier {
            ident: TemplatedIdent {
                ident: Ident::try_parse("vec3").unwrap().into(),
                args: expected_module.expressions.append_all(vec![f32_arg]),
            },
        }
        .into();
        let storage_arg = Expression::Identifier {
            ident: TemplatedIdent {
                ident: Ident::try_parse("storage").unwrap().into(),
                args: expected_module.expressions.append_all(vec![]),
            },
        }
        .into();
        let zero = expected_module.expressions.append(
            Expression::Literal {
                value: expression::Literal::Int("0"),
            }
            .into(),
        );
        let attrs = expected_module.attributes.append_all(vec![
            Attribute {
                identifier_span: Span::empty(),
                inner: attributes::AttributeInner::Group(zero),
            }
            .into(),
            Attribute {
                identifier_span: Span::empty(),
                inner: attributes::AttributeInner::Binding(zero),
            }
            .into(),
        ]);
        expected_module.declarations.append(
            GlobalDeclaration::Variable(GlobalVariableDeclaration {
                attributes: attrs,
                decl: VariableDeclaration {
                    template_list: expected_module.expressions.append_all(vec![storage_arg]),
                    ident: OptionallyTypedIdent {
                        ident: Ident::try_parse("foo").unwrap().into(),
                        ty: Some(TypeSpecifier(TemplatedIdent {
                            ident: Ident::try_parse("array").unwrap().into(),
                            args: expected_module.expressions.append_all(vec![vec_arg]),
                        })),
                    },
                }
                .into(),
                init: None,
            })
            .into(),
        );

        assert_parse_valid_succeeds(src, expected_module)
    }
    #[test]
    fn parse_valid_succeeds_case_global_var_02() {
        let src = "@must_use var<private> bar: array<u32, 4> = array<u32, 4>(1,1,2,3);";

        let mut expected_module = ParsedModule::<spans::SpansErased>::empty();
        let u32_arg = Expression::Identifier {
            ident: TemplatedIdent {
                ident: Ident::try_parse("u32").unwrap().into(),
                args: expected_module.expressions.append_all(vec![]),
            },
        };
        let four = Expression::Literal {
            value: expression::Literal::Int("4"),
        };

        let private_arg = Expression::Identifier {
            ident: TemplatedIdent {
                ident: Ident::try_parse("private").unwrap().into(),
                args: expected_module.expressions.append_all(vec![]),
            },
        }
        .into();

        let must_use_attr = expected_module.attributes.append_all(vec![Attribute {
            identifier_span: Span::empty(),
            inner: attributes::AttributeInner::MustUse,
        }
        .into()]);

        let call_args = expected_module.expressions.append_all(vec![
            Expression::Literal {
                value: expression::Literal::Int("1"),
            }
            .into(),
            Expression::Literal {
                value: expression::Literal::Int("1"),
            }
            .into(),
            Expression::Literal {
                value: expression::Literal::Int("2"),
            }
            .into(),
            Expression::Literal {
                value: expression::Literal::Int("3"),
            }
            .into(),
        ]);
        let template_args = expected_module
            .expressions
            .append_all(vec![u32_arg.clone().into(), four.clone().into()]);
        let rhs = expected_module.expressions.append(
            Expression::Call(CallPhrase {
                ident: TemplatedIdent {
                    ident: Ident::try_parse("array").unwrap().into(),
                    args: template_args,
                },
                args: call_args,
            })
            .into(),
        );

        expected_module.declarations.append(
            GlobalDeclaration::Variable(GlobalVariableDeclaration {
                attributes: must_use_attr,
                decl: VariableDeclaration {
                    template_list: expected_module.expressions.append_all(vec![private_arg]),
                    ident: OptionallyTypedIdent {
                        ident: Ident::try_parse("bar").unwrap().into(),
                        ty: Some(TypeSpecifier(TemplatedIdent {
                            ident: Ident::try_parse("array").unwrap().into(),
                            args: expected_module
                                .expressions
                                .append_all(vec![u32_arg.into(), four.into()]),
                        })),
                    },
                }
                .into(),
                init: Some(rhs),
            })
            .into(),
        );

        assert_parse_valid_succeeds(src, expected_module)
    }
    #[test]
    fn parse_valid_same_case_global_var_01() {
        assert_parse_as_same("var<> foo: u32;", "\nvar   foo : u32 ; ")
    }
    #[test]
    fn parse_valid_same_case_global_var_02() {
        assert_parse_as_same(
            r"
                @group(0) @binding(0) var<storage> foo: array<f32>;
                var<private> bar: vec3<u32>;
            ",
            r"
                @binding(0) @group(0) var<storage> foo: 
                    array<f32>;
                var<private> bar: 
                    vec3<u32>;
            ",
        )
    }
    #[test]
    fn parse_valid_same_case_global_var_03() {
        assert_parse_as_same(
            r"
                var<private> foo: vec3<u32> = vec3<u32>(1, 2, 3);
                var<private> bar: bool = 13 < 4;
            ",
            r"
                var<private> bar: bool = 13 < 4;
                var<private> foo: vec3<u32> = vec3<u32>(1, 2, 3);
            ",
        )
    }

    #[test]
    fn parse_valid_succeeds_case_global_const_01() {
        let src = "const v: vec2<u32> = vec2(0, 0);";

        let mut expected_module = ParsedModule::<spans::SpansErased>::empty();
        let u32_arg = Expression::Identifier {
            ident: TemplatedIdent {
                ident: Ident::try_parse("u32").unwrap().into(),
                args: expected_module.expressions.append_all(vec![]),
            },
        }
        .into();

        let call_args = expected_module.expressions.append_all(vec![
            Expression::Literal {
                value: expression::Literal::Int("0"),
            }
            .into(),
            Expression::Literal {
                value: expression::Literal::Int("0"),
            }
            .into(),
        ]);
        let template_args = expected_module.expressions.append_all(vec![]);
        let rhs = expected_module.expressions.append(
            Expression::Call(CallPhrase {
                ident: TemplatedIdent {
                    ident: Ident::try_parse("vec2").unwrap().into(),
                    args: template_args,
                },
                args: call_args,
            })
            .into(),
        );

        expected_module.declarations.append(
            GlobalDeclaration::Constant(GlobalConstantDeclaration {
                decl: OptionallyTypedIdent {
                    ident: Ident::try_parse("v").unwrap().into(),
                    ty: Some(TypeSpecifier(TemplatedIdent {
                        ident: Ident::try_parse("vec2").unwrap().into(),
                        args: expected_module.expressions.append_all(vec![u32_arg]),
                    })),
                }
                .into(),
                init: rhs,
            })
            .into(),
        );

        assert_parse_valid_succeeds(src, expected_module)
    }
    #[test]
    fn parse_valid_same_case_global_const_01() {
        assert_parse_as_same("const foo = 3;", "\nconst //my foo var\nfoo = 3 ; ")
    }
    #[test]
    fn parse_valid_different_case_global_const_01() {
        assert_parse_as_different("const foo = 3;", "const foo = 4;")
    }
    #[test]
    fn parse_valid_different_case_global_const_02() {
        assert_parse_as_different("const foo = 3;", "const bar = 3;")
    }
    #[test]
    fn parse_valid_different_case_global_const_03() {
        assert_parse_as_different("const foo = 3;", "const foo = 3.0;")
    }

    #[test]
    fn parse_valid_succeeds_case_global_override_01() {
        let src = "@id(1) override is_something = false;";

        let mut expected_module = ParsedModule::<spans::SpansErased>::empty();
        let one = expected_module.expressions.append(
            Expression::Literal {
                value: expression::Literal::Int("1"),
            }
            .into(),
        );
        let attr = expected_module.attributes.append_all(vec![Attribute {
            identifier_span: Span::empty(),
            inner: attributes::AttributeInner::Id(one),
        }
        .into()]);

        let rhs = expected_module.expressions.append(
            Expression::Literal {
                value: expression::Literal::Boolean(false),
            }
            .into(),
        );

        expected_module.declarations.append(
            GlobalDeclaration::Override(GlobalOverrideDeclaration {
                attributes: attr,
                decl: OptionallyTypedIdent {
                    ident: Ident::try_parse("is_something").unwrap().into(),
                    ty: None,
                }
                .into(),
                init: Some(rhs),
            })
            .into(),
        );

        assert_parse_valid_succeeds(src, expected_module)
    }
    #[test]
    fn parse_valid_same_case_global_override_01() {
        assert_parse_as_same("override foo = 3;", "\noverride //my foo var\nfoo = 3 ; ")
    }
    #[test]
    fn parse_valid_different_case_global_override_01() {
        assert_parse_as_different("override foo = 3;", "override foo = 4;")
    }
    #[test]
    fn parse_valid_different_case_global_override_02() {
        assert_parse_as_different("@id(1) override foo = 3;", "@id(0) override foo = 3;")
    }
    #[test]
    fn parse_valid_different_case_global_override_03() {
        assert_parse_as_different(
            "@id(1) override foo = 3;",
            "@id(1) override foo = 3; const foo = 3.0;",
        );
    }
    #[test]
    fn parse_valid_different_case_global_override_04() {
        assert_parse_as_different(
            "@id(1) override foo = 3; const foo = 3.0;",
            "@id(1) override foo = 3;",
        );
    }

    #[test]
    fn parse_valid_succeeds_case_global_alias_01() {
        let src = "alias MyAlias = vec3<u32>;";

        let mut expected_module = ParsedModule::<spans::SpansErased>::empty();
        let u32_arg = Expression::Identifier {
            ident: TemplatedIdent {
                ident: Ident::try_parse("u32").unwrap().into(),
                args: expected_module.expressions.append_all(vec![]),
            },
        }
        .into();

        expected_module.declarations.append(
            GlobalDeclaration::Alias(TypeAliasDeclaration {
                ident: Ident::try_parse("MyAlias").unwrap().into(),
                ty: TypeSpecifier(TemplatedIdent {
                    ident: Ident::try_parse("vec3").unwrap().into(),
                    args: expected_module.expressions.append_all(vec![u32_arg]),
                }),
            })
            .into(),
        );

        assert_parse_valid_succeeds(src, expected_module)
    }
    #[test]
    fn parse_valid_same_case_global_alias_01() {
        assert_parse_as_same("alias foo = bar;", "\t\t\talias \n\n\n\n\nfoo = bar;;;;;")
    }
    #[test]
    fn parse_valid_different_case_global_alias_01() {
        assert_parse_as_different("alias foo = bar;", "alias bar = foo;");
    }
    #[test]
    fn parse_valid_different_case_global_alias_02() {
        assert_parse_as_different("alias foo = vec3<u32>;", "alias foo = vec3<f32>;");
    }

    #[test]
    fn parse_valid_succeeds_case_struct_01() {
        let src = r"
            struct Foo {
                @size(4)
                v1: u32,
                @size(16)
                v2: vec3<f32>,
            }
        ";

        let mut expected_module = ParsedModule::<spans::SpansErased>::empty();

        let four = expected_module.expressions.append(
            Expression::Literal {
                value: expression::Literal::Int("4"),
            }
            .into(),
        );
        let sixteen = expected_module.expressions.append(
            Expression::Literal {
                value: expression::Literal::Int("16"),
            }
            .into(),
        );

        let attrs1 = expected_module.attributes.append_all(vec![Attribute {
            identifier_span: Span::empty(),
            inner: attributes::AttributeInner::Size(four),
        }
        .into()]);
        let attrs2 = expected_module.attributes.append_all(vec![Attribute {
            identifier_span: Span::empty(),
            inner: attributes::AttributeInner::Size(sixteen),
        }
        .into()]);

        let f32_arg = Expression::Identifier {
            ident: TemplatedIdent {
                ident: Ident::try_parse("f32").unwrap().into(),
                args: expected_module.expressions.append_all(vec![]),
            },
        }
        .into();

        let members = expected_module
            .members
            .append_all(vec![
                StructMember {
                    attributes: attrs1,
                    ident: Ident::try_parse("v1").unwrap().into(),
                    ty: TypeSpecifier(TemplatedIdent {
                        ident: Ident::try_parse("u32").unwrap().into(),
                        args: expected_module.expressions.append_all(vec![]),
                    }),
                }
                .into(),
                StructMember {
                    attributes: attrs2,
                    ident: Ident::try_parse("v2").unwrap().into(),
                    ty: TypeSpecifier(TemplatedIdent {
                        ident: Ident::try_parse("vec3").unwrap().into(),
                        args: expected_module.expressions.append_all(vec![f32_arg]),
                    }),
                }
                .into(),
            ])
            .into();

        expected_module.declarations.append(
            GlobalDeclaration::Struct(StructDeclaration {
                ident: Ident::try_parse("Foo").unwrap().into(),
                members,
            })
            .into(),
        );

        assert_parse_valid_succeeds(src, expected_module)
    }
    #[test]
    fn parse_valid_same_case_struct_01() {
        assert_parse_as_same(
            "struct Foo {v1: u32, v2: f32}",
            "\t\tstruct \nFoo {\n\tv1: u32,\r\n\t v2: f32};;",
        )
    }
    #[test]
    fn parse_valid_different_case_struct_01() {
        assert_parse_as_different(
            "struct Foo {v1: u32, v2: f32}",
            "struct Foo {v1: f32, v2: u32}",
        );
    }

    #[test]
    fn parse_valid_succeeds_case_global_const_assert_01() {
        let src = "const_assert A < B;";

        let mut expected_module = ParsedModule::<spans::SpansErased>::empty();

        let empty_args = expected_module.expressions.append_all(vec![]);
        let a = expected_module.expressions.append(
            Expression::Identifier {
                ident: TemplatedIdent {
                    ident: Ident::try_parse("A").unwrap().into(),
                    args: empty_args.clone(),
                },
            }
            .into(),
        );
        let b = expected_module.expressions.append(
            Expression::Identifier {
                ident: TemplatedIdent {
                    ident: Ident::try_parse("B").unwrap().into(),
                    args: empty_args,
                },
            }
            .into(),
        );
        let rhs = expected_module.expressions.append(
            Expression::Binary {
                lhs: a,
                op: BinaryOperator::LessThan.into(),
                rhs: b,
            }
            .into(),
        );

        expected_module
            .declarations
            .append(GlobalDeclaration::ConstAssert(ConstAssertStatement { expr: rhs }).into());

        assert_parse_valid_succeeds(src, expected_module)
    }
    #[test]
    fn parse_valid_same_case_global_const_assert_01() {
        assert_parse_as_same(
            "const_assert (A + B) != C[3];",
            "          const_assert     (       A    +       B      )     !=        C     [ 3  ]        ;;;;",
        )
    }
    #[test]
    fn parse_valid_different_case_global_const_assert_01() {
        assert_parse_as_different(
            "const_assert (A + B) != C[3];",
            "const_assert A + B != C[3];",
        );
    }
}
