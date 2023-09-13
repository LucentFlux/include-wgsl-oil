pub mod attributes;
pub mod directives;
pub mod expression;
pub mod ident;

use crate::{
    arena::{self, Arena, Handle, HandleRange},
    spans::{self, Spanned, WithSpan},
};

use std::{
    fmt::{Debug, Display},
    num::NonZeroUsize,
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
    expression::Expression,
    ident::{Ident, TemplatedIdent},
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
            parser::Rule::IDENT_PATTERN_TOKEN => "identifier pattern token",
            parser::Rule::IDENT => "identifier",
            parser::Rule::TEMPLATE_ARG_EXPRESSION => "template arg expression",
            parser::Rule::TEMPLATE_LIST => "template list",
            parser::Rule::ATTRIBUTE_INNER => "attribute inner",
            parser::Rule::ATTRIBUTE => "attribute",
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
            parser::Rule::MEMBER_IDENT => "member identifier",
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
            parser::Rule::_LESS_THAN => "`<`",
            parser::Rule::_GREATER_THAN => "`>`",
            parser::Rule::_LESS_THAN_EQUAL => "`<=`",
            parser::Rule::_GREATER_THAN_EQUAL => "`>=`",
            parser::Rule::RELATIONAL_EXPRESSION => "relational expression",
            parser::Rule::SHORT_CIRCUIT_AND_EXPRESSION => "short circuit and expression",
            parser::Rule::SHORT_CIRCUIT_OR_EXPRESSION => "short circuit or expression",
            parser::Rule::BITWISE_EXPRESSION => "bitwise expression",
            parser::Rule::EXPRESSION => "expression",
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
    pub issues: NonEmptyVec<spans::WithSpan<ParseIssue<'a>>>,
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
            let label = format!("{}", issue.inner);

            // Notes go at the bottom of the message
            let mut notes = vec![];
            if let Some(alternative) = issue.inner.recommend_alternative() {
                notes.push(format!("help: did you mean `{}`?", alternative));
            }

            let mut diagnostic = Diagnostic::error()
                .with_message("failed to parse EWGSL source")
                .with_labels(vec![Label::primary(
                    file_id,
                    issue.span.start()..issue.span.end(),
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
    fn emit(&self, stream: &StandardStream) -> Result<(), std::io::Error> {
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

    fn emit_to_stderr(&self) -> Result<(), std::io::Error> {
        let stream = StandardStream::stderr(ColorChoice::Always);
        self.emit(&stream)
    }

    fn emit_to_stdout(&self) -> Result<(), std::io::Error> {
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

/// A single module (source file), many of which can be unified into a shader.
#[perfect_derive(Debug)]
pub struct ParsedModule<'a, S: spans::Spanning = spans::WithSpans> {
    pub directives: directives::Directives<S>,
    pub attributes: Arena<Attribute<'a>, S>,
    pub expressions: Arena<Expression<'a, S>, S>,
}

impl<'a, S: spans::Spanning> ParsedModule<'a, S> {
    /// A module with no content. Equal to `ParsedModule::parse("")`.
    pub fn empty() -> Self {
        Self {
            directives: directives::Directives::empty(),
            attributes: Arena::new(),
            expressions: Arena::new(),
        }
    }
}

impl<'a> ParsedModule<'a, spans::WithSpans> {
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
                    issues: NonEmptyVec::of_one(spans::WithSpan { inner: error, span }),
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
        translation_unit: Pair<'_, parser::Rule>,
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
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
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
                        issues.push(spans::WithSpan {
                            span: name.as_span().into(),
                            inner: ParseIssue::UnknownSeverity {
                                found: name.as_str().to_owned(),
                            },
                        });
                        return;
                    }
                };

                module.directives.diagnostics.push(spans::WithSpan {
                    span,
                    inner: severity,
                });
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
                            issues.push(spans::WithSpan {
                                span: name.as_span().into(),
                                inner: ParseIssue::UnknownEnableExtension {
                                    found: name.as_str().to_owned(),
                                },
                            });
                            continue;
                        }
                    };

                    module.directives.enable_extensions.push(spans::WithSpan {
                        span: name.as_span().into(),
                        inner: extension,
                    });
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
                            issues.push(spans::WithSpan {
                                span: name.as_span().into(),
                                inner: ParseIssue::UnknownSoftwareExtension {
                                    found: name.as_str().to_owned(),
                                },
                            });
                            continue;
                        }
                    };

                    module.directives.software_extensions.push(spans::WithSpan {
                        span: name.as_span().into(),
                        inner: extension,
                    });
                }
            }
            _ => unreachable!(),
        }
    }

    // Declarations are constants, functions and types, and whatever else isn't a directive.
    fn parse_global_decl(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        global_decl: Pair<'_, parser::Rule>,
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
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        ident: Pair<'a, parser::Rule>,
    ) -> Option<Ident<'a>> {
        debug_assert_eq!(ident.as_rule(), parser::Rule::IDENT);
        let span = ident.as_span().into();

        let res = Ident::try_parse(ident.as_str(), span);
        if let Err(e) = res {
            issues.push(WithSpan {
                inner: ParseIssue::InvalidIdentifier {
                    inner: e,
                    found: ident.as_str(),
                },
                span,
            });
            return None;
        }

        return res.ok();
    }

    // An identifier. Just some characters without any templating or anything else.
    fn parse_template_list(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        template_list: Pair<'a, parser::Rule>,
    ) -> Option<Vec<Handle<Expression<'a>>>> {
        debug_assert_eq!(template_list.as_rule(), parser::Rule::TEMPLATE_LIST);
    }

    // An identifier possibly followed by some <template, args>
    fn parse_templated_ident(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        ident: Pair<'a, parser::Rule>,
    ) -> Option<TemplatedIdent<'a>> {
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

        return Some(TemplatedIdent { ident, args });
    }

    // Something of the form `foo<a, b>(x, y)`
    fn parse_call_prase_expression(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        call_phrase: Pair<'a, parser::Rule>,
    ) -> Option<expression::CallPhrase<'a>> {
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
        let args = args.into_iter().collect::<Option<Vec<_>>>()?;

        return Some(expression::CallPhrase { ident, args });
    }

    fn parse_primary_expression(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Option<Handle<Expression<'a>>> {
        debug_assert_eq!(expression.as_rule(), parser::Rule::PRIMARY_EXPRESSION);
        let expression_span = expression.as_span().into();

        let inner = expression.into_inner().next().unwrap();
        let expression = match inner.as_rule() {
            parser::Rule::TEMPLATE_ELABORATED_IDENT => {
                let ident = Self::parse_templated_ident(module, issues, inner)?;

                Expression::Identifier { ident }
            }
            parser::Rule::CALL_EXPRESSION => {
                let call_phrase = Self::parse_call_prase_expression(module, issues, inner)?;

                Expression::Call(call_phrase)
            }
            parser::Rule::LITERAL => {
                let literal = inner.into_inner().next().unwrap();
                let literal = match literal.as_rule() {
                    parser::Rule::INT_LITERAL => expression::Literal::Int(literal.as_str()),
                    parser::Rule::FLOAT_LITERAL => expression::Literal::Float(literal.as_str()),
                    parser::Rule::BOOLEAN_LITERAL => {
                        expression::Literal::Boolean(literal.as_str() == "true")
                    }
                    _ => unreachable!(),
                };

                Expression::Literal { value: literal }
            }
            parser::Rule::PAREN_EXPRESSION => {
                let inner_expr = inner.into_inner().next().unwrap();
                return Self::parse_expression(module, issues, inner_expr);
            }
            _ => unreachable!(),
        };

        let handle = module.expressions.append(WithSpan {
            span: expression_span,
            inner: expression,
        });
        return Some(handle);
    }

    // Accessors are swizzles, members or index expressions, and can be stacked on top of an expression.
    fn parse_accessor_on_expression(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        accessor: Pair<'a, parser::Rule>,
        base_expression: Handle<Expression>,
        composed_span: spans::Span,
    ) -> Option<Handle<Expression<'a>>> {
        assert_eq!(
            accessor.as_rule(),
            parser::Rule::COMPONENT_OR_SWIZZLE_SPECIFIER
        );
        let accessor_span = accessor.as_span().into();

        let inner = accessor.into_inner().next().unwrap();
        let accessed = match inner.as_rule() {
            parser::Rule::INDEX_EXPRESSION => {
                let index_expression = inner.into_inner().next().unwrap();
                let index_expression = Self::parse_expression(module, issues, index_expression)?;
                Expression::Index {
                    base: base_expression,
                    index: WithSpan {
                        span: accessor_span,
                        inner: index_expression,
                    },
                }
            }
            parser::Rule::SWIZZLE_NAME => {
                let swizzle = expression::Swizzle::try_parse(inner.as_str())
                    .expect("swizzle pattern only matches valid swizzles");
                Expression::Swizzle {
                    base: base_expression,
                    swizzle: WithSpan {
                        span: accessor_span,
                        inner: swizzle,
                    },
                }
            }
            parser::Rule::MEMBER_IDENT => {
                let member_ident = inner.as_str();
                Expression::MemberAccess {
                    base: base_expression,
                    member: WithSpan {
                        span: accessor_span,
                        inner: member_ident,
                    },
                }
            }
            _ => unreachable!(),
        };

        let handle = module.expressions.append(WithSpan {
            span: composed_span,
            inner: accessed,
        });

        return Some(handle);
    }

    fn parse_singular_expression(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Option<Handle<Expression<'a>>> {
        debug_assert_eq!(expression.as_rule(), parser::Rule::SINGULAR_EXPRESSION);

        let mut inner = expression.into_inner();

        // A primary expression followed by 1 or more accessors
        let inner_expr = inner.next().unwrap();
        let mut inner_span: spans::Span = inner_expr.as_span().into();
        let mut inner_expr = Self::parse_primary_expression(module, issues, expression)?;

        // If we find accessors, hand off to `parse_accessor_on_expression`
        while let Some(accessor) = inner.next() {
            inner_span = inner_span.union(accessor.as_span().into());
            inner_expr = Self::parse_accessor_on_expression(
                module, issues, accessor, inner_expr, inner_span,
            )?;
        }

        return Some(inner_expr);
    }

    fn parse_unary_expression(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Option<Handle<Expression<'a>>> {
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
        let op = WithSpan {
            span: op_span,
            inner: op,
        };

        // Then the expression - recurse
        let expression = inner.next().unwrap();
        assert!(inner.next().is_none());
        let expr = Self::parse_unary_expression(module, issues, expression)?;

        // Add to module and return
        let expr = Expression::Unary { op, expr };
        let expr_handle = module.expressions.append(WithSpan {
            span: expression_span,
            inner: expr,
        });
        return Some(expr_handle);
    }

    fn parse_bitwise_expression(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Option<Handle<Expression<'a>>> {
        debug_assert_eq!(expression.as_rule(), parser::Rule::BITWISE_EXPRESSION);
        let expression_span = expression.as_span().into();

        let mut inner = expression.into_inner();

        let lhs = inner.next().unwrap();
        debug_assert_eq!(lhs.as_rule(), parser::Rule::UNARY_EXPRESSION);

        let op = inner.next().unwrap();
        let op_span = op.as_span().into();
        let op = match op.as_rule() {
            parser::Rule::_AMPERSAND => expression::BinaryOperator::And,
            parser::Rule::_PIPE => expression::BinaryOperator::Or,
            parser::Rule::_UPTICK => expression::BinaryOperator::Xor,
            _ => unreachable!(),
        };
        let op = WithSpan {
            span: op_span,
            inner: op,
        };

        let mut rhs_list = vec![];
        while let Some(rhs) = inner.next() {
            debug_assert_eq!(rhs.as_rule(), parser::Rule::UNARY_EXPRESSION);
            rhs_list.push(rhs);

            // Skip next op, if there is one
            inner.next();
        }

        let mut lhs = Self::parse_unary_expression(module, issues, lhs)?;
        for rhs in rhs_list {
            let rhs = Self::parse_unary_expression(module, issues, rhs)?;

            let lhs_expr = Expression::Binary { lhs, op, rhs };
            lhs = module.expressions.append(WithSpan {
                span: expression_span,
                inner: lhs_expr,
            });
        }

        return Some(lhs);
    }

    fn parse_short_circuit_expression(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Option<Handle<Expression<'a>>> {
    }

    fn parse_relational_expression(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Option<Handle<Expression<'a>>> {
    }

    fn parse_expression(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        expression: Pair<'a, parser::Rule>,
    ) -> Option<Handle<Expression<'a>>> {
        debug_assert_eq!(expression.as_rule(), parser::Rule::EXPRESSION);

        let inner_rule = expression
            .into_inner()
            .next()
            .expect("every expression has a single sub-rule to switch on");
        match inner_rule.as_rule() {
            parser::Rule::BITWISE_EXPRESSION => {
                Self::parse_bitwise_expression(module, issues, inner_rule)
            }
            parser::Rule::SHORT_CIRCUIT_EXPRESSION => {
                Self::parse_short_circuit_expression(module, issues, inner_rule)
            }
            parser::Rule::RELATIONAL_EXPRESSION => {
                Self::parse_relational_expression(module, issues, inner_rule)
            }
            _ => unreachable!(),
        }
    }

    fn parse_attributes(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        attributes: impl IntoIterator<Item = Pair<'a, parser::Rule>>,
    ) -> HandleRange<Attribute<'a>> {
        let mut range = None;
        for attribute in attributes {
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
                Some(identifier) => WithSpan {
                    span: keyword_span,
                    inner: identifier,
                },
                None => {
                    issues.push(spans::WithSpan {
                        span: keyword_span,
                        inner: ParseIssue::UnknownAttributeIdentifier {
                            found: keyword.as_str().to_owned(),
                        },
                    });
                    continue;
                }
            };

            // Extract any arguments
            let mut expressions = Vec::new();
            while let Some(expression) = attribute_inner.next() {
                let expression = Self::parse_expression(module, issues, expression);
                expressions.push(expression);
            }
            let expressions = expressions.into_iter().collect::<Option<Vec<_>>>();
            let expressions = match expressions {
                Some(expressions) => expressions,
                None => continue,
            };

            // build and validate at the same time
            let attribute =
                match attributes::Attribute::try_parse_from(&*module, identifier, expressions) {
                    Ok(attribute) => attribute,
                    Err(mut attribute_issues) => {
                        issues.append(&mut attribute_issues);
                        continue;
                    }
                };

            // Add to the arena
            let attribute_handle = module.attributes.append(spans::WithSpan {
                span: attribute_span,
                inner: attribute,
            });

            // Update the range
            let mut new_range = range.unwrap_or(attribute_handle..attribute_handle);
            new_range.end = attribute_handle;
            range = Some(new_range);
        }

        range.unwrap_or(arena::empty_handle_range())
    }

    // Global variable declarations are global objects that use the `var` keyword, like bindings or workgroup memory.
    fn parse_global_variable_decl(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        global_variable_decl: Pair<'_, parser::Rule>,
    ) {
        assert_eq!(
            global_variable_decl.as_rule(),
            parser::Rule::GLOBAL_VARIABLE_DECL
        );

        todo!()
    }

    // Global value declarations are either `const` or `override` expressions.
    fn parse_global_value_decl(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        global_value_decl: Pair<'_, parser::Rule>,
    ) {
        debug_assert_eq!(global_value_decl.as_rule(), parser::Rule::GLOBAL_VALUE_DECL);

        todo!()
    }

    // Type alias declarations use the `alias` keyword.
    fn parse_type_alias_decl(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        type_alias_decl: Pair<'_, parser::Rule>,
    ) {
        debug_assert_eq!(type_alias_decl.as_rule(), parser::Rule::TYPE_ALIAS_DECL);

        todo!()
    }

    // Struct declarations use the `struct` keyword.
    fn parse_struct_decl(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        struct_decl: Pair<'_, parser::Rule>,
    ) {
        debug_assert_eq!(struct_decl.as_rule(), parser::Rule::STRUCT_DECL);

        todo!()
    }

    // Function declarations use the `fn` keyword.
    fn parse_function_decl(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        function_decl: Pair<'_, parser::Rule>,
    ) {
        debug_assert_eq!(function_decl.as_rule(), parser::Rule::FUNCTION_DECL);

        todo!()
    }

    // Const assert declarations use the `const_assert` keyword.
    fn parse_const_assert_decl(
        module: &mut ParsedModule,
        issues: &mut Vec<spans::WithSpan<ParseIssue>>,
        const_assert_decl: Pair<'_, parser::Rule>,
    ) {
        assert_eq!(
            const_assert_decl.as_rule(),
            parser::Rule::CONST_ASSERT_STATEMENT
        );

        todo!()
    }

    /// Remove all of the span information from this module. Useful when testing semantic equivalence
    /// of modules:
    ///
    /// ```rust
    /// # use ewgsl::parsing::ParsedModule;
    ///
    /// let mod1 = ParsedModule::parse("const VALUE: i32 = 12;")
    /// let mod2 = ParsedModule::parse("\tconst     VALUE:   i32 = \n 12    ;")
    ///
    /// assert!(mod1 != mod2);
    /// assert!(mod1.erase_spans() == mod2.erase_spans());
    /// ```
    pub fn erase_spans(self) -> ParsedModule<'a, spans::WithoutSpans> {
        ParsedModule {
            directives: self.directives.erase_spans(),
            attributes: self.attributes.erase_spans(),
            expressions: self
                .expressions
                .erase_spans()
                .map(|expr| expr.erase_spans()),
        }
    }
}

impl<'a, S: spans::Spanning> Default for ParsedModule<'a, S> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<'a, S: spans::Spanning> PartialEq for ParsedModule<'a, S>
where
    directives::Directives<S>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        return self.directives == other.directives;
    }
}
impl<'a, S: spans::Spanning> Eq for ParsedModule<'a, S> where Self: PartialEq {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty_gives_empty_module() {
        let parsed = ParsedModule::parse("empty_module.ewgsl", "").unwrap();
        assert_eq!(parsed, ParsedModule::empty());
    }

    #[rstest::rstest]
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
    fn parse_invalid_fails(#[case] invalid: &str) {
        assert!(
            ParsedModule::parse("invalid_module_test.ewgsl", invalid).is_err(),
            "`{}` was not a valid module, but parsed correctly",
            invalid
        )
    }

    fn parse_valid_succeeds(valid: &str, expected: ParsedModule<spans::WithoutSpans>) {
        let res = ParsedModule::parse("valid_module_test.ewgsl", valid);

        match res {
            Err(e) => panic!(
                "`{}` was a valid module, but parsed as invalid: {}",
                valid,
                e.diagnostics()
            ),
            Ok(res) => assert_eq!(
                res.erase_spans(),
                expected,
                "`{}` parsed incorrectly",
                valid
            ),
        }
    }

    #[test]
    fn parse_valid_succeeds_case_01() {
        parse_valid_succeeds(
            "diagnostic error;",
            ParsedModule::<spans::WithoutSpans> {
                directives: directives::Directives {
                    diagnostics: vec![directives::SeverityControlName::Error],
                    ..directives::Directives::default()
                },
                ..ParsedModule::default()
            },
        )
    }
    #[test]
    fn parse_valid_succeeds_case_02() {
        parse_valid_succeeds(
            "\t\tdiagnostic\n info\r\n;",
            ParsedModule::<spans::WithoutSpans> {
                directives: directives::Directives {
                    diagnostics: vec![directives::SeverityControlName::Info],
                    ..directives::Directives::default()
                },
                ..ParsedModule::default()
            },
        )
    }
    #[test]
    fn parse_valid_succeeds_case_03() {
        parse_valid_succeeds(
            "diagnostic error;\ndiagnostic info;",
            ParsedModule::<spans::WithoutSpans> {
                directives: directives::Directives {
                    diagnostics: vec![
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
    fn parse_valid_succeeds_case_04() {
        parse_valid_succeeds(
            "diagnostic warning;\r\nenable f16;",
            ParsedModule::<spans::WithoutSpans> {
                directives: directives::Directives {
                    diagnostics: vec![directives::SeverityControlName::Warning],
                    enable_extensions: vec![directives::EnableExtensionName::F16],
                    ..directives::Directives::default()
                },
                ..ParsedModule::default()
            },
        )
    }
    #[test]
    fn parse_valid_succeeds_case_05() {
        parse_valid_succeeds(
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
            ",
            ParsedModule::<spans::WithoutSpans> {
                directives: directives::Directives {
                    diagnostics: vec![directives::SeverityControlName::Warning],
                    enable_extensions: vec![directives::EnableExtensionName::F16],
                    ..directives::Directives::default()
                },
                ..ParsedModule::default()
            },
        )
    }
}
