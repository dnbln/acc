use crate::{diagnostics::IntoDiagnostic, parser::span::Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("unexpected token: expected {expected}, found {found}")]
    UnexpectedToken { expected: String, found: String },
    #[error("unexpected end of file: expected {expected}")]
    UnexpectedEof { expected: String },
    #[error("invalid token")]
    InvalidToken,
    #[error("'{0}' cannot be used as a prefix operator")]
    UnknownPrefixOperator(String),
    #[error("expected expression, found {found}")]
    ExpectedExpression { found: String },
    #[error("expected type, found {found}")]
    ExpectedType { found: String },
    #[error("expected identifier, found {found}")]
    ExpectedIdentifier { found: String },
    #[error("unclosed {delimiter}")]
    UnclosedDelimiter {
        delimiter: &'static str,
        opened_at: Span,
    },
}

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

pub type ParseResult<T> = Result<T, ParseError>;

impl ParseError {
    pub fn new(kind: ParseErrorKind, span: Span) -> Self {
        ParseError { kind, span }
    }
}

impl IntoDiagnostic for ParseError {
    fn to_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        match &self.kind {
            ParseErrorKind::UnexpectedToken { expected, found } => Diagnostic::error()
                .with_message(format!("unexpected token"))
                .with_labels(vec![
                    Label::primary(file_id, self.span.range())
                        .with_message(format!("expected {}, found {}", expected, found)),
                ]),

            ParseErrorKind::UnexpectedEof { expected } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_labels(vec![
                    Label::primary(file_id, self.span.range())
                        .with_message(format!("expected {} here", expected)),
                ]),

            ParseErrorKind::InvalidToken => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![
                    Label::primary(file_id, self.span.range())
                        .with_message("unrecognized character sequence"),
                ]),

            ParseErrorKind::UnknownPrefixOperator(op) => Diagnostic::error()
                .with_message(format!("'{}' cannot be used as a prefix operator", op))
                .with_labels(vec![
                    Label::primary(file_id, self.span.range())
                        .with_message("not a valid prefix operator"),
                ]),

            ParseErrorKind::ExpectedExpression { found } => Diagnostic::error()
                .with_message("expected expression")
                .with_labels(vec![
                    Label::primary(file_id, self.span.range())
                        .with_message(format!("expected expression, found {}", found)),
                ]),

            ParseErrorKind::ExpectedType { found } => Diagnostic::error()
                .with_message("expected type")
                .with_labels(vec![
                    Label::primary(file_id, self.span.range())
                        .with_message(format!("expected type specifier, found {}", found)),
                ]),

            ParseErrorKind::ExpectedIdentifier { found } => Diagnostic::error()
                .with_message("expected identifier")
                .with_labels(vec![
                    Label::primary(file_id, self.span.range())
                        .with_message(format!("expected identifier, found {}", found)),
                ]),

            ParseErrorKind::UnclosedDelimiter {
                delimiter,
                opened_at,
            } => Diagnostic::error()
                .with_message(format!("unclosed {}", delimiter))
                .with_labels(vec![
                    Label::primary(file_id, self.span.range())
                        .with_message(format!("expected closing {}", delimiter)),
                    Label::secondary(file_id, opened_at.range())
                        .with_message("opening delimiter here"),
                ]),
        }
    }
}
