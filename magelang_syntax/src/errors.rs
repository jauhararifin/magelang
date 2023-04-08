use magelang_common::{Error, Span};
use std::fmt::Display;

pub(crate) fn unexpected_char(span: Span, ch: char) -> Error {
    Error::new(span, format!("Unexpected character '{}'", ch))
}

pub(crate) fn unexpected_newline(span: Span) -> Error {
    Error::new(span, String::from("Unexpected newline"))
}

pub(crate) fn missing_closing_quote(span: Span) -> Error {
    Error::new(span, String::from("Missing closing quote in string literal"))
}

pub(crate) fn unexpected_parsing(span: Span, expected: impl Display, found: impl Display) -> Error {
    Error::new(span, format!("Expected {expected}, but found {found}"))
}
