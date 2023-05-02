use crate::Token;
use magelang_common::{Error, Span};
use std::fmt::Display;

pub(crate) fn unexpected_char(span: Span, ch: char) -> Error {
    Error::new(span, format!("Unexpected character '{}'", ch))
}

pub(crate) fn unexpected_token(token: &Token) -> Error {
    Error::new(token.span.clone(), format!("Unexpected token '{}'", token.kind))
}

pub(crate) fn unexpected_newline(span: Span) -> Error {
    Error::new(span, String::from("Unexpected newline"))
}

pub(crate) fn invalid_digit_in_base(span: Span, digit: char, base: u8) -> Error {
    Error::new(
        span,
        format!("Invalid digit, cannot use {} for base {} integer", digit, base),
    )
}

pub(crate) fn non_decimal_fraction(span: Span) -> Error {
    Error::new(span, String::from("Can only use decimal for fractional number"))
}

pub(crate) fn missing_closing_quote(span: Span) -> Error {
    Error::new(span, String::from("Missing closing quote in string literal"))
}

pub(crate) fn unexpected_parsing(span: Span, expected: impl Display, found: impl Display) -> Error {
    Error::new(span, format!("Expected {expected}, but found {found}"))
}
