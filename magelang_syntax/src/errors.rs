use crate::Token;
use magelang_common::{Error, Pos};
use std::fmt::Display;

pub(crate) fn unexpected_char(pos: Pos, ch: char) -> Error {
    Error::new(pos, format!("Unexpected character '{}'", ch))
}

pub(crate) fn unexpected_token(token: &Token) -> Error {
    Error::new(token.pos.clone(), format!("Unexpected token '{}'", token.kind))
}

pub(crate) fn unexpected_newline(pos: Pos) -> Error {
    Error::new(pos, String::from("Unexpected newline"))
}

pub(crate) fn invalid_digit_in_base(pos: Pos, digit: char, base: u8) -> Error {
    Error::new(
        pos,
        format!("Invalid digit, cannot use {} for base {} integer", digit, base),
    )
}

pub(crate) fn non_decimal_fraction(pos: Pos) -> Error {
    Error::new(pos, String::from("Can only use decimal for fractional number"))
}

pub(crate) fn missing_closing_quote(pos: Pos) -> Error {
    Error::new(pos, String::from("Missing closing quote in string literal"))
}

pub(crate) fn unexpected_parsing(pos: Pos, expected: impl Display, found: impl Display) -> Error {
    Error::new(pos, format!("Expected {expected}, but found {found}"))
}
