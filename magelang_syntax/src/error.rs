use crate::token::{Pos, TokenKind};
use std::fmt::Display;

pub struct SyntaxError {
    pub pos: Pos,
    pub kind: SyntaxErrorKind,
}

pub enum SyntaxErrorKind {
    UnexpectedChar(char),
    UnexpectedToken(TokenKind),
    InvalidDigitInBase { digit: char, base: u8 },
    NonDecimalFraction,
    MissingClosingQuote,
    Unexpected { expected: String, found: String },
}

impl SyntaxErrorKind {
    pub fn pos(self, pos: Pos) -> SyntaxError {
        SyntaxError { pos, kind: self }
    }
}

impl Display for SyntaxErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedChar(ch) => write!(f, "Unexpected character '{ch}'"),
            Self::UnexpectedToken(token_kind) => write!(f, "Unexpected token {token_kind}"),
            Self::InvalidDigitInBase { digit, base } => {
                write!(f, "Invalid digit, cannot use {digit} for base {base} integer")
            }
            Self::NonDecimalFraction => write!(f, "Can only use decimal base for floating value"),
            Self::MissingClosingQuote => write!(f, "Missing closing quote for string literal"),
            Self::Unexpected { expected, found } => write!(f, "Expected {expected} but found {found}"),
        }
    }
}
