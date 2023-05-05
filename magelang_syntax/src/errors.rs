use crate::TokenKind;
use magelang_common::{Error, ErrorAccumulator, FileId, Pos};
use std::fmt::Display;

pub(crate) struct SyntaxErrorAccumulator<'err> {
    err_accumulator: &'err ErrorAccumulator,
    file_id: FileId,
}

impl<'err> SyntaxErrorAccumulator<'err> {
    pub fn new(err_accumulator: &'err ErrorAccumulator, file_id: FileId) -> Self {
        Self {
            err_accumulator,
            file_id,
        }
    }

    pub fn unexpected_char(&self, offset: u32, ch: char) {
        let pos = Pos::new(self.file_id, offset);
        self.err_accumulator
            .push(Error::new(pos, format!("Unexpected character '{ch}'")));
    }

    pub fn unexpected_token(&self, offset: u32, kind: TokenKind) {
        let pos = Pos::new(self.file_id, offset);
        self.err_accumulator
            .push(Error::new(pos, format!("Unexpected token '{kind}'")));
    }

    pub fn invalid_digit_in_base(&self, offset: u32, digit: char, base: u8) {
        let pos = Pos::new(self.file_id, offset);
        self.err_accumulator.push(Error::new(
            pos,
            format!("Invalid digit, cannot use {digit} for base {base} integer"),
        ));
    }

    pub fn non_decimal_fraction(&self, offset: u32) {
        let pos = Pos::new(self.file_id, offset);
        self.err_accumulator.push(Error::new(
            pos,
            String::from("Can only use decimal for fractional number"),
        ));
    }

    pub fn missing_closing_quote(&self, offset: u32) {
        let pos = Pos::new(self.file_id, offset);
        self.err_accumulator
            .push(Error::new(pos, String::from("Missing closing quote in string literal")));
    }

    pub fn unexpected_parsing(&self, offset: u32, expected: impl Display, found: impl Display) {
        let pos = Pos::new(self.file_id, offset);
        self.err_accumulator
            .push(Error::new(pos, format!("Expected {expected}, but found {found}")));
    }
}
