use crate::lexer::Error as LexerError;
use crate::token::{Token, TokenKind};

#[derive(Debug)]
pub enum Error {
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: Token,
    },
    Lexer(LexerError),
}

impl From<LexerError> for Error {
    fn from(err: LexerError) -> Self {
        Error::Lexer(err)
    }
}
