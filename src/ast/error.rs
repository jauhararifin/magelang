use crate::token::{Error as LexerError, Token, TokenKind};

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
