use std::io;

use crate::{
    pos::Pos,
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub enum Error {
    // lexer error
    Io(io::Error),
    UnexpectedChar { char: char, pos: Pos },
    UnexpectedSymbol { symbol: String, pos: Pos },

    // parser error
    UnexpectedToken { expected: Vec<TokenKind>, found: Token },

    // analyzer
    UndeclaredSymbol,
    RedeclaredSymbol,
    MismatchType,
    NotAFn,
    FnCallArgNumMismatch,
    CannotAssignTo,
    MissingReturnValue,
    MissingMain,
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}
