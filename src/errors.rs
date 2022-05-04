use std::{io, num::{ParseIntError, ParseFloatError}, rc::Rc};

use crate::{
    pos::Pos,
    token::{Token, TokenKind}, semantic::{Type, Expr},
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
    InvalidIntLit { token: Token, err: ParseIntError },
    InvalidFloatLit { token: Token, err: ParseFloatError },
    CannotCast { pos: Pos, expr: Expr, target_type: Rc<Type> },

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

