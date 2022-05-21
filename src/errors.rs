use std::{
    io,
    num::{ParseFloatError, ParseIntError},
    rc::Rc,
};

use crate::{
    pos::Pos,
    semantic::{Expr, Type},
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub enum Error {
    // lexer error
    Io(io::Error),
    UnexpectedChar {
        char: u32,
        pos: Pos,
    },
    UnexpectedSymbol {
        symbol: String,
        pos: Pos,
    },

    // parser error
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: Token,
    },

    // analyzer
    InvalidIntLit {
        token: Token,
        err: ParseIntError,
    },
    InvalidFloatLit {
        token: Token,
        err: ParseFloatError,
    },
    CannotCast {
        pos: Pos,
        expr: Expr, 
        target_type: Rc<Type>,
    },
    NotAnArray{expr: Expr},
    IndexIsNotAnInt{expr: Expr},
    ArraySizeIsNotAnInt{expr: Expr},

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
