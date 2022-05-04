use std::io;

use crate::{
    ast,
    pos::Pos,
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub enum Error {
    // lexer error
    UnexpectedSymbol { symbol: char, pos: Pos },
    Io(io::Error),
    // parser error
    UnexpectedToken { expected: Vec<TokenKind>, found: Token },
    UnexpectedStructType { expr: ast::ExprNode },
    // analyzer
    EmptyPackage,
    MissingPackage,
    ImportCycle,
    RedeclaredSymbol,
    UndeclaredSymbol,
    UndeclaredField,
    UnresolvedType,
    MismatchType,
    CyclicType,
    NotAStruct,
    NotAFn,
    UnsupportedOperationInConstant,
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
