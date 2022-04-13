use std::io;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

pub trait Lexer {
    fn next(&mut self) -> Result<Token, Error>;
    fn peek(&mut self) -> Result<&Token, Error>;
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: Pos,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TokenKind {
    // keywords
    Fn,
    Var,
    If,
    While,
    Return,
    Type,
    Struct,
    Tuple,
    // constant
    Ident(String),
    StringLit(String),
    NumberLit(String),
    FloatLit(String),
    True,
    False,
    // symbols
    OpenBrace,
    CloseBrace,
    OpenBlock,
    CloseBlock,
    Assign,
    Endl,
    EOI,
    Comma,
    Colon,
    Dot,
    // operators
    Plus,
    PlusAssign,
    Minus,
    MinusAssign,
    Mul,
    MulAssign,
    Div,
    DivAssign,
    Mod,
    ModAssign,
    BitAnd,
    BitAndAssign,
    BitOr,
    BitOrAssign,
    BitNot,
    BitXor,
    BitXorAssign,
    SHL,
    SHLAssign,
    SHR,
    SHRAssign,
    And,
    Or,
    Not,
    GT,
    LT,
    GTEq,
    LTEq,
    Eq,
    NotEq,
    // comments
    Comment(String),
    // primitives
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Pos {
    pub line: i32,
    pub col: i32,
}
