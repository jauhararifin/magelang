use std::fmt;

#[derive(Clone, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: Pos,
}

impl Token {
    fn ident(&self) -> Option<&String> {
        if let TokenKind::Ident(s) = &self.kind {
            Some(s)
        } else {
            None
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{:?} {:?}]", &self.pos, &self.kind)
    }
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
    IntegerLit(String),
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
    As,
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

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Pos {
    pub line: i32,
    pub col: i32,
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}
