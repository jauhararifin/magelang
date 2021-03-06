use std::{fmt, rc::Rc};

use super::pos::Pos;

#[derive(Clone, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Rc<String>,
    pub pos: Pos,
}

impl Token {
    pub fn str(&self) -> &str {
        self.value.as_str()
    }

    pub fn clone_value(&self) -> Rc<String> {
        self.value.clone()
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{:?} {:?} {}]", &self.pos, &self.kind, self.value)
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TokenKind {
    Package,
    Import,
    Fn,
    Native,
    Var,
    If,
    While,
    Return,
    Ident,
    IntegerLit,
    FloatLit,
    StringLit,
    True,
    False,
    OpenBrace,
    CloseBrace,
    OpenBlock,
    CloseBlock,
    OpenBrack,
    CloseBrack,
    Assign,
    PlusAssign,
    MinusAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShlAssign,
    ShrAssign,
    Endl,
    Eoi,
    Comma,
    Colon,
    As,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitNot,
    BitXor,
    Shl,
    Shr,
    And,
    Or,
    Not,
    GT,
    LT,
    GTEq,
    LTEq,
    Eq,
    NotEq,
    Comment,
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
