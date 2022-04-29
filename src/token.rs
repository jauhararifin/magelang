use std::fmt;

use super::pos::Pos;

#[derive(Clone, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Option<String>,
    pub pos: Pos,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref value) = self.value {
            write!(f, "[{:?} {:?}({})]", &self.pos, &self.kind, value)
        } else {
            write!(f, "[{:?} {:?}]", &self.pos, &self.kind)
        }
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
    Ident,
    StringLit,
    IntegerLit,
    FloatLit,
    True,
    False,
    // symbols
    OpenBrace,
    CloseBrace,
    OpenBlock,
    CloseBlock,
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
    Dot,
    As,
    // operators
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
    // comments
    Comment,
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
