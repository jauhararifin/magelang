use std::{fmt, rc::Rc};

use super::pos::Pos;

#[derive(Clone, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Option<Rc<String>>,
    pub pos: Pos,
}

impl Token {
    pub fn unwrap_value(&self) -> &String {
        self.value.as_ref().unwrap()
    }

    pub fn unwrap_str(&self) -> &str {
        self.unwrap_value().as_str()
    }

    pub fn clone_value(&self) -> Rc<String> {
        Rc::clone(self.value.as_ref().unwrap())
    }
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
    Native,
    Var,
    If,
    While,
    Return,
    // constant
    Ident,
    IntegerLit,
    FloatLit,
    StringLit, // not used yet.
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
