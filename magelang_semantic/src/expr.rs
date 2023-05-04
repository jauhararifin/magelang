use crate::types::TypeId;
use magelang_common::SymbolId;

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub type_id: TypeId,
    pub assignable: bool,
    pub kind: ExprKind,
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Invalid,
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    U64(u64),
    U32(u32),
    U16(u16),
    U8(u8),
    F64(f64),
    F32(f32),
    Bool(bool),
    Usize(u64),
    Local(usize),
    Func(FuncExpr),
    StringLit(StringLitExpr),
    Binary { a: Box<Expr>, op: BinOp, b: Box<Expr> },
    Unary { op: UnOp, val: Box<Expr> },
    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Cast(Box<Expr>, TypeId),
    Pointer(usize, TypeId),
    Deref(Box<Expr>),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitOr,
    BitAnd,
    BitXor,
    ShiftLeft,
    ShiftRight,
    And,
    Or,
    Eq,
    NEq,
    Gt,
    GEq,
    Lt,
    LEq,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum UnOp {
    BitNot,
    Sub,
    Add,
    Not,
}

#[derive(Debug, PartialEq)]
pub struct FuncExpr {
    pub package_name: SymbolId,
    pub function_name: SymbolId,
}

#[derive(Debug, PartialEq)]
pub struct StringLitExpr {
    pub package_name: SymbolId,
    pub index: usize,
}
