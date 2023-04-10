use crate::types::TypeId;
use magelang_common::SymbolId;

#[derive(Debug)]
pub struct Expr {
    pub type_id: TypeId,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Invalid,
    I64(i64),
    F64(f64),
    Local(usize),
    Func(FuncExpr),
    Binary { a: Box<Expr>, op: BinOp, b: Box<Expr> },
    Call(Box<Expr>, Vec<Expr>),
}

#[derive(PartialEq, Eq, Debug)]
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

#[derive(Debug)]
pub struct FuncExpr {
    pub package_name: SymbolId,
    pub function_name: SymbolId,
}
