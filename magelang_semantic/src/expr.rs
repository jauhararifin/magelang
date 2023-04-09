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
    Local(usize),
    Func(FuncExpr),
    Call(Box<Expr>, Vec<Expr>),
}

#[derive(Debug)]
pub struct FuncExpr {
    pub package_name: SymbolId,
    pub function_name: SymbolId,
}
