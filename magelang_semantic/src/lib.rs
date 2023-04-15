mod expr;
mod statement;
mod types;

pub use expr::{BinOp, Expr, ExprKind, FuncExpr, UnOp};
pub use statement::{BlockStatement, ReturnStatement, Statement, WhileStatement};
pub use types::{FuncType, Type, TypeDisplay, TypeId, TypeLoader};

use magelang_common::SymbolId;

#[derive(Debug)]
pub struct Package {
    pub name: SymbolId,
    pub functions: Vec<Func>,
    pub native_functions: Vec<NativeFunction>,
}

#[derive(Debug)]
pub struct NativeFunction {
    pub package_name: SymbolId,
    pub function_name: SymbolId,
    pub func_type: FuncType,
}

#[derive(Debug)]
pub struct Func {
    pub package_name: SymbolId,
    pub function_name: SymbolId,
    pub func_type: FuncType,
    pub body: Statement,
}
