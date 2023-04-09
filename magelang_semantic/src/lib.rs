mod types;
mod expr;
mod statement;

pub use types::{TypeId, Type, FuncType, IntType, TypeDisplay, TypeLoader};
pub use expr::{Expr, ExprKind, FuncExpr};
pub use statement::{Statement, BlockStatement, ReturnStatement};

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

