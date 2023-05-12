mod expr;
mod statement;
mod types;
mod value;
mod printer;
mod names;

pub use expr::{BinOp, Expr, ExprKind, GlobalId, StringLitExpr, UnOp};
pub use statement::{BlockStatement, IfStatement, ReturnStatement, Statement, WhileStatement};
pub use types::{ArrayPtrType, FuncType, PointerType, SliceType, Type, TypeId, TypeLoader};
pub use value::value_from_string_lit;
pub use printer::TypePrinter;
pub use names::*;

use magelang_common::SymbolId;
use std::rc::Rc;

#[derive(Debug)]
pub struct Package {
    pub name: SymbolId,
    pub globals: Vec<Global>,
    pub functions: Vec<Func>,
    pub native_functions: Vec<NativeFunction>,
    pub strings: Vec<Box<[u8]>>,
}

#[derive(Debug)]
pub struct Global {
    pub package_name: SymbolId,
    pub variable_name: SymbolId,
    pub type_id: TypeId,
    pub value: Expr,
}

#[derive(Debug)]
pub struct NativeFunction {
    pub package_name: SymbolId,
    pub function_name: SymbolId,
    pub func_type: FuncType,
    pub tags: Vec<Tag>,
}

#[derive(Debug)]
pub struct Tag {
    pub name: SymbolId,
    pub arguments: Vec<Rc<[u8]>>,
}

#[derive(Debug)]
pub struct Func {
    pub package_name: SymbolId,
    pub function_name: SymbolId,
    pub func_type: FuncType,
    pub locals: Vec<TypeId>,
    pub body: Statement,
}
