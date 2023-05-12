use crate::types::TypeId;
use magelang_common::SymbolId;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub type_id: TypeId,
    pub assignable: bool,
    pub comp_const: bool,
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
    Isize(i64),
    Usize(u64),
    Local(usize),
    ZeroOf(TypeId),
    SizeOf(TypeId),
    AlignOf(TypeId),
    DataEnd,
    Global(GlobalId),
    FuncInit(GlobalId, Rc<[TypeId]>),
    StringLit(StringLitExpr),
    Binary { a: Box<Expr>, op: BinOp, b: Box<Expr> },
    Unary { op: UnOp, val: Box<Expr> },
    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Cast(Box<Expr>, TypeId),
    Deref(Box<Expr>),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct GlobalId {
    pub package_name: SymbolId,
    pub item_name: SymbolId,
}

impl GlobalId {
    pub fn new(package_name: SymbolId, item_name: SymbolId) -> Self {
        Self {
            package_name,
            item_name,
        }
    }
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
pub struct StringLitExpr {
    pub package_name: SymbolId,
    pub index: usize,
}
