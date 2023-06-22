use crate::ty::TypeId;
use crate::def::GlobalId;
use crate::expr::Expr;

#[derive(Debug)]
pub struct Assignable {
    pub type_id: TypeId,
    pub kind: AssignableKind,
}

#[derive(Debug)]
pub enum AssignableKind {
    Invalid,
    Deref(Box<Assignable>),
    ArrayPtrIndex(Expr, Expr),
    Local(usize),
    Global(GlobalId),
    Element(Box<Assignable>, usize),
}

