use std::{collections::HashMap, rc::Rc};

pub struct Type {
    kind: TypeKind,
    size: usize,
    allignment: usize,
}

pub enum TypeKind {
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
    Bool,
    Struct(Struct),
    Obj(Rc<Type>),
    Invalid,
    // TODO: support array.
}

pub struct Struct {
    pub name: Option<String>,
    pub fields: HashMap<String, Field>,
}

pub struct Field {
    pub index: usize,
    pub offset: usize,
    pub name: String,
    pub typ: Rc<Type>,
}
