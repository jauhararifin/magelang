use std::rc::Rc;

pub enum Type {
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
}

pub struct Struct {
    pub name: String,
    pub fields: Vec<Field>,
}

pub struct Field {
    pub index: usize,
    pub offset: usize,
    pub name: String,
    pub typ: Rc<Type>,
}
