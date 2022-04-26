use std::{collections::HashMap, rc::Rc};

use super::typ::Type;

pub enum Value {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Bool(bool),
    Void,
    Struct(StructValue),
    Obj(Rc<Value>), // for heap allocated values.
                    // TODO: support pointer, struct, string, function.
}

struct StructValue {
    typ: Rc<StructType>,
    values: Vec<Value>, // TODO: find a way to make this more native. Maybe use raw pointer.
}

pub struct StructType {
    pub name: Option<String>, // None means anonymous struct.
    pub fields: HashMap<String, FieldType>,
}

pub struct FieldType {
    pub index: usize,
    pub offset: usize,
    pub name: String,
    pub typ: Rc<Type>,
}
