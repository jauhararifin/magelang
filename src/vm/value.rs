use std::{mem::size_of, fmt::Debug};

#[derive(Clone)]
pub struct RuntimeValue {
    pub typ: ValueType,
    pub data: usize, // pointer.
}

impl Debug for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            match &self.typ {
                ValueType::I64 => write!(f, "{:?}@{} {}", self.typ, self.data, &*(self.data as *const i64)),
                _ => todo!(),
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueType {
    Void,
    Bool,
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
    FnId,
}

impl ValueType {
    pub fn size(&self) -> usize {
        match &self {
            ValueType::Void => 0,
            ValueType::Bool => 1,
            ValueType::I8 => 1,
            ValueType::I16 => 2,
            ValueType::I32 => 4,
            ValueType::I64 => 8,
            ValueType::U8 => 1,
            ValueType::U16 => 2,
            ValueType::U32 => 4,
            ValueType::U64 => 8,
            ValueType::F32 => 4,
            ValueType::F64 => 8,
            ValueType::FnId => size_of::<usize>(),
        }
    }
}
