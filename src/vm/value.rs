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
                ValueType::Void => write!(f, "{:?}@{}", self.typ, self.data),
                ValueType::Bool => write!(f, "{:?}@{} {}", self.typ, self.data, &*(self.data as *const bool)),
                ValueType::I64 => write!(f, "{:?}@{} {}", self.typ, self.data, &*(self.data as *const i64)),
                ValueType::FnId => write!(f, "{:?}@{} {}", self.typ, self.data, &*(self.data as *const usize)),
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

pub trait IntoValueType {
    fn into_value_type() -> ValueType;
}

impl IntoValueType for () {
    fn into_value_type() -> ValueType {
        ValueType::Void
    }
}

impl IntoValueType for i8 {
    fn into_value_type() -> ValueType {
        ValueType::I8
    }
}

impl IntoValueType for i16 {
    fn into_value_type() -> ValueType {
        ValueType::I16
    }
}

impl IntoValueType for i32 {
    fn into_value_type() -> ValueType {
        ValueType::I32
    }
}

impl IntoValueType for i64 {
    fn into_value_type() -> ValueType {
        ValueType::I64
    }
}

impl IntoValueType for u8 {
    fn into_value_type() -> ValueType {
        ValueType::U8
    }
}

impl IntoValueType for u16 {
    fn into_value_type() -> ValueType {
        ValueType::U16
    }
}

impl IntoValueType for u32 {
    fn into_value_type() -> ValueType {
        ValueType::U32
    }
}

impl IntoValueType for u64 {
    fn into_value_type() -> ValueType {
        ValueType::U64
    }
}

impl IntoValueType for f32 {
    fn into_value_type() -> ValueType {
        ValueType::F32
    }
}

impl IntoValueType for f64 {
    fn into_value_type() -> ValueType {
        ValueType::F64
    }
}

impl IntoValueType for bool {
    fn into_value_type() -> ValueType {
        ValueType::Bool
    }
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
