pub enum Value {
    I8(ValueI8),
    I16(ValueI16),
    I32(ValueI32),
    I64(ValueI64),
    U8(ValueU8),
    U16(ValueU16),
    U32(ValueU32),
    U64(ValueU64),
    Void,
    Ptr(Box<Value>), // for heap allocated values.
    // TODO: support pointer, struct, string, function.
}

pub struct ValueI8 {
    pub val: i8,
}

pub struct ValueI16 {
    pub val: i16,
}

pub struct ValueI32 {
    pub val: i32,
}

pub struct ValueI64 {
    pub val: i64,
}

pub struct ValueU8 {
    pub val: u8,
}

pub struct ValueU16 {
    pub val: u16,
}

pub struct ValueU32 {
    pub val: u32,
}

pub struct ValueU64 {
    pub val: u64,
}
