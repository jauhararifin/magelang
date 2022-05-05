use std::{collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct Program {
    pub values: Vec<Value>,
    pub entry_point: usize,
}

#[derive(Debug)]
pub struct Object {
    pub symbol_table: HashMap<Rc<String>, usize>, // contain the mangled name of the symbols.
    pub values: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq)]
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
    Fn(Vec<Instruction>),
    Ptr(usize), // pointer to a value, doesn't have to be in heap. the value itself can be function.
                // TODO: support struct
                // Struct(Vec<Value>),
}

impl Eq for Value {}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Value::Bool(v)
    }
}

// note that this is not used for runtime.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Nop,
    Constant(Value), // push constant value to stack.

    // number operations
    Add(BitSize),
    Sub(BitSize),
    Div(BitSize),
    Mul(BitSize),
    Mod(BitSize),
    SDiv(BitSize),
    SMul(BitSize),
    SMod(BitSize),
    AddFloat(BitSize),
    SubFloat(BitSize),
    DivFloat(BitSize),
    MulFloat(BitSize),
    Shl(BitSize),
    Shr(BitSize),
    Eq,
    NEq,
    LT(BitSize),
    LTEq(BitSize),
    GT(BitSize),
    GTEq(BitSize),
    SLT(BitSize),
    SLTEq(BitSize),
    SGT(BitSize),
    SGTEq(BitSize),
    LTFloat(BitSize),
    LTEqFloat(BitSize),
    GTFloat(BitSize),
    GTEqFloat(BitSize),
    Not(BitSize),
    And(BitSize),
    Or(BitSize),
    Xor(BitSize),
    Neg(BitSize),

    // Alloc allocate a Value in the heap.
    // Then, it will push a Value::Obj to the stack.
    Alloc(Value),

    // TODO: support struct-like data-type.
    SetLocal(isize), // pop stack, and set it into the n-th stack element.
    GetLocal(isize), // push the n-th local value to stack.

    SetGlobal(usize),
    GetGlobal(usize),

    // pop stack, and get n-th prop.
    // GetProp(usize),
    // pop stack, get its n-th property, and set it to the top of the stack.
    // if you have statement like this: a.b.c.d.e = 10;
    // then -> Constant(10), GetLocal(a), GetProp(b), GetProp(c), GetProp(d),
    // SetProp(e).
    // SetProp(usize),
    Jump(isize), // jump offset
    JumpIfTrue(isize),
    JumpIfFalse(isize),

    Pop(usize), // pop n items from the stack

    Call, // pop 1 value and use it as function pointer.
    Ret,  // return

    // call native method
    CallNative(Rc<String>),
}

impl From<Value> for Instruction {
    fn from(v: Value) -> Self {
        Self::Constant(v)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BitSize {
    B8,
    B16,
    B32,
    B64,
}

impl From<u8> for BitSize {
    fn from(size: u8) -> Self {
        match size {
            8 => Self::B8,
            16 => Self::B16,
            32 => Self::B32,
            64 => Self::B64,
            _ => panic!("got invalid size: {}", size),
        }
    }
}
