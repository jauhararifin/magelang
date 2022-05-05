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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    pub kind: ValueKind,
}

impl Value {
    pub fn constant(kind: ValueKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueKind {
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
    Struct(Vec<Value>),
    Fn(Vec<Instruction>),
    Ptr(usize), // pointer to a value, doesn't have to be in heap. the value itself can be function.
}

impl Eq for ValueKind {}

impl From<bool> for ValueKind {
    fn from(v: bool) -> Self {
        ValueKind::Bool(v)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Nop,
    Constant(Value), // push constant value to stack.

    // number operations
    Add(bool, u8), // (signed, size)
    Sub(bool, u8),
    Div(bool, u8),
    Mul(bool, u8),
    Mod(bool, u8),

    AddFloat(u8),
    SubFloat(u8),
    DivFloat(u8),
    MulFloat(u8),

    // shift operations
    Shl(u8),
    Shr(u8),

    // binary comparison operation
    Eq,
    NEq,

    LT(bool, u8),
    LTEq(bool, u8),
    GT(bool, u8),
    GTEq(bool, u8),

    LTFloat(u8),
    LTEqFloat(u8),
    GTFloat(u8),
    GTEqFloat(u8),

    // unary comparison operation
    Not(u8),

    // bitwise operation
    And(u8),
    Or(u8),
    Xor(u8),
    Neg(u8),

    // Alloc allocate a Value in the heap.
    // Then, it will push a Value::Obj to the stack.
    Alloc(Value),

    // TODO: support struct-like data-type.
    SetLocal(isize), // pop stack, and set it into the n-th stack element.
    GetLocal(isize), // push the n-th local value to stack.

    SetGlobal(usize),
    GetGlobal(usize), // if primitive, copy the value. otherwise, use ptr.

    // pop stack, and get n-th prop.
    GetProp(usize),
    // pop stack, get its n-th property, and set it to the top of the stack.
    // if you have statement like this: a.b.c.d.e = 10;
    // then -> Constant(10), GetLocal(a), GetProp(b), GetProp(c), GetProp(d),
    // SetProp(e).
    SetProp(usize),

    Jump(isize), // jump offset
    JumpIfTrue(isize),
    JumpIfFalse(isize),

    Pop(usize), // pop n items from the stack

    Call(usize), // pop 1 value and use it as function pointer. then pop n values to make it as the arguments.
    Ret,         // return

    // call native method
    CallNative(Rc<String>),
}
