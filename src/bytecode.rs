use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub values: Vec<Value>,
    pub entry_point: usize,
}

pub struct Object {
    pub symbol_table: HashMap<String, usize>, // contain the mangled name of the symbols.
    pub values: Vec<Value>,
}

#[derive(Debug)]
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
    Struct(Vec<Value>),
    Fn(Vec<Instruction>),
    Ptr(usize), // pointer to a value, doesn't have to be in heap. the value itself can be function.
}

#[derive(Debug)]
pub enum Instruction {
    Constant(Value), // push constant value to stack.

    // number operations
    Add,
    Sub,
    Div,
    Mul,
    Mod,

    // shift operations
    Shl,
    Shr,

    // binary comparison operation
    Eq,
    NEq,
    LT,
    LTEq,
    GT,
    GTEq,
    // unary comparison operation
    Not,

    // bitwise operation
    And,
    Or,
    Xor,
    Neg,

    // Alloc allocate a Value in the heap.
    // Then, it will push a Value::Obj to the stack.
    Alloc(Value),

    SetLocal(isize), // pop stack, and set it into the n-th stack element.
    GetLocal(isize), // push the n-th local value to stack.

    SetGlobal(usize),
    GetGlobal(usize),

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

    Call, // pop 1 value, and use it as function pointer.
    Ret,  // return

    // call native method
    CallNative(String),
}
