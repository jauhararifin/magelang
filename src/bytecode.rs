use std::{collections::HashMap, rc::Rc};

pub struct Program {
    pub executable: bool,
    pub global_values: HashMap<String, Value>,
}

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
    Fn(FnValue),
    Heap(Rc<Value>),
}

pub struct StructValue {
    pub values: Vec<Value>,
}

pub struct FnValue {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

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

    SetLocal(usize), // pop stack, and set it into the n-th stack element.
    GetLocal(usize), // push the n-th local value to stack.

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

    Push(Value), // TODO: use more native method.
    Pop,

    Call(usize), // call(func_id),
}
