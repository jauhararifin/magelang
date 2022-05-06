use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub entry_point: usize,
}

#[derive(Debug)]
pub struct Object {
    pub functions: Vec<Function>, // list of functions.
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Void,
    Bool(bool),
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
    FnId(usize),
    Ptr(usize),
}

impl Eq for Value {}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

impl From<i8> for Value {
    fn from(v: i8) -> Self {
        Self::I8(v)
    }
}

impl From<i16> for Value {
    fn from(v: i16) -> Self {
        Self::I16(v)
    }
}

impl From<i32> for Value {
    fn from(v: i32) -> Self {
        Self::I32(v)
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Self::I64(v)
    }
}

impl From<u8> for Value {
    fn from(v: u8) -> Self {
        Self::U8(v)
    }
}

impl From<u16> for Value {
    fn from(v: u16) -> Self {
        Self::U16(v)
    }
}

impl From<u32> for Value {
    fn from(v: u32) -> Self {
        Self::U32(v)
    }
}

impl From<u64> for Value {
    fn from(v: u64) -> Self {
        Self::U64(v)
    }
}

impl From<f32> for Value {
    fn from(v: f32) -> Self {
        Self::F32(v)
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self::F64(v)
    }
}

// note that this is not used for runtime.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Nop,

    Constant(Value),

    // number operations
    // TODO: remove the variant. Split these into Add8, Add16, FAdd32, etc..
    Add(Variant),
    Sub(Variant),
    Div(Variant),
    Mul(Variant),
    Mod(Variant),
    Shl(Variant),
    Shr(Variant),
    Eq(Variant),
    NEq(Variant),
    LT(Variant),
    LTEq(Variant),
    GT(Variant),
    GTEq(Variant),
    Not(Variant),
    And(Variant),
    Or(Variant),
    Xor(Variant),

    // Alloc allocate a Value in the heap.
    // Then, it will push a Value::Obj to the stack.
    // pop a usize from stack, and allocate heap with that size, then push stack with the pointer.
    Alloc(Variant), // [size] -> [ptr to heap]
    // TODO: I think GetHeap need a variant
    GetHeap, // [heap_ptr] -> [copied_value_from_heap]
    SetHeap, // [value] [heap_ptr]-> {}

    // TODO: support struct-like data-type.
    SetLocal(isize), // pop stack, and set it into the n-th stack element.
    GetLocal(isize), // push the n-th local value to stack.

    // TODO: support global values.
    // SetGlobal(usize),
    // GetGlobal(usize),

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Variant {
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
}

impl Variant {
    pub fn int(signed: bool, size: u8) -> Self {
        match (signed, size) {
            (true, 8) => Self::I8,
            (true, 16) => Self::I16,
            (true, 32) => Self::I32,
            (true, 64) => Self::I64,
            (false, 8) => Self::U8,
            (false, 16) => Self::U16,
            (false, 32) => Self::U32,
            (false, 64) => Self::U64,
            _ => panic!("got invalid size: {}", size),
        }
    }

    pub fn float(size: u8) -> Self {
        match size {
            32 => Self::F32,
            64 => Self::F64,
            _ => panic!("got invalid size: {}", size),
        }
    }
}
