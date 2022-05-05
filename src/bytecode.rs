use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub entry_point: usize,
}

#[derive(Debug)]
pub struct Object {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

// note that this is not used for runtime.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Nop,

    Constant8(u8),
    Constant16(u16),
    Constant32(u32),
    Constant64(u64),

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
    Eq(BitSize),
    NEq(BitSize),
    LT(BitSize),
    LTEq(BitSize),
    GT(BitSize),
    GTEq(BitSize),
    SLT(BitSize),
    SLTEq(BitSize),
    SGT(BitSize),
    SGTEq(BitSize),
    EqFloat(BitSize),
    NEqFloat(BitSize),
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
    // Alloc(Value),

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
