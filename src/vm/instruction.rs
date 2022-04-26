use super::value::Value;

pub enum Instruction {
    Add(NumType),
    Sub(NumType),
    Div(NumType),
    Mul(NumType),
    Mod(NumType),

    Eq(PrimitiveType),
    NEq(PrimitiveType),
    LT(NumType),
    LTEq(NumType),
    GT(NumType),
    GTEq(NumType),

    And(IntType),
    Or(IntType),
    Xor(IntType),
    Neg(IntType),
    Not,

    Jump(isize), // jump offset
    JumpIfTrue(isize),
    JumpIfFalse(isize),
    Push(Value), // TODO: use more native method.

    Call(usize), // call(func_id),
}

pub enum IntType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

pub enum NumType {
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

pub enum PrimitiveType {
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
    Bool,
}
