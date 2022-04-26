use crate::ast::Type;

use super::value::Value;

pub enum Instruction {
    Constant(Value), // push constant value to stack.

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

    Alloc(Type), // based on the type, push heap allocated value to stack.

    SetLocal(usize), // pop stack, and set it into the n-th stack element.
    GetLocal(usize), // push the n-th local value to stack.

    GetProp(usize), // pop stack, and get n-th prop.
    SetProp(usize), // pop stack, get its n-th property, and set it to the top of the stack.
                    // if you have statement like this: a.b.c.d.e = 10;
                    // then -> Constant(10), GetLocal(a), GetProp(b), GetProp(c), GetProp(d),
                    // SetProp(e).

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
