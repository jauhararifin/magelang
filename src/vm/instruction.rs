use crate::ast::Type;

use super::value::Value;

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

    Alloc(Type), // based on the type, push heap allocated value to stack.

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

