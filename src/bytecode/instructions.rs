use std::rc::Rc;

#[rustfmt::skip]
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Nop,

    ConstI64(u64),
    ConstF32(f32),
    ConstF64(f64),

    AddI64, AddF32, AddF64,
    SubI64, SubF32, SubF64,
    SMulI64, MulI64, MulF32, MulF64,
    SDivI64, DivI64, DivF32, DivF64,
    ModI64, SModI64,
    ShlI64, ShrI64, SShrI64,

    EqI64, EqF32, EqF64,
    NEqI64, NEqF32, NEqF64,
    GTI64, GTF32, GTF64,
    GTEqI64, GTEqF32, GTEqF64,
    LTI64, LTF32, LTF64,
    LTEqI64, LTEqF32, LTEqF64,

    NotI64,
    AndI64,
    OrI64,
    XorI64,

    ConvertF32ToI64,
    SConvertF32ToI64,
    ConvertF32ToF64,

    ConvertF64ToI64,
    SConvertF64ToI64,
    ConvertF64ToF32,

    ConvertI64ToF32,
    ConvertI64ToF64,
    SConvertI64ToF32,
    SConvertI64ToF64,

    GetLocal(isize),
    SetLocal(isize),

    // TODO: support multidimensional array
    AllocArrayI64, // [count] -> ptr
    AllocArrayF32,
    AllocArrayF64,

    ArrayGetI64, // [array][index] -> [I64]
    ArrayGetF32,
    ArrayGetF64,

    ArraySetI64, // [i64][array][index] -> []
    ArraySetF32,
    ArraySetF64,

    Jump(isize),
    JumpIfTrue(isize),
    JumpIfFalse(isize),

    Pop(usize),

    Call, // [fn_id] -> []
    Ret,

    CallNative(Rc<String>),
}

impl Eq for Instruction {}
