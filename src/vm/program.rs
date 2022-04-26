use crate::ast::Type;

use super::instruction::Instruction;

pub struct Program {
    pub types: Vec<Type>,
    pub functions: Vec<Function>,
}

pub struct Function {
    pub name: Option<String>,
    pub instructions: Vec<Instruction>,
}

