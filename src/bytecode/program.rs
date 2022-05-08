use super::Instruction;

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
