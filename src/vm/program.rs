use super::instruction::Instruction;

struct Program {
    functions: Vec<Function>,
}

struct Function {
    name: Option<String>,
    instructions: Vec<Instruction>,
}

