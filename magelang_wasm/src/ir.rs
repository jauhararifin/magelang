#[derive(Debug, Default)]
pub struct Module {
    pub types: Vec<FunctionType>,
    pub funcs: Vec<Func>,
}

#[derive(Debug)]
pub struct TypeIdx(usize);

impl Module {
    pub fn add_func_type(&mut self, ty: FunctionType) -> TypeIdx {
        self.types.push(ty);
        TypeIdx(self.types.len() - 1)
    }

    pub fn add_func(&mut self, func: Func) {
        self.funcs.push(func);
    }
}

#[derive(Debug)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug)]
pub struct FunctionType {
    pub params: Vec<ValType>,
    pub returns: Vec<ValType>,
}

#[derive(Debug)]
pub struct Func {
    pub ty: TypeIdx,
    pub locals: Vec<ValType>,
    pub body: Vec<Instr>,
}

#[derive(Debug)]
pub enum Instr {
    I32Const(u32),
    I64Const(u64),
    F32Const(f32),
    F64Const(f64),
}
