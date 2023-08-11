use crate::ir::{Func, FunctionType, Instr, Module, ValType};
use indexmap::IndexSet;
use magelang_semantic::{
    BitSize, Db, FloatSize, FloatType, FuncBody, FuncId, FuncType, GenFuncId, IntType, PackageId, StatementDb, Type,
    TypeArgsId, TypeDb, TypeId,
};

pub struct ProgramBuilder<'db> {
    db: &'db Db,
    main_package: PackageId,

    module: Module,
    funcs: IndexSet<FuncToBuild>,
}

#[derive(PartialEq, Eq, Hash)]
enum FuncToBuild {
    Normal(FuncId),
    GenInst(GenFuncId, TypeArgsId),
}

impl<'db> ProgramBuilder<'db> {
    pub fn new(db: &'db Db, main_package: PackageId) -> Self {
        Self {
            db,
            main_package,
            module: Module::default(),
            funcs: IndexSet::default(),
        }
    }

    pub fn build_module(&mut self) {
        let func_id = self.db.get_main_func(self.main_package);
        self.funcs.insert(FuncToBuild::Normal(func_id));

        let mut i = 0;
        while let Some(func) = self.funcs.get_index(i) {
            match func {
                FuncToBuild::Normal(func_id) => self.build_func(*func_id),
                FuncToBuild::GenInst(gen_func_id, typeargs_id) => self.build_gen_func_inst(*gen_func_id, *typeargs_id),
            }
            i += 1;
        }
    }

    pub fn take_module(self) -> Module {
        self.module
    }

    fn build_func(&mut self, func_id: FuncId) {
        let func_type_id = self.db.get_func_type_id(func_id);
        let func_type = self.db.get_func_type(func_type_id);
        let func_type = self.get_func_type(&func_type);
        let wasm_type_id = self.module.add_func_type(func_type);

        let body = self.db.get_func_body(func_id);
        let body = self.build_func_body(&body);

        self.module.add_func(Func {
            ty: wasm_type_id,
            locals: vec![],
            body,
        });
    }

    fn build_gen_func_inst(&mut self, gen_func_id: GenFuncId, typeargs_id: TypeArgsId) {
        let func_type_id = self.db.get_generic_func_inst_type_id(gen_func_id, typeargs_id);
        let func_type = self.db.get_func_type(func_type_id);
        let func_type = self.get_func_type(&func_type);
        let wasm_type_id = self.module.add_func_type(func_type);

        let body = self.db.get_generic_func_inst_body(gen_func_id, typeargs_id);
        let body = self.build_func_body(&body);

        self.module.add_func(Func {
            ty: wasm_type_id,
            locals: vec![],
            body,
        });
    }

    fn build_func_body(&self, stmt: &FuncBody) -> Vec<Instr> {
        match stmt {
            FuncBody::User(..) => vec![],
            FuncBody::Native(..) => vec![],
        }
    }

    fn get_func_type(&self, func_type: &FuncType) -> FunctionType {
        let returns = self.to_wasm_type(func_type.return_type);
        FunctionType {
            params: func_type
                .params
                .iter()
                .flat_map(|type_id| self.to_wasm_type(*type_id))
                .collect(),
            returns,
        }
    }

    fn to_wasm_type(&self, type_id: TypeId) -> Vec<ValType> {
        let ty = self.db.get_type(type_id);
        match ty.as_ref() {
            Type::Unknown => unreachable!("unknown type can't be compiled"),
            Type::Void => Vec::default(),
            Type::Int(int_type) => vec![Self::int_to_wasm_type(int_type)],
            Type::Float(float_type) => vec![Self::float_to_wasm_type(float_type)],
            Type::Bool => vec![ValType::I32],
            Type::Pointer(..) => vec![ValType::I32],
            Type::ArrayPtr(..) => vec![ValType::I32],
            Type::Func(..) => unreachable!("func type is not a value type"),
            Type::Struct(..) => {
                let fields = self.db.get_struct_field(type_id.into());
                fields
                    .fields
                    .values()
                    .flat_map(|type_id| self.to_wasm_type(*type_id))
                    .collect()
            }
            Type::GenericArg(..) => unreachable!("cannot compile an opaque func parameter"),
        }
    }

    fn int_to_wasm_type(ty: &IntType) -> ValType {
        match ty.size {
            BitSize::I64 => ValType::I64,
            BitSize::ISize => ValType::I32,
            _ => ValType::I32,
        }
    }

    fn float_to_wasm_type(ty: &FloatType) -> ValType {
        match ty.size {
            FloatSize::F32 => ValType::F32,
            FloatSize::F64 => ValType::F64,
        }
    }
}
