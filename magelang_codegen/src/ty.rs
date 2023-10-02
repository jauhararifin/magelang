use magelang_typecheck::{BitSize, FloatType, InternType, Type};
use wasm_helper as wasm;

pub(crate) fn build_val_type(ty: InternType<'_>) -> Vec<wasm::ValType> {
    match ty.as_ref() {
        Type::Unknown | Type::TypeArg(..) => unreachable!("found invalid type"),
        Type::Struct(struct_type) => {
            let mut fields = vec![];
            for field_ty in struct_type
                .body
                .get()
                .expect("missing struct body")
                .fields
                .values()
            {
                fields.extend(build_val_type(*field_ty));
            }
            fields
        }
        Type::Inst(inst_type) => {
            let mut fields = vec![];
            for field_ty in inst_type
                .body
                .get()
                .expect("missing struct body")
                .fields
                .values()
            {
                fields.extend(build_val_type(*field_ty));
            }
            fields
        }

        Type::Func(..) => vec![wasm::ValType::Num(wasm::NumType::I32)],

        Type::Void => vec![],
        Type::Opaque => vec![wasm::ValType::Ref(wasm::RefType::ExternRef)],
        Type::Bool => vec![wasm::ValType::Num(wasm::NumType::I32)],

        Type::Int(_, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
            vec![wasm::ValType::Num(wasm::NumType::I32)]
        }
        Type::Int(_, BitSize::I64) => vec![wasm::ValType::Num(wasm::NumType::I64)],

        Type::Float(FloatType::F32) => vec![wasm::ValType::Num(wasm::NumType::F32)],
        Type::Float(FloatType::F64) => vec![wasm::ValType::Num(wasm::NumType::F64)],

        Type::Ptr(..) => vec![wasm::ValType::Num(wasm::NumType::I32)],
        Type::ArrayPtr(..) => vec![wasm::ValType::Num(wasm::NumType::I32)],
    }
}

pub(crate) fn build_zero_wasm_type<'ctx>(ty: &wasm::ValType) -> Vec<wasm::Instr> {
    match ty {
        wasm::ValType::Num(wasm::NumType::I32) => vec![wasm::Instr::I32Const(0)],
        wasm::ValType::Num(wasm::NumType::I64) => vec![wasm::Instr::I64Const(0)],
        wasm::ValType::Num(wasm::NumType::F32) => vec![wasm::Instr::F32Const(0f32)],
        wasm::ValType::Num(wasm::NumType::F64) => vec![wasm::Instr::F64Const(0f64)],
        wasm::ValType::Ref(wasm::RefType::FuncRef) => {
            vec![wasm::Instr::RefNull(wasm::RefType::FuncRef)]
        }
        wasm::ValType::Ref(wasm::RefType::ExternRef) => {
            vec![wasm::Instr::RefNull(wasm::RefType::ExternRef)]
        }
    }
}

pub(crate) fn build_zero_type(ty: InternType<'_>) -> Vec<wasm::Instr> {
    match ty.as_ref() {
        Type::Unknown | Type::TypeArg(..) => unreachable!("found invalid type"),
        Type::Inst(inst_type) => inst_type
            .body
            .get()
            .expect("missing struct body")
            .fields
            .values()
            .flat_map(|ty| build_zero_type(*ty))
            .collect(),
        Type::Struct(struct_type) => struct_type
            .body
            .get()
            .expect("missing struct body")
            .fields
            .values()
            .flat_map(|ty| build_zero_type(*ty))
            .collect(),
        Type::Func(..) => vec![wasm::Instr::I32Const(0)],
        Type::Void => vec![],
        Type::Opaque => vec![wasm::Instr::RefNull(wasm::RefType::ExternRef)],
        Type::Bool => vec![wasm::Instr::I32Const(0)],
        Type::Int(_, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
            vec![wasm::Instr::I32Const(0)]
        }
        Type::Int(_, BitSize::I64) => vec![wasm::Instr::I64Const(0)],
        Type::Float(FloatType::F32) => vec![wasm::Instr::F32Const(0f32)],
        Type::Float(FloatType::F64) => vec![wasm::Instr::F64Const(0f64)],
        Type::Ptr(..) | Type::ArrayPtr(..) => vec![wasm::Instr::I32Const(0)],
    }
}
