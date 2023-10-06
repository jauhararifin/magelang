use magelang_typecheck::{BitSize, FloatType, Type, TypeRepr};
use wasm_helper as wasm;

pub(crate) fn build_val_type(ty: &Type<'_>) -> Vec<wasm::ValType> {
    match &ty.repr {
        TypeRepr::Unknown | TypeRepr::TypeArg(..) => unreachable!("found invalid type {ty}"),
        TypeRepr::Struct(struct_type) => {
            let mut fields = vec![];
            for field_ty in struct_type
                .body
                .get()
                .expect("missing struct body")
                .fields
                .values()
            {
                fields.extend(build_val_type(field_ty));
            }
            fields
        }

        TypeRepr::Func(..) => vec![wasm::ValType::Num(wasm::NumType::I32)],

        TypeRepr::Void => vec![],
        TypeRepr::Opaque => vec![wasm::ValType::Ref(wasm::RefType::ExternRef)],
        TypeRepr::Bool => vec![wasm::ValType::Num(wasm::NumType::I32)],

        TypeRepr::Int(_, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
            vec![wasm::ValType::Num(wasm::NumType::I32)]
        }
        TypeRepr::Int(_, BitSize::I64) => vec![wasm::ValType::Num(wasm::NumType::I64)],

        TypeRepr::Float(FloatType::F32) => vec![wasm::ValType::Num(wasm::NumType::F32)],
        TypeRepr::Float(FloatType::F64) => vec![wasm::ValType::Num(wasm::NumType::F64)],

        TypeRepr::Ptr(..) => vec![wasm::ValType::Num(wasm::NumType::I32)],
        TypeRepr::ArrayPtr(..) => vec![wasm::ValType::Num(wasm::NumType::I32)],
    }
}

pub(crate) fn build_zero_wasm_type(ty: &wasm::ValType) -> Vec<wasm::Instr> {
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

pub(crate) fn build_zero_type(ty: &Type<'_>) -> Vec<wasm::Instr> {
    match &ty.repr {
        TypeRepr::Unknown | TypeRepr::TypeArg(..) => unreachable!("found invalid type"),
        TypeRepr::Struct(struct_type) => struct_type
            .body
            .get()
            .expect("missing struct body")
            .fields
            .values()
            .flat_map(|ty| build_zero_type(ty))
            .collect(),
        TypeRepr::Func(..) => vec![wasm::Instr::I32Const(0)],
        TypeRepr::Void => vec![],
        TypeRepr::Opaque => vec![wasm::Instr::RefNull(wasm::RefType::ExternRef)],
        TypeRepr::Bool => vec![wasm::Instr::I32Const(0)],
        TypeRepr::Int(_, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
            vec![wasm::Instr::I32Const(0)]
        }
        TypeRepr::Int(_, BitSize::I64) => vec![wasm::Instr::I64Const(0)],
        TypeRepr::Float(FloatType::F32) => vec![wasm::Instr::F32Const(0f32)],
        TypeRepr::Float(FloatType::F64) => vec![wasm::Instr::F64Const(0f64)],
        TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..) => vec![wasm::Instr::I32Const(0)],
    }
}
