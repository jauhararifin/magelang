use wasm_helper as wasm;

pub(crate) fn build_zero_expr(ty: &wasm::ValType) -> Vec<wasm::Instr> {
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
