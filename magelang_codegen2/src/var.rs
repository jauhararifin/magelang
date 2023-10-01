use crate::ty::{build_val_type, build_zero_wasm_type};
use magelang_typecheck::{DefId, Module};
use std::collections::HashMap;
use wasm_helper as wasm;

#[derive(Default)]
pub(crate) struct VarMapper<'ctx> {
    globals: Vec<wasm::Global>,
    ids: HashMap<DefId<'ctx>, u32>,
    last_unused: u32,
}

impl<'ctx> VarMapper<'ctx> {
    pub(crate) fn build(module: &Module<'ctx>) -> Self {
        let mut s = Self::default();

        let globals = module.packages.iter().flat_map(|pkg| &pkg.globals);
        for global in globals {
            let val_types = build_val_type(global.ty);

            s.ids.insert(global.name, s.last_unused);
            s.last_unused += val_types.len() as u32;

            for val_type in val_types {
                let init = wasm::Expr(build_zero_wasm_type(&val_type));
                s.globals.push(wasm::Global {
                    ty: wasm::GlobalType {
                        mutability: wasm::Mut::Var,
                        ty: val_type,
                    },
                    init,
                })
            }
        }

        s
    }
}
