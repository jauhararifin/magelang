use crate::expr::build_zero_expr;
use crate::ty::build_val_type;
use magelang_typecheck::{DefId, Module};
use std::collections::HashMap;
use wasm_helper as wasm;

#[derive(Default)]
pub(crate) struct GlobalManager<'ctx> {
    globals: Vec<wasm::Global>,
    ids: HashMap<DefId<'ctx>, u32>,
    last_unused: u32,
}

impl<'ctx> GlobalManager<'ctx> {
    pub(crate) fn build(module: &Module<'ctx>) -> Self {
        let mut s = Self::default();

        let globals = module.packages.iter().flat_map(|pkg| &pkg.globals);
        for global in globals {
            let val_types = build_val_type(global.ty);

            s.ids.insert(global.name, s.last_unused);
            s.last_unused += val_types.len() as u32;

            for val_type in val_types {
                let init = wasm::Expr(build_zero_expr(&val_type));
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

    pub(crate) fn get(&self, def_id: DefId<'ctx>) -> wasm::GlobalIdx {
        *self.ids.get(&def_id).unwrap()
    }

    pub(crate) fn take(self) -> Vec<wasm::Global> {
        self.globals
    }
}
