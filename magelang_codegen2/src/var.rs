use crate::ty::{build_val_type, build_zero_wasm_type};
use indexmap::IndexMap;
use magelang_typecheck::{DefId, Module};
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use wasm_helper as wasm;

#[derive(Default)]
pub(crate) struct GlobalMapper<'ctx> {
    globals: Vec<wasm::Global>,
    ids: HashMap<DefId<'ctx>, u32>,
    last_unused: u32,
}

impl<'ctx> GlobalMapper<'ctx> {
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

    pub(crate) fn get(&self, def_id: DefId<'ctx>) -> wasm::GlobalIdx {
        *self.ids.get(&def_id).unwrap()
    }

    pub(crate) fn take(self) -> Vec<wasm::Global> {
        self.globals
    }
}

#[derive(Default)]
pub(crate) struct LocalManager {
    internal: RefCell<LocalManagerInternal>,
}

impl LocalManager {
    pub(crate) fn take(&self) -> Vec<wasm::Local> {
        self.internal.borrow_mut().take()
    }

    pub(crate) fn set_params(&self, parameters: impl Iterator<Item = Vec<wasm::Local>>) {
        self.internal.borrow_mut().set_params(parameters)
    }

    pub(crate) fn new_local(&self, local_id: usize, ty: &[wasm::ValType]) -> u32 {
        self.internal.borrow_mut().new_local(local_id, ty)
    }

    pub(crate) fn get_local(&self, local_id: usize) -> u32 {
        self.internal.borrow().get_local(local_id)
    }

    pub(crate) fn get_temporary_locals(&self, spec: Vec<wasm::ValType>) -> Vec<u32> {
        self.internal.borrow_mut().get_temporary_locals(spec)
    }
}

#[derive(Default)]
struct LocalManagerInternal {
    locals: Vec<wasm::Local>,
    last_used_local: u32,
    local_maps: HashMap<usize, u32>,
    temps: HashMap<wasm::ValType, Vec<u32>>,
}

impl LocalManagerInternal {
    fn take(&mut self) -> Vec<wasm::Local> {
        let locals = std::mem::take(&mut self.locals);
        self.last_used_local = 0;
        self.local_maps = HashMap::default();
        self.temps = HashMap::default();
        locals
    }

    fn set_params(&mut self, parameters: impl Iterator<Item = Vec<wasm::Local>>) {
        for (i, locals) in parameters.enumerate() {
            let local_id = self.last_used_local;
            self.local_maps.insert(i, local_id);
            self.last_used_local += locals.len() as u32;
        }
    }

    fn new_local(&mut self, local_id: usize, ty: &[wasm::ValType]) -> u32 {
        let wasm_local_id = self.last_used_local;
        self.local_maps.insert(local_id, wasm_local_id);
        self.last_used_local += ty.len() as u32;
        self.locals.extend(ty.iter().map(|ty| wasm::Local {
            name: "!local".into(),
            ty: *ty,
        }));
        wasm_local_id
    }

    fn get_local(&self, local_id: usize) -> u32 {
        *self.local_maps.get(&local_id).unwrap()
    }

    fn get_temporary_locals(&mut self, spec: Vec<wasm::ValType>) -> Vec<u32> {
        let mut maps = IndexMap::<wasm::ValType, u32>::default();
        for ty in &spec {
            *maps.entry(*ty).or_default() += 1;
        }

        let mut type_maps = HashMap::<wasm::ValType, VecDeque<u32>>::default();
        for (val_type, required_count) in maps.into_iter() {
            let indexes = self.temps.entry(val_type).or_default();
            while indexes.len() < required_count as usize {
                let local_id = self.last_used_local;
                indexes.push(local_id);
                self.last_used_local += 1;
                self.locals.push(wasm::Local {
                    name: "!temp".to_string(),
                    ty: val_type,
                });
            }
            type_maps.insert(
                val_type,
                indexes
                    .iter()
                    .cloned()
                    .take(required_count as usize)
                    .collect(),
            );
        }

        let mut result = Vec::default();
        for ty in &spec {
            result.push(type_maps.get_mut(ty).unwrap().pop_front().unwrap());
        }
        result
    }
}
