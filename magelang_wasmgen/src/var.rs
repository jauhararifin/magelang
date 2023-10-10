use crate::ty::{build_val_type, PrimitiveType};
use magelang_typecheck::{DefId, Module};
use std::cell::RefCell;
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
                let init = wasm::Expr(val_type.zero());
                s.globals.push(wasm::Global {
                    ty: wasm::GlobalType {
                        mutability: wasm::Mut::Var,
                        ty: val_type.into(),
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

pub(crate) struct LocalManager {
    internal: RefCell<LocalManagerInternal>,
}

#[derive(Default)]
struct LocalManagerInternal {
    locals: Vec<wasm::Local>,
    last_unused_local: wasm::LocalIdx,
    local_maps: HashMap<usize, wasm::LocalIdx>,
    temps: HashMap<wasm::ValType, Vec<wasm::LocalIdx>>,
    is_in_use: Vec<bool>,
}

impl LocalManager {
    pub(crate) fn new(parameters: impl Iterator<Item = Vec<wasm::Local>>) -> Self {
        let mut internal = LocalManagerInternal::default();

        for (i, locals) in parameters.enumerate() {
            let local_id = internal.last_unused_local;
            internal.local_maps.insert(i, local_id);
            internal.last_unused_local += locals.len() as u32;
            internal
                .is_in_use
                .resize(internal.is_in_use.len() + locals.len(), true);
        }

        Self {
            internal: RefCell::new(internal),
        }
    }

    pub(crate) fn take(&self) -> Vec<wasm::Local> {
        let mut internal = self.internal.borrow_mut();

        let locals = std::mem::take(&mut internal.locals);
        std::mem::take(&mut internal.is_in_use);
        internal.last_unused_local = 0;
        internal.local_maps = HashMap::default();
        internal.temps = HashMap::default();
        locals
    }

    pub(crate) fn new_local(
        &self,
        local_id: usize,
        types: impl Iterator<Item = PrimitiveType>,
    ) -> u32 {
        let mut internal = self.internal.borrow_mut();

        let wasm_local_id = internal.last_unused_local;
        internal.local_maps.insert(local_id, wasm_local_id);
        for ty in types {
            internal.last_unused_local += 1;
            internal.locals.push(wasm::Local {
                name: "!local".into(),
                ty: ty.into(),
            });
            internal.is_in_use.push(true);
        }
        wasm_local_id
    }

    pub(crate) fn get_local(&self, local_id: usize) -> u32 {
        *self.internal.borrow().local_maps.get(&local_id).unwrap()
    }

    pub(crate) fn get_temporary_locals(&self, spec: Vec<PrimitiveType>) -> LocalTemp<'_> {
        let spec = spec.into_iter().map(Into::<wasm::ValType>::into).collect();
        let mut internal = self.internal.borrow_mut();
        let result = internal.get_temporary_locals(spec);
        LocalTemp {
            result,
            manager: self,
        }
    }
}

impl LocalManagerInternal {
    fn get_temporary_locals(&mut self, spec: Vec<wasm::ValType>) -> Vec<u32> {
        let mut result = Vec::default();

        let mut last_iter = HashMap::<wasm::ValType, usize>::default();
        'spec: for ty in spec {
            let indexes = self.temps.entry(ty).or_default();
            let i = last_iter.entry(ty).or_default();

            while *i < indexes.len() {
                let idx = indexes[*i];
                *i += 1;

                if !self.is_in_use[idx as usize] {
                    result.push(idx);
                    self.is_in_use[idx as usize] = true;
                    continue 'spec;
                }
            }

            let local_id = self.last_unused_local;
            self.last_unused_local += 1;
            self.locals.push(wasm::Local {
                name: "!temp".to_string(),
                ty,
            });
            self.is_in_use.push(true);
            indexes.push(local_id);
            result.push(local_id);
            *i += 1;
        }

        result
    }
}

pub(crate) struct LocalTemp<'a> {
    result: Vec<u32>,
    manager: &'a LocalManager,
}

impl<'a> std::fmt::Debug for LocalTemp<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self.result)
    }
}

impl<'a> std::ops::Deref for LocalTemp<'a> {
    type Target = Vec<u32>;
    fn deref(&self) -> &Self::Target {
        &self.result
    }
}

impl<'a> Drop for LocalTemp<'a> {
    fn drop(&mut self) {
        let mut internal = self.manager.internal.borrow_mut();
        for idx in &self.result {
            internal.is_in_use[*idx as usize] = false;
        }
    }
}
