use crate::context::Context;
use indexmap::IndexMap;
use magelang_typecheck::{BitSize, FloatType, StructBody, Type, TypeRepr};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use wasm_helper as wasm;

#[derive(Default)]
pub(crate) struct TypeManager<'ctx> {
    func_types: RefCell<IndexMap<wasm::FuncType, wasm::TypeIdx>>,
    internal: RefCell<LayoutManagerInternal<'ctx>>,
}

#[derive(Default)]
struct LayoutManagerInternal<'ctx> {
    mems: HashMap<&'ctx Type<'ctx>, Option<Rc<MemLayout>>>,
    stacks: HashMap<&'ctx Type<'ctx>, Rc<StackLayout>>,
}

impl<'ctx> TypeManager<'ctx> {
    pub(crate) fn build<E>(ctx: Context<'ctx, E>) -> Self {
        todo!();
    }

    pub(crate) fn get_func_type(&self, func_type: wasm::FuncType) -> wasm::TypeIdx {
        let mut func_type_cache = self.func_types.borrow_mut();
        let next_id = func_type_cache.len() as wasm::TypeIdx;
        *func_type_cache.entry(func_type).or_insert(next_id)
    }

    pub(crate) fn get_stack_layout(&self, ty: &'ctx Type<'ctx>) -> Rc<StackLayout> {
        // Note: it is ok to recursively calculate the layout since it's guaranteed that there is no
        // cycle in the struct definition

        match &ty.repr {
            TypeRepr::Unknown => unreachable!("found unknown type"),
            TypeRepr::TypeArg(..) => unreachable!("found typearg type"),
            TypeRepr::Struct(struct_type) => {
                {
                    let internal = self.internal.borrow_mut();
                    if let Some(val) = internal.stacks.get(&ty) {
                        return val.clone();
                    }
                }

                let layout = self
                    .get_struct_stack_layout(struct_type.body.get().expect("missing struct body"));

                let mut internal = self.internal.borrow_mut();
                internal.stacks.insert(ty, layout.clone());
                layout
            }
            TypeRepr::Func(..) => Rc::new(1u32.into()),
            TypeRepr::Void => Rc::new(0u32.into()),
            TypeRepr::Opaque => Rc::new(1u32.into()),
            TypeRepr::Bool => Rc::new(1u32.into()),
            TypeRepr::Int(_, BitSize::I8) => Rc::new(1u32.into()),
            TypeRepr::Int(_, BitSize::I16) => Rc::new(1u32.into()),
            TypeRepr::Int(_, BitSize::I32) => Rc::new(1u32.into()),
            TypeRepr::Int(_, BitSize::I64) => Rc::new(1u32.into()),
            TypeRepr::Int(_, BitSize::ISize) => Rc::new(1u32.into()),
            TypeRepr::Float(FloatType::F32) => Rc::new(1u32.into()),
            TypeRepr::Float(FloatType::F64) => Rc::new(1u32.into()),
            TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..) => Rc::new(1u32.into()),
        }
    }

    fn get_struct_stack_layout(&self, body: &StructBody<'ctx>) -> Rc<StackLayout> {
        let mut idx_offset = Vec::default();
        let mut curr_idx = 0;
        for ty in body.fields.values() {
            idx_offset.push(curr_idx);
            let type_layout = self.get_stack_layout(ty);
            curr_idx += type_layout.size;
        }
        Rc::new(StackLayout {
            size: curr_idx,
            offset: idx_offset,
        })
    }

    pub(crate) fn get_mem_layout(&self, ty: &'ctx Type<'ctx>) -> Option<Rc<MemLayout>> {
        // it is ok to recursively calculate the layout since it's guaranteed that there is no
        // cycle in the struct definition

        Some(match &ty.repr {
            TypeRepr::Unknown => unreachable!("found unknown type"),
            TypeRepr::TypeArg(..) => unreachable!("found typearg type"),
            TypeRepr::Opaque => return None,
            TypeRepr::Struct(struct_type) => {
                {
                    let internal = self.internal.borrow_mut();
                    if let Some(val) = internal.mems.get(&ty) {
                        return val.clone();
                    }
                }

                let layout = self
                    .get_struct_mem_layout(struct_type.body.get().expect("missing struct body"));

                let mut internal = self.internal.borrow_mut();
                internal.mems.insert(ty, layout.clone());
                return layout;
            }
            TypeRepr::Func(..) => Rc::new(MemLayout::primitive(4, 4)),
            TypeRepr::Void => Rc::new(MemLayout::primitive(0, 1)),
            TypeRepr::Bool => Rc::new(MemLayout::primitive(1, 1)),
            TypeRepr::Int(_, BitSize::I8) => Rc::new(MemLayout::primitive(1, 1)),
            TypeRepr::Int(_, BitSize::I16) => Rc::new(MemLayout::primitive(2, 2)),
            TypeRepr::Int(_, BitSize::I32) => Rc::new(MemLayout::primitive(4, 4)),
            TypeRepr::Int(_, BitSize::I64) => Rc::new(MemLayout::primitive(8, 8)),
            TypeRepr::Int(_, BitSize::ISize) => Rc::new(MemLayout::primitive(4, 4)),
            TypeRepr::Float(FloatType::F32) => Rc::new(MemLayout::primitive(4, 4)),
            TypeRepr::Float(FloatType::F64) => Rc::new(MemLayout::primitive(8, 8)),
            TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..) => Rc::new(MemLayout::primitive(4, 4)),
        })
    }

    fn get_struct_mem_layout(&self, body: &StructBody<'ctx>) -> Option<Rc<MemLayout>> {
        let mut mem_offset = Vec::default();
        let mut curr_mem = 0;
        let mut total_align = 1;

        for ty in body.fields.values() {
            let type_layout = self.get_mem_layout(ty)?;

            let (size, align) = (type_layout.size, type_layout.align);
            if align > total_align {
                total_align = align;
            }
            if curr_mem % align != 0 {
                curr_mem += align - (curr_mem % align);
            }
            mem_offset.push(curr_mem);

            curr_mem += size;
        }

        let mut mem_size = curr_mem;
        if mem_size % total_align != 0 {
            mem_size = mem_size + total_align - (mem_size % total_align)
        }

        Some(Rc::new(MemLayout {
            size: mem_size,
            align: total_align,
            offset: mem_offset,
        }))
    }
}

pub(crate) struct MemLayout {
    pub(crate) size: u32,
    pub(crate) align: u32,
    pub(crate) offset: Vec<u32>, // only relevant for struct
}

impl MemLayout {
    fn primitive(size: u32, align: u32) -> Self {
        Self {
            size,
            align,
            offset: Vec::default(),
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct StackLayout {
    pub(crate) size: u32,
    pub(crate) offset: Vec<u32>, // only relevant for struct
}

impl From<u32> for StackLayout {
    fn from(size: u32) -> Self {
        Self {
            size,
            offset: Vec::default(),
        }
    }
}

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
