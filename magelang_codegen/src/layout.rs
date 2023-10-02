use magelang_typecheck::{BitSize, FloatType, StructBody, Type};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Default)]
pub(crate) struct LayoutManager<'ctx> {
    internal: RefCell<LayoutManagerInternal<'ctx>>,
}

#[derive(Default)]
struct LayoutManagerInternal<'ctx> {
    mems: HashMap<&'ctx Type<'ctx>, Rc<MemLayout>>,
    stacks: HashMap<&'ctx Type<'ctx>, Rc<StackLayout>>,
}

impl<'ctx> LayoutManager<'ctx> {
    pub(crate) fn get_stack_layout(&self, ty: &'ctx Type<'ctx>) -> Rc<StackLayout> {
        // Note: it is ok to recursively calculate the layout since it's guaranteed that there is no
        // cycle in the struct definition

        match ty {
            Type::Unknown => unreachable!("found unknown type"),
            Type::TypeArg(..) => unreachable!("found typearg type"),
            Type::Struct(struct_type) => {
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
            Type::Inst(inst_type) => {
                {
                    let internal = self.internal.borrow_mut();
                    if let Some(val) = internal.stacks.get(&ty) {
                        return val.clone();
                    }
                }

                let layout = self
                    .get_struct_stack_layout(inst_type.body.get().expect("missing struct body"));

                let mut internal = self.internal.borrow_mut();
                internal.stacks.insert(ty, layout.clone());
                layout
            }
            Type::Func(..) => Rc::new(1u32.into()),
            Type::Void => Rc::new(0u32.into()),
            Type::Opaque => Rc::new(1u32.into()),
            Type::Bool => Rc::new(1u32.into()),
            Type::Int(_, BitSize::I8) => Rc::new(1u32.into()),
            Type::Int(_, BitSize::I16) => Rc::new(1u32.into()),
            Type::Int(_, BitSize::I32) => Rc::new(1u32.into()),
            Type::Int(_, BitSize::I64) => Rc::new(1u32.into()),
            Type::Int(_, BitSize::ISize) => Rc::new(1u32.into()),
            Type::Float(FloatType::F32) => Rc::new(1u32.into()),
            Type::Float(FloatType::F64) => Rc::new(1u32.into()),
            Type::Ptr(..) | Type::ArrayPtr(..) => Rc::new(1u32.into()),
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

    pub(crate) fn get_mem_layout(&self, ty: &'ctx Type<'ctx>) -> Rc<MemLayout> {
        // it is ok to recursively calculate the layout since it's guaranteed that there is no
        // cycle in the struct definition

        match ty {
            Type::Unknown => unreachable!("found unknown type"),
            Type::TypeArg(..) => unreachable!("found typearg type"),
            Type::Opaque => unreachable!("opaque doesn't have memory representation"),
            Type::Struct(struct_type) => {
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
                layout
            }
            Type::Inst(inst_type) => {
                {
                    let internal = self.internal.borrow_mut();
                    if let Some(val) = internal.mems.get(&ty) {
                        return val.clone();
                    }
                }

                let layout =
                    self.get_struct_mem_layout(inst_type.body.get().expect("missing struct body"));

                let mut internal = self.internal.borrow_mut();
                internal.mems.insert(ty, layout.clone());
                layout
            }
            Type::Func(..) => Rc::new(MemLayout::primitive(4, 4)),
            Type::Void => Rc::new(MemLayout::primitive(0, 1)),
            Type::Bool => Rc::new(MemLayout::primitive(1, 1)),
            Type::Int(_, BitSize::I8) => Rc::new(MemLayout::primitive(1, 1)),
            Type::Int(_, BitSize::I16) => Rc::new(MemLayout::primitive(2, 2)),
            Type::Int(_, BitSize::I32) => Rc::new(MemLayout::primitive(4, 4)),
            Type::Int(_, BitSize::I64) => Rc::new(MemLayout::primitive(8, 8)),
            Type::Int(_, BitSize::ISize) => Rc::new(MemLayout::primitive(4, 4)),
            Type::Float(FloatType::F32) => Rc::new(MemLayout::primitive(4, 4)),
            Type::Float(FloatType::F64) => Rc::new(MemLayout::primitive(8, 8)),
            Type::Ptr(..) | Type::ArrayPtr(..) => Rc::new(MemLayout::primitive(4, 4)),
        }
    }

    fn get_struct_mem_layout(&self, body: &StructBody<'ctx>) -> Rc<MemLayout> {
        let mut mem_offset = Vec::default();
        let mut curr_mem = 0;
        let mut total_align = 1;

        for ty in body.fields.values() {
            let type_layout = self.get_mem_layout(ty);

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

        Rc::new(MemLayout {
            size: mem_size,
            align: total_align,
            offset: mem_offset,
        })
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
