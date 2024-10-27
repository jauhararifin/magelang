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
            TypeRepr::UntypedInt => unreachable!("found untyped int type"),
            TypeRepr::UntypedFloat => unreachable!("found untyped float type"),
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
        let mut field_index = Vec::default();
        let mut idx_offset = Vec::default();
        let mut curr_idx = 0;
        for (i, ty) in body.fields.values().enumerate() {
            field_index.push(i);
            let type_layout = self.get_stack_layout(ty);
            idx_offset.push(StackComponent {
                offset: curr_idx,
                size: type_layout.size,
            });
            curr_idx += type_layout.size;
        }
        Rc::new(StackLayout {
            size: curr_idx,
            field_index,
            components: idx_offset,
        })
    }

    pub(crate) fn get_mem_layout(&self, ty: &'ctx Type<'ctx>) -> Option<Rc<MemLayout>> {
        // it is ok to recursively calculate the layout since it's guaranteed that there is no
        // cycle in the struct definition

        Some(match &ty.repr {
            TypeRepr::Unknown => unreachable!("found unknown type"),
            TypeRepr::UntypedInt => unreachable!("found untyped int type"),
            TypeRepr::UntypedFloat => unreachable!("found untyped float type"),
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
        let mut field_idx = Vec::default();
        let mut mem_offset = Vec::default();
        let mut curr_mem = 0;
        let mut total_align = 1;

        for ty in body.fields.values() {
            field_idx.push(mem_offset.len());
            let type_layout = self.get_mem_layout(ty)?;

            let (size, align) = (type_layout.size, type_layout.align);
            if align > total_align {
                total_align = align;
            }
            if curr_mem % align != 0 {
                curr_mem += align - (curr_mem % align);
            }

            for component in &type_layout.components {
                mem_offset.push(MemComponent {
                    offset: curr_mem + component.offset,
                    align: component.align,
                });
            }

            curr_mem += size;
        }

        let mut mem_size = curr_mem;
        if mem_size % total_align != 0 {
            mem_size = mem_size + total_align - (mem_size % total_align)
        }

        Some(Rc::new(MemLayout {
            size: mem_size,
            align: total_align,
            field_idx,
            components: mem_offset,
        }))
    }

    pub(crate) fn take(self) -> Vec<wasm::FuncType> {
        self.func_types.take().into_keys().collect()
    }
}

#[derive(Debug)]
pub(crate) struct MemLayout {
    pub(crate) size: u32,
    pub(crate) align: u32,
    pub(crate) field_idx: Vec<usize>,
    pub(crate) components: Vec<MemComponent>, // only relevant for struct
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct MemComponent {
    pub(crate) offset: u32,
    pub(crate) align: u32,
}

impl MemLayout {
    fn primitive(size: u32, align: u32) -> Self {
        Self {
            size,
            align,
            field_idx: vec![0],
            components: vec![MemComponent { offset: 0, align }],
        }
    }
}

pub(crate) trait AlignNormalize {
    fn normalize(self) -> u32;
}

impl AlignNormalize for u32 {
    fn normalize(self) -> u32 {
        let mut x = self;
        let mut v = 0;
        loop {
            x >>= 1;
            if x == 0 {
                break;
            }
            v += 1;
        }
        v
    }
}

#[derive(Debug, Default)]
pub(crate) struct StackLayout {
    pub(crate) size: u32,
    pub(crate) field_index: Vec<usize>,
    pub(crate) components: Vec<StackComponent>, // only relevant for struct
}

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct StackComponent {
    pub(crate) offset: u32,
    pub(crate) size: u32,
}

impl From<u32> for StackLayout {
    fn from(size: u32) -> Self {
        Self {
            size,
            field_index: vec![0],
            components: vec![StackComponent { offset: 0, size }],
        }
    }
}

pub(crate) fn build_val_type(ty: &Type<'_>) -> Vec<PrimitiveType> {
    match &ty.repr {
        TypeRepr::Unknown | TypeRepr::UntypedInt | TypeRepr::UntypedFloat | TypeRepr::TypeArg(..) => {
            unreachable!("found invalid type {ty}")
        }
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

        TypeRepr::Func(..) => vec![PrimitiveType::U32],

        TypeRepr::Void => vec![],
        TypeRepr::Opaque => vec![PrimitiveType::Extern],
        TypeRepr::Bool => vec![PrimitiveType::U8],

        TypeRepr::Int(true, BitSize::I8) => vec![PrimitiveType::I8],
        TypeRepr::Int(true, BitSize::I16) => vec![PrimitiveType::I16],
        TypeRepr::Int(true, BitSize::I32 | BitSize::ISize) => vec![PrimitiveType::I32],
        TypeRepr::Int(true, BitSize::I64) => vec![PrimitiveType::I64],
        TypeRepr::Int(false, BitSize::I8) => vec![PrimitiveType::U8],
        TypeRepr::Int(false, BitSize::I16) => vec![PrimitiveType::U16],
        TypeRepr::Int(false, BitSize::I32 | BitSize::ISize) => vec![PrimitiveType::U32],
        TypeRepr::Int(false, BitSize::I64) => vec![PrimitiveType::U64],

        TypeRepr::Float(FloatType::F32) => vec![PrimitiveType::F32],
        TypeRepr::Float(FloatType::F64) => vec![PrimitiveType::F64],

        TypeRepr::Ptr(..) => vec![PrimitiveType::U32],
        TypeRepr::ArrayPtr(..) => vec![PrimitiveType::U32],
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum PrimitiveType {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    Extern,
}

impl From<PrimitiveType> for wasm::ValType {
    fn from(val: PrimitiveType) -> Self {
        match val {
            PrimitiveType::I8 => wasm::ValType::Num(wasm::NumType::I32),
            PrimitiveType::U8 => wasm::ValType::Num(wasm::NumType::I32),
            PrimitiveType::I16 => wasm::ValType::Num(wasm::NumType::I32),
            PrimitiveType::U16 => wasm::ValType::Num(wasm::NumType::I32),
            PrimitiveType::I32 => wasm::ValType::Num(wasm::NumType::I32),
            PrimitiveType::U32 => wasm::ValType::Num(wasm::NumType::I32),
            PrimitiveType::I64 => wasm::ValType::Num(wasm::NumType::I64),
            PrimitiveType::U64 => wasm::ValType::Num(wasm::NumType::I64),
            PrimitiveType::F32 => wasm::ValType::Num(wasm::NumType::F32),
            PrimitiveType::F64 => wasm::ValType::Num(wasm::NumType::F64),
            PrimitiveType::Extern => wasm::ValType::Ref(wasm::RefType::ExternRef),
        }
    }
}

impl PrimitiveType {
    pub(crate) fn zero(&self) -> Vec<wasm::Instr> {
        match self {
            Self::I8 | Self::U8 | Self::I16 | Self::U16 | Self::I32 | Self::U32 => {
                vec![wasm::Instr::I32Const(0)]
            }
            Self::I64 | Self::U64 => vec![wasm::Instr::I64Const(0)],
            Self::F32 => vec![wasm::Instr::F32Const(0.0)],
            Self::F64 => vec![wasm::Instr::F64Const(0.0)],
            Self::Extern => vec![wasm::Instr::RefNull(wasm::RefType::ExternRef)],
        }
    }
}
