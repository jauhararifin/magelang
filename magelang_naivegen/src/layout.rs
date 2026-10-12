use magelang_typecheck::{BitSize, FloatType, Type, TypeRepr};

#[derive(Clone, Copy, Debug)]
pub(crate) enum Scalar {
    Int { signed: bool, bits: usize },
    F32,
    F64,
}

impl Scalar {
    pub(crate) fn size(self) -> usize {
        match self {
            Self::Int { bits, .. } => bits / 8,
            Self::F32 => 4,
            Self::F64 => 8,
        }
    }

    pub(crate) fn signed(self) -> bool {
        matches!(self, Self::Int { signed: true, .. })
    }

    pub(crate) fn float(self) -> bool {
        matches!(self, Self::F32 | Self::F64)
    }
}

#[derive(Clone, Copy)]
pub(crate) struct Component {
    pub(crate) offset: usize,
    pub(crate) scalar: Scalar,
}

pub(crate) struct Field {
    pub(crate) offset: usize,
    pub(crate) first: usize,
    pub(crate) count: usize,
}

pub(crate) struct Layout {
    pub(crate) size: usize,
    pub(crate) align: usize,
    pub(crate) components: Vec<Component>,
    pub(crate) fields: Vec<Field>,
}

pub(crate) fn align_up(size: usize, align: usize) -> usize {
    size.div_ceil(align) * align
}

impl Layout {
    pub(crate) fn of(ty: &Type<'_>) -> Self {
        let scalar = match &ty.repr {
            TypeRepr::Void => {
                return Self { size: 0, align: 1, components: vec![], fields: vec![] };
            }
            TypeRepr::Struct(body) => {
                let mut result = Self { size: 0, align: 1, components: vec![], fields: vec![] };
                for ty in body.body.get().expect("resolved struct body").fields.values() {
                    let field = Self::of(ty);
                    result.size = align_up(result.size, field.align);
                    result.fields.push(Field {
                        offset: result.size,
                        first: result.components.len(),
                        count: field.components.len(),
                    });
                    result.components.extend(field.components.into_iter().map(|component| Component {
                        offset: result.size + component.offset,
                        scalar: component.scalar,
                    }));
                    result.size += field.size;
                    result.align = result.align.max(field.align);
                }
                result.size = align_up(result.size, result.align);
                return result;
            }
            TypeRepr::SlicePtr(..) => {
                let scalar = Scalar::Int { signed: false, bits: 64 };
                return Self {
                    size: 16,
                    align: 8,
                    components: vec![Component { offset: 0, scalar }, Component { offset: 8, scalar }],
                    fields: vec![Field { offset: 0, first: 0, count: 1 }, Field { offset: 8, first: 1, count: 1 }],
                };
            }
            TypeRepr::Int(signed, size) => Scalar::Int {
                signed: *signed,
                bits: match size {
                    BitSize::I8 => 8,
                    BitSize::I16 => 16,
                    BitSize::I32 => 32,
                    BitSize::I64 | BitSize::ISize => 64,
                },
            },
            TypeRepr::Bool => Scalar::Int { signed: false, bits: 8 },
            TypeRepr::Float(FloatType::F32) => Scalar::F32,
            TypeRepr::Float(FloatType::F64) => Scalar::F64,
            TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..) | TypeRepr::Func(..) | TypeRepr::Opaque => {
                Scalar::Int { signed: false, bits: 64 }
            }
            _ => unreachable!("unresolved type in valid module: {ty}"),
        };
        Self {
            size: scalar.size(),
            align: scalar.size(),
            components: vec![Component { offset: 0, scalar }],
            fields: vec![],
        }
    }

    pub(crate) fn stack_size(&self) -> usize {
        self.components.len() * 16
    }
}
