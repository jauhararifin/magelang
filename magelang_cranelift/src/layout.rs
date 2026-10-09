use anyhow::{bail, ensure, Result};
use cranelift_codegen::ir::{self, types, AbiParam, Signature};
use cranelift_module::Module;
use magelang_typecheck::{BitSize, FloatType, FuncType, Type, TypeRepr};
use std::ops::Range;

pub(crate) struct Layouts {
    pub pointer: ir::Type,
}

pub(crate) struct Layout {
    pub size: u32,
    pub align: u32,
    pub components: Vec<Component>,
    pub fields: Vec<Field>,
}

pub(crate) struct Component {
    pub ty: ir::Type,
    pub offset: i32,
    pub signed: bool,
}

pub(crate) struct Field {
    pub offset: u32,
    pub components: Range<usize>,
}

pub(crate) fn aggregate(ty: &Type<'_>) -> bool {
    matches!(ty.repr, TypeRepr::Struct(..) | TypeRepr::SlicePtr(..))
}

impl Layouts {
    pub fn get(&self, ty: &Type<'_>) -> Result<Layout> {
        let scalar = match &ty.repr {
            TypeRepr::Void => return Ok(Layout { size: 0, align: 1, components: vec![], fields: vec![] }),
            TypeRepr::Bool => types::I8,
            TypeRepr::Int(_, BitSize::I8) => types::I8,
            TypeRepr::Int(_, BitSize::I16) => types::I16,
            TypeRepr::Int(_, BitSize::I32) => types::I32,
            TypeRepr::Int(_, BitSize::I64) => types::I64,
            TypeRepr::Int(_, BitSize::ISize) | TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..) | TypeRepr::Func(..) => {
                self.pointer
            }
            TypeRepr::Float(FloatType::F32) => types::F32,
            TypeRepr::Float(FloatType::F64) => types::F64,
            TypeRepr::SlicePtr(..) => {
                let size = self.pointer.bytes();
                return Ok(Layout {
                    size: size * 2,
                    align: size,
                    components: vec![
                        Component { ty: self.pointer, offset: 0, signed: false },
                        Component { ty: self.pointer, offset: size as i32, signed: false },
                    ],
                    fields: vec![Field { offset: 0, components: 0..1 }, Field { offset: size, components: 1..2 }],
                });
            }
            TypeRepr::Struct(struct_type) => {
                let mut result = Layout { size: 0, align: 1, components: vec![], fields: vec![] };
                for field in struct_type.body.get().unwrap().fields.values() {
                    let layout = self.get(field)?;
                    let offset = result.size.next_multiple_of(layout.align);
                    result.size = offset
                        .checked_add(layout.size)
                        .filter(|size| *size <= i32::MAX as u32)
                        .ok_or_else(|| anyhow::anyhow!("native type is too large: {ty}"))?;
                    result.align = result.align.max(layout.align);
                    let start = result.components.len();
                    result.components.extend(
                        layout.components.into_iter().map(|c| Component { offset: c.offset + offset as i32, ..c }),
                    );
                    result.fields.push(Field { offset, components: start..result.components.len() });
                }
                result.size = result.size.next_multiple_of(result.align);
                return Ok(result);
            }
            TypeRepr::Opaque => bail!("Wasm opaque references are not supported by the native backend; use a pointer"),
            _ => bail!("unresolved type in native codegen: {ty}"),
        };
        Ok(Layout {
            size: scalar.bytes(),
            align: scalar.bytes(),
            components: vec![Component { ty: scalar, offset: 0, signed: matches!(ty.repr, TypeRepr::Int(true, _)) }],
            fields: vec![],
        })
    }

    pub fn signature(&self, module: &impl Module, ty: &FuncType<'_>) -> Result<Signature> {
        let mut signature = module.make_signature();
        // Aggregate returns use a caller-owned buffer, avoiding platform-specific struct return ABIs.
        if aggregate(ty.return_type) {
            signature.params.push(AbiParam::new(self.pointer));
        } else {
            signature.returns.extend(self.get(ty.return_type)?.components.iter().map(Component::abi_param));
        }
        for param in ty.params {
            signature.params.extend(self.get(param)?.components.iter().map(Component::abi_param));
        }
        Ok(signature)
    }

    pub fn check_ffi(&self, ty: &FuncType<'_>) -> Result<()> {
        for param in ty.params.iter().copied().chain([ty.return_type]) {
            ensure!(
                !aggregate(param),
                "native imports only support scalar/pointer parameters and returns; pass aggregates by pointer"
            );
            self.get(param)?;
            if let TypeRepr::Func(function) = &param.repr {
                self.check_ffi(function)?;
            }
        }
        ensure!(!ty.params.iter().any(|param| param.is_void()), "native imports cannot have void parameters");
        Ok(())
    }
}

impl Component {
    fn abi_param(&self) -> AbiParam {
        let param = AbiParam::new(self.ty);
        if self.ty.is_int() && self.ty.bits() < 64 {
            if self.signed {
                param.sext()
            } else {
                param.uext()
            }
        } else {
            param
        }
    }
}
