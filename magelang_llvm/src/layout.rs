use anyhow::{bail, ensure, Result};
use magelang_typecheck::{BitSize, FloatType, FuncType, Type, TypeRepr};
use std::fmt::{self, Display};
use std::ops::Range;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Scalar {
    Int(u32),
    Float(u32),
    Pointer,
}

impl Display for Scalar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(bits) => write!(f, "i{bits}"),
            Self::Float(32) => f.write_str("float"),
            Self::Float(64) => f.write_str("double"),
            Self::Pointer => f.write_str("ptr"),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone)]
pub(crate) struct Value {
    pub ty: Scalar,
    pub name: String,
}

impl Value {
    pub fn int(bits: u32, value: u64) -> Self {
        Self { ty: Scalar::Int(bits), name: (value & (u64::MAX >> (64 - bits))).to_string() }
    }

    pub fn float(bits: u32, value: f64) -> Self {
        let value = if bits == 32 { (value as f32) as f64 } else { value };
        Self { ty: Scalar::Float(bits), name: format!("0x{:016X}", value.to_bits()) }
    }

    pub fn pointer(name: String) -> Self {
        Self { ty: Scalar::Pointer, name }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.ty, self.name)
    }
}

#[derive(Clone, Copy)]
pub(crate) struct AbiType {
    pub scalar: Scalar,
    pub signed: bool,
}

impl AbiType {
    fn extension(self) -> &'static str {
        // AAPCS64 leaves the upper bits of narrow integer arguments/results unspecified.
        if cfg!(all(target_arch = "aarch64", target_os = "linux")) {
            return "";
        }
        if matches!(self.scalar, Scalar::Int(bits) if bits < 32) {
            if self.signed {
                "signext "
            } else {
                "zeroext "
            }
        } else {
            ""
        }
    }

    pub fn result(self) -> String {
        format!("{}{}", self.extension(), self.scalar)
    }

    pub fn parameter(self, name: &str) -> String {
        format!("{} {}{name}", self.scalar, self.extension())
    }
}

pub(crate) struct Signature {
    pub result: Option<AbiType>,
    pub params: Vec<AbiType>,
}

impl Signature {
    pub fn result(&self) -> String {
        self.result.map(AbiType::result).unwrap_or_else(|| "void".into())
    }
}

pub(crate) struct Layout {
    pub size: u32,
    pub align: u32,
    pub components: Vec<Component>,
    pub fields: Vec<Field>,
}

pub(crate) struct Component {
    pub ty: AbiType,
    pub offset: u32,
}

pub(crate) struct Field {
    pub offset: u32,
    pub components: Range<usize>,
}

pub(crate) fn aggregate(ty: &Type<'_>) -> bool {
    matches!(ty.repr, TypeRepr::Struct(..) | TypeRepr::SlicePtr(..))
}

pub(crate) fn layout(ty: &Type<'_>) -> Result<Layout> {
    let scalar = match &ty.repr {
        TypeRepr::Void => return Ok(Layout { size: 0, align: 1, components: vec![], fields: vec![] }),
        TypeRepr::Bool => Scalar::Int(1),
        TypeRepr::Int(_, BitSize::I8) => Scalar::Int(8),
        TypeRepr::Int(_, BitSize::I16) => Scalar::Int(16),
        TypeRepr::Int(_, BitSize::I32) => Scalar::Int(32),
        TypeRepr::Int(_, BitSize::I64 | BitSize::ISize) => Scalar::Int(64),
        TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..) | TypeRepr::Func(..) => Scalar::Pointer,
        TypeRepr::Float(FloatType::F32) => Scalar::Float(32),
        TypeRepr::Float(FloatType::F64) => Scalar::Float(64),
        TypeRepr::SlicePtr(..) => {
            return Ok(Layout {
                size: 16,
                align: 8,
                components: vec![
                    Component { ty: AbiType { scalar: Scalar::Pointer, signed: false }, offset: 0 },
                    Component { ty: AbiType { scalar: Scalar::Int(64), signed: false }, offset: 8 },
                ],
                fields: vec![Field { offset: 0, components: 0..1 }, Field { offset: 8, components: 1..2 }],
            })
        }
        TypeRepr::Struct(struct_type) => {
            let mut result = Layout { size: 0, align: 1, components: vec![], fields: vec![] };
            for field in struct_type.body.get().unwrap().fields.values() {
                let field = layout(field)?;
                let offset = result.size.next_multiple_of(field.align);
                result.size = offset
                    .checked_add(field.size)
                    .filter(|size| *size <= i32::MAX as u32)
                    .ok_or_else(|| anyhow::anyhow!("native type is too large: {ty}"))?;
                result.align = result.align.max(field.align);
                let start = result.components.len();
                result
                    .components
                    .extend(field.components.into_iter().map(|c| Component { offset: c.offset + offset, ..c }));
                result.fields.push(Field { offset, components: start..result.components.len() });
            }
            result.size = result.size.next_multiple_of(result.align);
            return Ok(result);
        }
        TypeRepr::Opaque => bail!("Wasm opaque references are not supported by the native backend; use a pointer"),
        _ => bail!("unresolved type in LLVM codegen: {ty}"),
    };
    let size = match scalar {
        Scalar::Int(bits) | Scalar::Float(bits) => bits.div_ceil(8),
        Scalar::Pointer => 8,
    };
    Ok(Layout {
        size,
        align: size,
        components: vec![Component {
            ty: AbiType { scalar, signed: matches!(ty.repr, TypeRepr::Int(true, _)) },
            offset: 0,
        }],
        fields: vec![],
    })
}

pub(crate) fn signature(ty: &FuncType<'_>) -> Result<Signature> {
    let mut result = Signature { result: None, params: vec![] };
    if aggregate(ty.return_type) {
        result.params.push(AbiType { scalar: Scalar::Pointer, signed: false });
    } else {
        result.result = layout(ty.return_type)?.components.first().map(|c| c.ty);
    }
    for param in ty.params {
        result.params.extend(layout(param)?.components.iter().map(|c| c.ty));
    }
    Ok(result)
}

pub(crate) fn check_ffi(ty: &FuncType<'_>) -> Result<()> {
    for param in ty.params.iter().copied().chain([ty.return_type]) {
        ensure!(
            !aggregate(param),
            "native imports only support scalar/pointer parameters and returns; pass aggregates by pointer"
        );
        layout(param)?;
        if let TypeRepr::Func(function) = &param.repr {
            check_ffi(function)?;
        }
    }
    ensure!(!ty.params.iter().any(|param| param.is_void()), "native imports cannot have void parameters");
    Ok(())
}
