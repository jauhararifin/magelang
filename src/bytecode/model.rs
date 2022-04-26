// Automatically generated rust module for 'model.proto' file

#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_imports)]
#![allow(unknown_lints)]
#![allow(clippy::all)]
#![cfg_attr(rustfmt, rustfmt_skip)]


use std::borrow::Cow;
use quick_protobuf::{MessageRead, MessageWrite, BytesReader, Writer, WriterBackend, Result};
use quick_protobuf::sizeofs::*;
use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TypeKind {
    I8 = 1,
    I16 = 2,
    I32 = 3,
    I64 = 4,
    U8 = 5,
    U16 = 6,
    U32 = 7,
    U64 = 8,
    F32 = 9,
    F64 = 10,
    Bool = 11,
    Struct = 12,
    Object = 13,
}

impl Default for TypeKind {
    fn default() -> Self {
        TypeKind::I8
    }
}

impl From<i32> for TypeKind {
    fn from(i: i32) -> Self {
        match i {
            1 => TypeKind::I8,
            2 => TypeKind::I16,
            3 => TypeKind::I32,
            4 => TypeKind::I64,
            5 => TypeKind::U8,
            6 => TypeKind::U16,
            7 => TypeKind::U32,
            8 => TypeKind::U64,
            9 => TypeKind::F32,
            10 => TypeKind::F64,
            11 => TypeKind::Bool,
            12 => TypeKind::Struct,
            13 => TypeKind::Object,
            _ => Self::default(),
        }
    }
}

impl<'a> From<&'a str> for TypeKind {
    fn from(s: &'a str) -> Self {
        match s {
            "I8" => TypeKind::I8,
            "I16" => TypeKind::I16,
            "I32" => TypeKind::I32,
            "I64" => TypeKind::I64,
            "U8" => TypeKind::U8,
            "U16" => TypeKind::U16,
            "U32" => TypeKind::U32,
            "U64" => TypeKind::U64,
            "F32" => TypeKind::F32,
            "F64" => TypeKind::F64,
            "Bool" => TypeKind::Bool,
            "Struct" => TypeKind::Struct,
            "Object" => TypeKind::Object,
            _ => Self::default(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OpCode {
    Const = 0,
    Add = 1,
    Sub = 2,
    Div = 3,
    Mul = 4,
    Mod = 5,
    Eq = 6,
    NEq = 7,
    LT = 8,
    LTEq = 9,
    GT = 10,
    GTEq = 11,
    And = 12,
    Or = 13,
    Xor = 14,
    Neg = 15,
    Not = 16,
    Alloc = 17,
    SetLoc = 18,
    GetLoc = 19,
    SetProp = 20,
    GetProp = 21,
    Jump = 22,
    JumpIfTrue = 23,
    JumpIfFalse = 24,
    Push = 25,
    Call = 26,
}

impl Default for OpCode {
    fn default() -> Self {
        OpCode::Const
    }
}

impl From<i32> for OpCode {
    fn from(i: i32) -> Self {
        match i {
            0 => OpCode::Const,
            1 => OpCode::Add,
            2 => OpCode::Sub,
            3 => OpCode::Div,
            4 => OpCode::Mul,
            5 => OpCode::Mod,
            6 => OpCode::Eq,
            7 => OpCode::NEq,
            8 => OpCode::LT,
            9 => OpCode::LTEq,
            10 => OpCode::GT,
            11 => OpCode::GTEq,
            12 => OpCode::And,
            13 => OpCode::Or,
            14 => OpCode::Xor,
            15 => OpCode::Neg,
            16 => OpCode::Not,
            17 => OpCode::Alloc,
            18 => OpCode::SetLoc,
            19 => OpCode::GetLoc,
            20 => OpCode::SetProp,
            21 => OpCode::GetProp,
            22 => OpCode::Jump,
            23 => OpCode::JumpIfTrue,
            24 => OpCode::JumpIfFalse,
            25 => OpCode::Push,
            26 => OpCode::Call,
            _ => Self::default(),
        }
    }
}

impl<'a> From<&'a str> for OpCode {
    fn from(s: &'a str) -> Self {
        match s {
            "Const" => OpCode::Const,
            "Add" => OpCode::Add,
            "Sub" => OpCode::Sub,
            "Div" => OpCode::Div,
            "Mul" => OpCode::Mul,
            "Mod" => OpCode::Mod,
            "Eq" => OpCode::Eq,
            "NEq" => OpCode::NEq,
            "LT" => OpCode::LT,
            "LTEq" => OpCode::LTEq,
            "GT" => OpCode::GT,
            "GTEq" => OpCode::GTEq,
            "And" => OpCode::And,
            "Or" => OpCode::Or,
            "Xor" => OpCode::Xor,
            "Neg" => OpCode::Neg,
            "Not" => OpCode::Not,
            "Alloc" => OpCode::Alloc,
            "SetLoc" => OpCode::SetLoc,
            "GetLoc" => OpCode::GetLoc,
            "SetProp" => OpCode::SetProp,
            "GetProp" => OpCode::GetProp,
            "Jump" => OpCode::Jump,
            "JumpIfTrue" => OpCode::JumpIfTrue,
            "JumpIfFalse" => OpCode::JumpIfFalse,
            "Push" => OpCode::Push,
            "Call" => OpCode::Call,
            _ => Self::default(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Variant {
    I8 = 1,
    I16 = 2,
    I32 = 3,
    I64 = 4,
    U8 = 5,
    U16 = 6,
    U32 = 7,
    U64 = 8,
    F32 = 9,
    F64 = 10,
    Bool = 11,
}

impl Default for Variant {
    fn default() -> Self {
        Variant::I8
    }
}

impl From<i32> for Variant {
    fn from(i: i32) -> Self {
        match i {
            1 => Variant::I8,
            2 => Variant::I16,
            3 => Variant::I32,
            4 => Variant::I64,
            5 => Variant::U8,
            6 => Variant::U16,
            7 => Variant::U32,
            8 => Variant::U64,
            9 => Variant::F32,
            10 => Variant::F64,
            11 => Variant::Bool,
            _ => Self::default(),
        }
    }
}

impl<'a> From<&'a str> for Variant {
    fn from(s: &'a str) -> Self {
        match s {
            "I8" => Variant::I8,
            "I16" => Variant::I16,
            "I32" => Variant::I32,
            "I64" => Variant::I64,
            "U8" => Variant::U8,
            "U16" => Variant::U16,
            "U32" => Variant::U32,
            "U64" => Variant::U64,
            "F32" => Variant::F32,
            "F64" => Variant::F64,
            "Bool" => Variant::Bool,
            _ => Self::default(),
        }
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Program<'a> {
    pub types: Vec<Type<'a>>,
    pub functions: Vec<Function<'a>>,
}

impl<'a> MessageRead<'a> for Program<'a> {
    fn from_reader(r: &mut BytesReader, bytes: &'a [u8]) -> Result<Self> {
        let mut msg = Self::default();
        while !r.is_eof() {
            match r.next_tag(bytes) {
                Ok(10) => msg.types.push(r.read_message::<Type>(bytes)?),
                Ok(18) => msg.functions.push(r.read_message::<Function>(bytes)?),
                Ok(t) => { r.read_unknown(bytes, t)?; }
                Err(e) => return Err(e),
            }
        }
        Ok(msg)
    }
}

impl<'a> MessageWrite for Program<'a> {
    fn get_size(&self) -> usize {
        0
        + self.types.iter().map(|s| 1 + sizeof_len((s).get_size())).sum::<usize>()
        + self.functions.iter().map(|s| 1 + sizeof_len((s).get_size())).sum::<usize>()
    }

    fn write_message<W: WriterBackend>(&self, w: &mut Writer<W>) -> Result<()> {
        for s in &self.types { w.write_with_tag(10, |w| w.write_message(s))?; }
        for s in &self.functions { w.write_with_tag(18, |w| w.write_message(s))?; }
        Ok(())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Type<'a> {
    pub kind: TypeKind,
    pub struct_pb: Option<Struct<'a>>,
    pub elem: Option<Box<Type<'a>>>,
}

impl<'a> MessageRead<'a> for Type<'a> {
    fn from_reader(r: &mut BytesReader, bytes: &'a [u8]) -> Result<Self> {
        let mut msg = Self::default();
        while !r.is_eof() {
            match r.next_tag(bytes) {
                Ok(8) => msg.kind = r.read_enum(bytes)?,
                Ok(18) => msg.struct_pb = Some(r.read_message::<Struct>(bytes)?),
                Ok(26) => msg.elem = Some(Box::new(r.read_message::<Type>(bytes)?)),
                Ok(t) => { r.read_unknown(bytes, t)?; }
                Err(e) => return Err(e),
            }
        }
        Ok(msg)
    }
}

impl<'a> MessageWrite for Type<'a> {
    fn get_size(&self) -> usize {
        0
        + if self.kind == model::TypeKind::I8 { 0 } else { 1 + sizeof_varint(*(&self.kind) as u64) }
        + self.struct_pb.as_ref().map_or(0, |m| 1 + sizeof_len((m).get_size()))
        + self.elem.as_ref().map_or(0, |m| 1 + sizeof_len((m).get_size()))
    }

    fn write_message<W: WriterBackend>(&self, w: &mut Writer<W>) -> Result<()> {
        if self.kind != model::TypeKind::I8 { w.write_with_tag(8, |w| w.write_enum(*&self.kind as i32))?; }
        if let Some(ref s) = self.struct_pb { w.write_with_tag(18, |w| w.write_message(s))?; }
        if let Some(ref s) = self.elem { w.write_with_tag(26, |w| w.write_message(&**s))?; }
        Ok(())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Struct<'a> {
    pub name: Cow<'a, str>,
    pub fields: Vec<Field<'a>>,
}

impl<'a> MessageRead<'a> for Struct<'a> {
    fn from_reader(r: &mut BytesReader, bytes: &'a [u8]) -> Result<Self> {
        let mut msg = Self::default();
        while !r.is_eof() {
            match r.next_tag(bytes) {
                Ok(10) => msg.name = r.read_string(bytes).map(Cow::Borrowed)?,
                Ok(18) => msg.fields.push(r.read_message::<Field>(bytes)?),
                Ok(t) => { r.read_unknown(bytes, t)?; }
                Err(e) => return Err(e),
            }
        }
        Ok(msg)
    }
}

impl<'a> MessageWrite for Struct<'a> {
    fn get_size(&self) -> usize {
        0
        + if self.name == "" { 0 } else { 1 + sizeof_len((&self.name).len()) }
        + self.fields.iter().map(|s| 1 + sizeof_len((s).get_size())).sum::<usize>()
    }

    fn write_message<W: WriterBackend>(&self, w: &mut Writer<W>) -> Result<()> {
        if self.name != "" { w.write_with_tag(10, |w| w.write_string(&**&self.name))?; }
        for s in &self.fields { w.write_with_tag(18, |w| w.write_message(s))?; }
        Ok(())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Field<'a> {
    pub index: u64,
    pub offset: u64,
    pub name: Cow<'a, str>,
    pub type_pb: Option<Type<'a>>,
}

impl<'a> MessageRead<'a> for Field<'a> {
    fn from_reader(r: &mut BytesReader, bytes: &'a [u8]) -> Result<Self> {
        let mut msg = Self::default();
        while !r.is_eof() {
            match r.next_tag(bytes) {
                Ok(8) => msg.index = r.read_uint64(bytes)?,
                Ok(16) => msg.offset = r.read_uint64(bytes)?,
                Ok(26) => msg.name = r.read_string(bytes).map(Cow::Borrowed)?,
                Ok(34) => msg.type_pb = Some(r.read_message::<Type>(bytes)?),
                Ok(t) => { r.read_unknown(bytes, t)?; }
                Err(e) => return Err(e),
            }
        }
        Ok(msg)
    }
}

impl<'a> MessageWrite for Field<'a> {
    fn get_size(&self) -> usize {
        0
        + if self.index == 0u64 { 0 } else { 1 + sizeof_varint(*(&self.index) as u64) }
        + if self.offset == 0u64 { 0 } else { 1 + sizeof_varint(*(&self.offset) as u64) }
        + if self.name == "" { 0 } else { 1 + sizeof_len((&self.name).len()) }
        + self.type_pb.as_ref().map_or(0, |m| 1 + sizeof_len((m).get_size()))
    }

    fn write_message<W: WriterBackend>(&self, w: &mut Writer<W>) -> Result<()> {
        if self.index != 0u64 { w.write_with_tag(8, |w| w.write_uint64(*&self.index))?; }
        if self.offset != 0u64 { w.write_with_tag(16, |w| w.write_uint64(*&self.offset))?; }
        if self.name != "" { w.write_with_tag(26, |w| w.write_string(&**&self.name))?; }
        if let Some(ref s) = self.type_pb { w.write_with_tag(34, |w| w.write_message(s))?; }
        Ok(())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Value<'a> {
    pub type_pb: Option<Type<'a>>,
    pub int_val: i64,
    pub uint_val: u64,
    pub bool_val: bool,
    pub f32_val: u32,
    pub f64_val: u64,
    pub struct_val: Option<StructValue<'a>>,
    pub elem: Option<Box<Value<'a>>>,
}

impl<'a> MessageRead<'a> for Value<'a> {
    fn from_reader(r: &mut BytesReader, bytes: &'a [u8]) -> Result<Self> {
        let mut msg = Self::default();
        while !r.is_eof() {
            match r.next_tag(bytes) {
                Ok(10) => msg.type_pb = Some(r.read_message::<Type>(bytes)?),
                Ok(16) => msg.int_val = r.read_int64(bytes)?,
                Ok(24) => msg.uint_val = r.read_uint64(bytes)?,
                Ok(32) => msg.bool_val = r.read_bool(bytes)?,
                Ok(45) => msg.f32_val = r.read_fixed32(bytes)?,
                Ok(49) => msg.f64_val = r.read_fixed64(bytes)?,
                Ok(58) => msg.struct_val = Some(r.read_message::<StructValue>(bytes)?),
                Ok(66) => msg.elem = Some(Box::new(r.read_message::<Value>(bytes)?)),
                Ok(t) => { r.read_unknown(bytes, t)?; }
                Err(e) => return Err(e),
            }
        }
        Ok(msg)
    }
}

impl<'a> MessageWrite for Value<'a> {
    fn get_size(&self) -> usize {
        0
        + self.type_pb.as_ref().map_or(0, |m| 1 + sizeof_len((m).get_size()))
        + if self.int_val == 0i64 { 0 } else { 1 + sizeof_varint(*(&self.int_val) as u64) }
        + if self.uint_val == 0u64 { 0 } else { 1 + sizeof_varint(*(&self.uint_val) as u64) }
        + if self.bool_val == false { 0 } else { 1 + sizeof_varint(*(&self.bool_val) as u64) }
        + if self.f32_val == 0u32 { 0 } else { 1 + 4 }
        + if self.f64_val == 0u64 { 0 } else { 1 + 8 }
        + self.struct_val.as_ref().map_or(0, |m| 1 + sizeof_len((m).get_size()))
        + self.elem.as_ref().map_or(0, |m| 1 + sizeof_len((m).get_size()))
    }

    fn write_message<W: WriterBackend>(&self, w: &mut Writer<W>) -> Result<()> {
        if let Some(ref s) = self.type_pb { w.write_with_tag(10, |w| w.write_message(s))?; }
        if self.int_val != 0i64 { w.write_with_tag(16, |w| w.write_int64(*&self.int_val))?; }
        if self.uint_val != 0u64 { w.write_with_tag(24, |w| w.write_uint64(*&self.uint_val))?; }
        if self.bool_val != false { w.write_with_tag(32, |w| w.write_bool(*&self.bool_val))?; }
        if self.f32_val != 0u32 { w.write_with_tag(45, |w| w.write_fixed32(*&self.f32_val))?; }
        if self.f64_val != 0u64 { w.write_with_tag(49, |w| w.write_fixed64(*&self.f64_val))?; }
        if let Some(ref s) = self.struct_val { w.write_with_tag(58, |w| w.write_message(s))?; }
        if let Some(ref s) = self.elem { w.write_with_tag(66, |w| w.write_message(&**s))?; }
        Ok(())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct StructValue<'a> {
    pub type_pb: Option<Type<'a>>,
    pub values: Vec<Value<'a>>,
}

impl<'a> MessageRead<'a> for StructValue<'a> {
    fn from_reader(r: &mut BytesReader, bytes: &'a [u8]) -> Result<Self> {
        let mut msg = Self::default();
        while !r.is_eof() {
            match r.next_tag(bytes) {
                Ok(10) => msg.type_pb = Some(r.read_message::<Type>(bytes)?),
                Ok(18) => msg.values.push(r.read_message::<Value>(bytes)?),
                Ok(t) => { r.read_unknown(bytes, t)?; }
                Err(e) => return Err(e),
            }
        }
        Ok(msg)
    }
}

impl<'a> MessageWrite for StructValue<'a> {
    fn get_size(&self) -> usize {
        0
        + self.type_pb.as_ref().map_or(0, |m| 1 + sizeof_len((m).get_size()))
        + self.values.iter().map(|s| 1 + sizeof_len((s).get_size())).sum::<usize>()
    }

    fn write_message<W: WriterBackend>(&self, w: &mut Writer<W>) -> Result<()> {
        if let Some(ref s) = self.type_pb { w.write_with_tag(10, |w| w.write_message(s))?; }
        for s in &self.values { w.write_with_tag(18, |w| w.write_message(s))?; }
        Ok(())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Function<'a> {
    pub name: Cow<'a, str>,
    pub instruction: Vec<Instruction<'a>>,
}

impl<'a> MessageRead<'a> for Function<'a> {
    fn from_reader(r: &mut BytesReader, bytes: &'a [u8]) -> Result<Self> {
        let mut msg = Self::default();
        while !r.is_eof() {
            match r.next_tag(bytes) {
                Ok(10) => msg.name = r.read_string(bytes).map(Cow::Borrowed)?,
                Ok(18) => msg.instruction.push(r.read_message::<Instruction>(bytes)?),
                Ok(t) => { r.read_unknown(bytes, t)?; }
                Err(e) => return Err(e),
            }
        }
        Ok(msg)
    }
}

impl<'a> MessageWrite for Function<'a> {
    fn get_size(&self) -> usize {
        0
        + if self.name == "" { 0 } else { 1 + sizeof_len((&self.name).len()) }
        + self.instruction.iter().map(|s| 1 + sizeof_len((s).get_size())).sum::<usize>()
    }

    fn write_message<W: WriterBackend>(&self, w: &mut Writer<W>) -> Result<()> {
        if self.name != "" { w.write_with_tag(10, |w| w.write_string(&**&self.name))?; }
        for s in &self.instruction { w.write_with_tag(18, |w| w.write_message(s))?; }
        Ok(())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Instruction<'a> {
    pub op_code: OpCode,
    pub Variant: Variant,
    pub index: u64,
    pub offset: i64,
    pub value: Option<Value<'a>>,
}

impl<'a> MessageRead<'a> for Instruction<'a> {
    fn from_reader(r: &mut BytesReader, bytes: &'a [u8]) -> Result<Self> {
        let mut msg = Self::default();
        while !r.is_eof() {
            match r.next_tag(bytes) {
                Ok(8) => msg.op_code = r.read_enum(bytes)?,
                Ok(16) => msg.Variant = r.read_enum(bytes)?,
                Ok(24) => msg.index = r.read_uint64(bytes)?,
                Ok(32) => msg.offset = r.read_int64(bytes)?,
                Ok(42) => msg.value = Some(r.read_message::<Value>(bytes)?),
                Ok(t) => { r.read_unknown(bytes, t)?; }
                Err(e) => return Err(e),
            }
        }
        Ok(msg)
    }
}

impl<'a> MessageWrite for Instruction<'a> {
    fn get_size(&self) -> usize {
        0
        + if self.op_code == model::OpCode::Const { 0 } else { 1 + sizeof_varint(*(&self.op_code) as u64) }
        + if self.Variant == model::Variant::I8 { 0 } else { 1 + sizeof_varint(*(&self.Variant) as u64) }
        + if self.index == 0u64 { 0 } else { 1 + sizeof_varint(*(&self.index) as u64) }
        + if self.offset == 0i64 { 0 } else { 1 + sizeof_varint(*(&self.offset) as u64) }
        + self.value.as_ref().map_or(0, |m| 1 + sizeof_len((m).get_size()))
    }

    fn write_message<W: WriterBackend>(&self, w: &mut Writer<W>) -> Result<()> {
        if self.op_code != model::OpCode::Const { w.write_with_tag(8, |w| w.write_enum(*&self.op_code as i32))?; }
        if self.Variant != model::Variant::I8 { w.write_with_tag(16, |w| w.write_enum(*&self.Variant as i32))?; }
        if self.index != 0u64 { w.write_with_tag(24, |w| w.write_uint64(*&self.index))?; }
        if self.offset != 0i64 { w.write_with_tag(32, |w| w.write_int64(*&self.offset))?; }
        if let Some(ref s) = self.value { w.write_with_tag(42, |w| w.write_message(s))?; }
        Ok(())
    }
}

