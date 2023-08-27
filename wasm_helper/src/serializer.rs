use crate::structure::*;

pub trait Serializer {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized;
}

trait WriterExt: std::io::Write {
    fn byte(&mut self, opcode: u8) -> std::io::Result<()> {
        self.write_all(&[opcode])
    }
}

impl<T: std::io::Write + ?Sized> WriterExt for &mut T {}

static MAGIC_NUMBER: &[u8] = &[0x00, 0x61, 0x73, 0x6d];
static VERSION: &[u8] = &[0x01, 0x00, 0x00, 0x00];

impl Serializer for Module {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        writer.write_all(MAGIC_NUMBER)?;
        writer.write_all(VERSION)?;

        self.types.as_section(0x01).serialize(writer)?;
        self.imports.as_section(0x02).serialize(writer)?;
        self.funcs
            .iter()
            .map(|func| func.ty)
            .collect::<Vec<_>>()
            .as_section(0x03)
            .serialize(writer)?;
        self.tables.as_section(0x04).serialize(writer)?;
        self.mems.as_section(0x05).serialize(writer)?;
        self.globals.as_section(0x06).serialize(writer)?;
        self.exports.as_section(0x07).serialize(writer)?;
        self.start
            .as_ref()
            .map(|start| Section::new(0x08, start))
            .serialize(writer)?;
        self.elems.as_section(0x09).serialize(writer)?;
        let data_len = self.datas.len() as u32;
        Section {
            id: 0x0c,
            content: &data_len,
        }
        .serialize(writer)?;
        self.funcs.as_section(0x0a).serialize(writer)?;
        self.datas.as_section(0x0b).serialize(writer)?;

        let mut names = Vec::default();
        for import in &self.imports {
            if let ImportDesc::Func(..) = import.desc {
                // TODO: technically wrong, we need to use the
                // user-defined function name, not the imported name.
                names.push((names.len(), format!("{}.{}", import.module, import.name)));
            }
        }
        for func in &self.funcs {
            names.push((names.len(), func.name.clone()));
        }
        NameSection {
            functions: names
                .iter()
                .map(|(idx, name)| (*idx as u32, name.as_str()))
                .collect(),
        }
        .serialize(writer)?;

        Ok(())
    }
}

macro_rules! leb128_unsigned_serializer {
    ($name:ident) => {
        impl Serializer for $name {
            fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
            where
                W: std::io::Write + ?Sized,
            {
                let mut val = *self;
                loop {
                    let b = (val & 0b1111111) as u8;
                    val >>= 7;

                    if val > 0 {
                        writer.write_all(&[b | 0b10000000u8])?;
                    } else {
                        writer.write_all(&[b])?;
                        break;
                    }
                }

                Ok(())
            }
        }
    };
}

leb128_unsigned_serializer!(u8);
leb128_unsigned_serializer!(u16);
leb128_unsigned_serializer!(u32);
leb128_unsigned_serializer!(u64);
leb128_unsigned_serializer!(usize);

macro_rules! leb128_signed_serializer {
    ($name:ident) => {
        impl Serializer for $name {
            fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
            where
                W: std::io::Write + ?Sized,
            {
                let mut val = *self;
                loop {
                    let mut b = val as u8;
                    val >>= 6;

                    let done = val == 0 || val == -1;
                    if done {
                        b &= 0b1111111;
                    } else {
                        val >>= 1;
                        b |= 0b10000000;
                    }

                    writer.write_all(&[b])?;
                    if done {
                        break;
                    }
                }

                Ok(())
            }
        }
    };
}

leb128_signed_serializer!(i8);
leb128_signed_serializer!(i16);
leb128_signed_serializer!(i32);
leb128_signed_serializer!(i64);
leb128_signed_serializer!(isize);

impl Serializer for f32 {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        writer.write_all(&self.to_le_bytes())
    }
}

impl Serializer for f64 {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        writer.write_all(&self.to_le_bytes())
    }
}

impl<T> Serializer for Vec<T>
where
    T: Serializer,
{
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        self.len().serialize(writer)?;
        for item in self.iter() {
            item.serialize(writer)?;
        }
        Ok(())
    }
}

struct Section<'a, T> {
    id: u8,
    content: &'a T,
}

trait AsSection: Sized {
    fn as_section(&self, id: u8) -> Option<Section<Self>>;
}

impl<T> AsSection for Vec<T> {
    fn as_section(&self, id: u8) -> Option<Section<Self>> {
        if self.is_empty() {
            None
        } else {
            Some(Section { id, content: self })
        }
    }
}

impl<'a, T> Section<'a, T> {
    fn new(id: u8, content: &'a T) -> Self {
        Self { id, content }
    }
}

impl<T> Serializer for Option<T>
where
    T: Serializer,
{
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        if let Some(ref val) = &self {
            val.serialize(writer)
        } else {
            Ok(())
        }
    }
}

impl<'a, T> Serializer for Section<'a, T>
where
    T: Serializer,
{
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        let mut buff = Vec::<u8>::default();
        self.content.serialize(&mut buff)?;
        if buff.is_empty() {
            return Ok(());
        }

        writer.write_all(&[self.id])?;
        buff.len().serialize(writer)?;
        writer.write_all(&buff)?;
        Ok(())
    }
}

impl Serializer for FuncType {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        writer.write_all(&[0x60])?;
        self.parameters.serialize(writer)?;
        self.returns.serialize(writer)?;
        Ok(())
    }
}

impl Serializer for ValType {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        match self {
            Self::Num(num_ty) => num_ty.serialize(writer),
            Self::Ref(ref_ty) => ref_ty.serialize(writer),
        }
    }
}

impl Serializer for NumType {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        match self {
            Self::I32 => writer.write_all(&[0x7f]),
            Self::I64 => writer.write_all(&[0x7e]),
            Self::F32 => writer.write_all(&[0x7d]),
            Self::F64 => writer.write_all(&[0x7c]),
        }
    }
}

impl Serializer for RefType {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        match self {
            Self::FuncRef => writer.write_all(&[0x70]),
            Self::ExternRef => writer.write_all(&[0x6f]),
        }
    }
}

impl Serializer for Import {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        self.module.serialize(writer)?;
        self.name.serialize(writer)?;
        self.desc.serialize(writer)
    }
}

impl Serializer for ImportDesc {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        match self {
            Self::Func(idx) => {
                writer.write_all(&[0x00])?;
                idx.serialize(writer)
            }
            Self::Table(table_type) => {
                writer.write_all(&[0x01])?;
                table_type.serialize(writer)
            }
            Self::Mem(mem_type) => {
                writer.write_all(&[0x02])?;
                mem_type.serialize(writer)
            }
            Self::Global(global_type) => {
                writer.write_all(&[0x03])?;
                global_type.serialize(writer)
            }
        }
    }
}

impl Serializer for String {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        self.len().serialize(writer)?;
        writer.write_all(self.as_bytes())?;
        Ok(())
    }
}

impl Serializer for &str {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        self.len().serialize(writer)?;
        writer.write_all(self.as_bytes())?;
        Ok(())
    }
}

impl Serializer for TableType {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        self.ref_type.serialize(writer)?;
        self.limits.serialize(writer)
    }
}

impl Serializer for Limits {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        if let Some(ref max) = self.max {
            writer.write_all(&[0x01])?;
            self.min.serialize(writer)?;
            max.serialize(writer)
        } else {
            writer.write_all(&[0x00])?;
            self.min.serialize(writer)
        }
    }
}

impl Serializer for Global {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        self.ty.serialize(writer)?;
        self.init.serialize(writer)
    }
}

impl Serializer for GlobalType {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        self.ty.serialize(writer)?;
        match self.mutability {
            Mut::Const => writer.write_all(&[0x00]),
            Mut::Var => writer.write_all(&[0x01]),
        }
    }
}

impl Serializer for Export {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        self.name.serialize(writer)?;
        self.desc.serialize(writer)?;
        Ok(())
    }
}

impl Serializer for ExportDesc {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        match self {
            Self::Func(val) => {
                writer.write_all(&[0x00])?;
                val.serialize(writer)
            }
            Self::Table(val) => {
                writer.write_all(&[0x01])?;
                val.serialize(writer)
            }
            Self::Mem(val) => {
                writer.write_all(&[0x02])?;
                val.serialize(writer)
            }
            Self::Global(val) => {
                writer.write_all(&[0x03])?;
                val.serialize(writer)
            }
        }
    }
}

impl Serializer for Elem {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        let func_ids: Vec<FuncIdx> = self
            .init
            .iter()
            .filter_map(|expr| {
                if let &[Instr::RefFunc(func_id)] = &expr.0[..] {
                    Some(func_id)
                } else {
                    None
                }
            })
            .collect();

        let func_ids = if func_ids.len() == self.init.len() {
            Some(func_ids)
        } else {
            None
        };

        match (&self.ty, func_ids, &self.mode) {
            (RefType::FuncRef, Some(func_ids), ElemMode::Active { table: 0, offset }) => {
                writer.write_all(&[0x00])?;
                offset.serialize(writer)?;
                func_ids.serialize(writer)
            }
            (RefType::ExternRef, Some(func_ids), ElemMode::Passive) => {
                writer.write_all(&[0x01, 0x00])?;
                func_ids.serialize(writer)
            }
            (RefType::ExternRef, Some(func_ids), ElemMode::Active { table, offset }) => {
                writer.write_all(&[0x02])?;
                table.serialize(writer)?;
                offset.serialize(writer)?;
                writer.write_all(&[0x00])?;
                func_ids.serialize(writer)
            }
            (RefType::ExternRef, Some(func_ids), ElemMode::Declarative) => {
                writer.write_all(&[0x03, 0x00])?;
                func_ids.serialize(writer)
            }
            (RefType::FuncRef, None, ElemMode::Active { table: 0, offset }) => {
                writer.write_all(&[0x04])?;
                offset.serialize(writer)?;
                self.init.serialize(writer)
            }
            (RefType::ExternRef, None, ElemMode::Passive) => {
                writer.write_all(&[0x05])?;
                self.ty.serialize(writer)?;
                self.init.serialize(writer)
            }
            (RefType::ExternRef, None, ElemMode::Active { table, offset }) => {
                writer.write_all(&[0x06])?;
                table.serialize(writer)?;
                offset.serialize(writer)?;
                self.ty.serialize(writer)?;
                self.init.serialize(writer)
            }
            (RefType::ExternRef, None, ElemMode::Declarative) => {
                writer.write_all(&[0x07])?;
                self.ty.serialize(writer)?;
                self.init.serialize(writer)
            }
            _ => unreachable!("invalid element ir"),
        }
    }
}

impl Serializer for Func {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        let mut buff = Vec::<u8>::default();

        let mut local_groups = Vec::default();
        let mut last_group = ValTypeGroup {
            count: 0,
            ty: ValType::Num(NumType::I32),
        };
        for val_ty in &self.locals {
            if val_ty.ty == last_group.ty {
                last_group.count += 1;
            } else {
                if last_group.count > 0 {
                    local_groups.push(last_group);
                }
                last_group.count = 1;
                last_group.ty = val_ty.ty;
            }
        }
        if last_group.count > 0 {
            local_groups.push(last_group);
        }
        local_groups.serialize(&mut buff)?;
        self.body.serialize(&mut buff)?;

        buff.len().serialize(writer)?;
        writer.write_all(&buff)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
struct ValTypeGroup {
    count: u32,
    ty: ValType,
}

impl Serializer for ValTypeGroup {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        if self.count > 0 {
            self.count.serialize(writer)?;
            self.ty.serialize(writer)
        } else {
            Ok(())
        }
    }
}

impl Serializer for Expr {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        for instr in self.0.iter() {
            instr.serialize(writer)?;
        }
        writer.write_all(&[0x0b])
    }
}

impl Serializer for Instr {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        match self {
            Self::Unreachable => writer.write_all(&[0x00]),
            Self::Nop => writer.write_all(&[0x01]),
            Self::Block(block_ty, instrs) => {
                writer.write_all(&[0x02])?;
                block_ty.serialize(writer)?;
                for instr in instrs.iter() {
                    instr.serialize(writer)?;
                }
                writer.write_all(&[0x0b])
            }
            Self::Loop(block_ty, instrs) => {
                writer.write_all(&[0x03])?;
                block_ty.serialize(writer)?;
                for instr in instrs.iter() {
                    instr.serialize(writer)?;
                }
                writer.write_all(&[0x0b])
            }
            Self::If(block_ty, body, else_body) => {
                writer.write_all(&[0x04])?;
                block_ty.serialize(writer)?;
                for instr in body.iter() {
                    instr.serialize(writer)?;
                }
                if !else_body.is_empty() {
                    writer.write_all(&[0x05])?;
                    for instr in else_body.iter() {
                        instr.serialize(writer)?;
                    }
                }
                writer.write_all(&[0x0b])
            }
            Self::Br(label_id) => {
                writer.write_all(&[0x0c])?;
                label_id.serialize(writer)
            }
            Self::BrIf(label_id) => {
                writer.write_all(&[0x0d])?;
                label_id.serialize(writer)
            }
            Self::BrTable(label_ids, label_id) => {
                writer.write_all(&[0x0e])?;
                label_ids.serialize(writer)?;
                label_id.serialize(writer)
            }
            Self::Return => writer.write_all(&[0x0f]),
            Self::Call(func_idx) => {
                writer.write_all(&[0x10])?;
                func_idx.serialize(writer)
            }
            Self::CallIndirect(table_id, type_id) => {
                writer.write_all(&[0x11])?;
                type_id.serialize(writer)?;
                table_id.serialize(writer)
            }
            Self::RefNull(ref_type) => {
                writer.write_all(&[0xd0])?;
                ref_type.serialize(writer)
            }
            Self::RefIsNull => writer.write_all(&[0xd1]),
            Self::RefFunc(func_id) => {
                writer.write_all(&[0xd2])?;
                func_id.serialize(writer)
            }
            Self::Drop => writer.write_all(&[0x1a]),
            Self::LocalGet(local_id) => {
                writer.write_all(&[0x20])?;
                local_id.serialize(writer)
            }
            Self::LocalSet(local_id) => {
                writer.write_all(&[0x21])?;
                local_id.serialize(writer)
            }
            Self::LocalTee(local_id) => {
                writer.write_all(&[0x22])?;
                local_id.serialize(writer)
            }
            Self::GlobalGet(global_id) => {
                writer.write_all(&[0x23])?;
                global_id.serialize(writer)
            }
            Self::GlobalSet(global_id) => {
                writer.write_all(&[0x24])?;
                global_id.serialize(writer)
            }
            Self::TableGet(table_id) => {
                writer.write_all(&[0x25])?;
                table_id.serialize(writer)
            }
            Self::TableSet(table_id) => {
                writer.write_all(&[0x26])?;
                table_id.serialize(writer)
            }
            Self::TableInit(table_id, elem_id) => {
                writer.write_all(&[0xfc])?;
                12.serialize(writer)?;
                elem_id.serialize(writer)?;
                table_id.serialize(writer)
            }
            Self::ElemDrop(elem_id) => {
                writer.write_all(&[0xfc])?;
                13.serialize(writer)?;
                elem_id.serialize(writer)
            }
            Self::TableCopy(dest_table_id, src_table_id) => {
                writer.write_all(&[0xfc])?;
                14.serialize(writer)?;
                dest_table_id.serialize(writer)?;
                src_table_id.serialize(writer)
            }
            Self::TableGrow(table_id) => {
                writer.write_all(&[0xfc])?;
                15.serialize(writer)?;
                table_id.serialize(writer)
            }
            Self::TableSize(table_id) => {
                writer.write_all(&[0xfc])?;
                16.serialize(writer)?;
                table_id.serialize(writer)
            }
            Self::TableFill(table_id) => {
                writer.write_all(&[0xfc])?;
                17.serialize(writer)?;
                table_id.serialize(writer)
            }
            Self::I32Load(memarg) => {
                writer.write_all(&[0x28])?;
                memarg.serialize(writer)
            }
            Self::I64Load(memarg) => {
                writer.write_all(&[0x29])?;
                memarg.serialize(writer)
            }
            Self::F32Load(memarg) => {
                writer.write_all(&[0x2a])?;
                memarg.serialize(writer)
            }
            Self::F64Load(memarg) => {
                writer.write_all(&[0x2b])?;
                memarg.serialize(writer)
            }
            Self::I32Load8S(memarg) => {
                writer.write_all(&[0x2c])?;
                memarg.serialize(writer)
            }
            Self::I32Load8U(memarg) => {
                writer.write_all(&[0x2d])?;
                memarg.serialize(writer)
            }
            Self::I32Load16S(memarg) => {
                writer.write_all(&[0x2e])?;
                memarg.serialize(writer)
            }
            Self::I32Load16U(memarg) => {
                writer.write_all(&[0x2f])?;
                memarg.serialize(writer)
            }
            Self::I64Load8S(memarg) => {
                writer.write_all(&[0x30])?;
                memarg.serialize(writer)
            }
            Self::I64Load8U(memarg) => {
                writer.write_all(&[0x31])?;
                memarg.serialize(writer)
            }
            Self::I64Load16S(memarg) => {
                writer.write_all(&[0x32])?;
                memarg.serialize(writer)
            }
            Self::I64Load16U(memarg) => {
                writer.write_all(&[0x33])?;
                memarg.serialize(writer)
            }
            Self::I64Load32S(memarg) => {
                writer.write_all(&[0x34])?;
                memarg.serialize(writer)
            }
            Self::I64Load32U(memarg) => {
                writer.write_all(&[0x35])?;
                memarg.serialize(writer)
            }
            Self::I32Store(memarg) => {
                writer.write_all(&[0x36])?;
                memarg.serialize(writer)
            }
            Self::I64Store(memarg) => {
                writer.write_all(&[0x37])?;
                memarg.serialize(writer)
            }
            Self::F32Store(memarg) => {
                writer.write_all(&[0x38])?;
                memarg.serialize(writer)
            }
            Self::F64Store(memarg) => {
                writer.write_all(&[0x39])?;
                memarg.serialize(writer)
            }
            Self::I32Store8(memarg) => {
                writer.write_all(&[0x3a])?;
                memarg.serialize(writer)
            }
            Self::I32Store16(memarg) => {
                writer.write_all(&[0x3b])?;
                memarg.serialize(writer)
            }
            Self::I64Store8(memarg) => {
                writer.write_all(&[0x3c])?;
                memarg.serialize(writer)
            }
            Self::I64Store16(memarg) => {
                writer.write_all(&[0x3d])?;
                memarg.serialize(writer)
            }
            Self::I64Store32(memarg) => {
                writer.write_all(&[0x3e])?;
                memarg.serialize(writer)
            }
            Self::MemorySize => writer.write_all(&[0x3f, 0x00]),
            Self::MemoryGrow => writer.write_all(&[0x40, 0x00]),
            Self::MemoryInit(data_idx) => {
                writer.write_all(&[0xfc, 0x08])?;
                data_idx.serialize(writer)?;
                writer.write_all(&[0x00])
            }
            Self::DataDrop(data_idx) => {
                writer.write_all(&[0xfc, 0x09])?;
                data_idx.serialize(writer)
            }
            Self::MemoryCopy => writer.write_all(&[0xfc, 0x0a, 0x00, 0x00]),
            Self::MemoryFill => writer.write_all(&[0xfc, 0x0b, 0x00]),
            Self::I32Const(val) => {
                writer.write_all(&[0x41])?;
                val.serialize(writer)
            }
            Self::I64Const(val) => {
                writer.write_all(&[0x42])?;
                val.serialize(writer)
            }
            Self::F32Const(val) => {
                writer.write_all(&[0x43])?;
                val.serialize(writer)
            }
            Self::F64Const(val) => {
                writer.write_all(&[0x44])?;
                val.serialize(writer)
            }
            Self::I32Eqz => writer.write_all(&[0x45]),
            Self::I32Eq => writer.write_all(&[0x46]),
            Self::I32Ne => writer.write_all(&[0x47]),
            Self::I32LtS => writer.write_all(&[0x48]),
            Self::I32LtU => writer.write_all(&[0x49]),
            Self::I32GtS => writer.write_all(&[0x4a]),
            Self::I32GtU => writer.write_all(&[0x4b]),
            Self::I32LeS => writer.write_all(&[0x4c]),
            Self::I32LeU => writer.write_all(&[0x4d]),
            Self::I32GeS => writer.write_all(&[0x4e]),
            Self::I32GeU => writer.write_all(&[0x4f]),
            Self::I64Eqz => writer.write_all(&[0x50]),
            Self::I64Eq => writer.write_all(&[0x51]),
            Self::I64Ne => writer.write_all(&[0x52]),
            Self::I64LtS => writer.write_all(&[0x53]),
            Self::I64LtU => writer.write_all(&[0x54]),
            Self::I64GtS => writer.write_all(&[0x55]),
            Self::I64GtU => writer.write_all(&[0x56]),
            Self::I64LeS => writer.write_all(&[0x57]),
            Self::I64LeU => writer.write_all(&[0x58]),
            Self::I64GeS => writer.write_all(&[0x59]),
            Self::I64GeU => writer.write_all(&[0x5a]),
            Self::F32Eq => writer.write_all(&[0x5b]),
            Self::F32Ne => writer.write_all(&[0x5c]),
            Self::F32Lt => writer.write_all(&[0x5d]),
            Self::F32Gt => writer.write_all(&[0x5e]),
            Self::F32Le => writer.write_all(&[0x5f]),
            Self::F32Ge => writer.write_all(&[0x60]),
            Self::F64Eq => writer.write_all(&[0x61]),
            Self::F64Ne => writer.write_all(&[0x62]),
            Self::F64Lt => writer.write_all(&[0x63]),
            Self::F64Gt => writer.write_all(&[0x64]),
            Self::F64Le => writer.write_all(&[0x65]),
            Self::F64Ge => writer.write_all(&[0x66]),
            Self::I32Clz => writer.write_all(&[0x67]),
            Self::I32Ctz => writer.write_all(&[0x68]),
            Self::I32Popcnt => writer.write_all(&[0x69]),
            Self::I32Add => writer.write_all(&[0x6a]),
            Self::I32Sub => writer.write_all(&[0x6b]),
            Self::I32Mul => writer.write_all(&[0x6c]),
            Self::I32DivS => writer.write_all(&[0x6d]),
            Self::I32DivU => writer.write_all(&[0x6e]),
            Self::I32RemS => writer.write_all(&[0x6f]),
            Self::I32RemU => writer.write_all(&[0x70]),
            Self::I32And => writer.write_all(&[0x71]),
            Self::I32Or => writer.write_all(&[0x72]),
            Self::I32Xor => writer.write_all(&[0x73]),
            Self::I32Shl => writer.write_all(&[0x74]),
            Self::I32ShrS => writer.write_all(&[0x75]),
            Self::I32ShrU => writer.write_all(&[0x76]),
            Self::I32Rotl => writer.write_all(&[0x77]),
            Self::I32Rotr => writer.write_all(&[0x78]),
            Self::I64Clz => writer.write_all(&[0x79]),
            Self::I64Ctz => writer.write_all(&[0x7a]),
            Self::I64Popcnt => writer.write_all(&[0x7b]),
            Self::I64Add => writer.write_all(&[0x7c]),
            Self::I64Sub => writer.write_all(&[0x7d]),
            Self::I64Mul => writer.write_all(&[0x7e]),
            Self::I64DivS => writer.write_all(&[0x7f]),
            Self::I64DivU => writer.write_all(&[0x80]),
            Self::I64RemS => writer.write_all(&[0x81]),
            Self::I64RemU => writer.write_all(&[0x82]),
            Self::I64And => writer.write_all(&[0x83]),
            Self::I64Or => writer.write_all(&[0x84]),
            Self::I64Xor => writer.write_all(&[0x85]),
            Self::I64Shl => writer.write_all(&[0x86]),
            Self::I64ShrS => writer.write_all(&[0x87]),
            Self::I64ShrU => writer.write_all(&[0x88]),
            Self::I64Rotl => writer.write_all(&[0x89]),
            Self::I64Rotr => writer.write_all(&[0x8a]),
            Self::F32Abs => writer.write_all(&[0x8b]),
            Self::F32Neg => writer.write_all(&[0x8c]),
            Self::F32Ceil => writer.write_all(&[0x8d]),
            Self::F32Floor => writer.write_all(&[0x8e]),
            Self::F32Trunc => writer.write_all(&[0x8f]),
            Self::F32Nearest => writer.write_all(&[0x90]),
            Self::F32Sqrt => writer.write_all(&[0x91]),
            Self::F32Add => writer.write_all(&[0x92]),
            Self::F32Sub => writer.write_all(&[0x93]),
            Self::F32Mul => writer.write_all(&[0x94]),
            Self::F32Div => writer.write_all(&[0x95]),
            Self::F32Min => writer.write_all(&[0x96]),
            Self::F32Max => writer.write_all(&[0x97]),
            Self::F32CopySign => writer.write_all(&[0x98]),
            Self::F64Abs => writer.write_all(&[0x99]),
            Self::F64Neg => writer.write_all(&[0x9a]),
            Self::F64Ceil => writer.write_all(&[0x9b]),
            Self::F64Floor => writer.write_all(&[0x9c]),
            Self::F64Trunc => writer.write_all(&[0x9d]),
            Self::F64Nearest => writer.write_all(&[0x9e]),
            Self::F64Sqrt => writer.write_all(&[0x9f]),
            Self::F64Add => writer.write_all(&[0xa0]),
            Self::F64Sub => writer.write_all(&[0xa1]),
            Self::F64Mul => writer.write_all(&[0xa2]),
            Self::F64Div => writer.write_all(&[0xa3]),
            Self::F64Min => writer.write_all(&[0xa4]),
            Self::F64Max => writer.write_all(&[0xa5]),
            Self::F64CopySign => writer.write_all(&[0xa6]),
            Self::I32WrapI64 => writer.write_all(&[0xa7]),
            Self::I32TruncF32S => writer.write_all(&[0xa8]),
            Self::I32TruncF32U => writer.write_all(&[0xa9]),
            Self::I32TruncF64S => writer.write_all(&[0xaa]),
            Self::I32TruncF64U => writer.write_all(&[0xab]),
            Self::I64ExtendI32S => writer.write_all(&[0xac]),
            Self::I64ExtendI32U => writer.write_all(&[0xad]),
            Self::I64TruncF32S => writer.write_all(&[0xae]),
            Self::I64TruncF32U => writer.write_all(&[0xaf]),
            Self::I64TruncF64S => writer.write_all(&[0xb0]),
            Self::I64TruncF64U => writer.write_all(&[0xb1]),
            Self::F32ConvertI32S => writer.write_all(&[0xb2]),
            Self::F32ConvertI32U => writer.write_all(&[0xb3]),
            Self::F32ConvertI64S => writer.write_all(&[0xb4]),
            Self::F32ConvertI64U => writer.write_all(&[0xb5]),
            Self::F32DemoteF64 => writer.write_all(&[0xb6]),
            Self::F64ConvertI32S => writer.write_all(&[0xb7]),
            Self::F64ConvertI32U => writer.write_all(&[0xb8]),
            Self::F64ConvertI64S => writer.write_all(&[0xb9]),
            Self::F64ConvertI64U => writer.write_all(&[0xba]),
            Self::F64PromoteF32 => writer.write_all(&[0xbb]),
            Self::I32ReinterpretF32 => writer.write_all(&[0xbc]),
            Self::I64ReinterpretF64 => writer.write_all(&[0xbd]),
            Self::F32ReinterpretI32 => writer.write_all(&[0xbe]),
            Self::F64ReinterpretI64 => writer.write_all(&[0xbf]),
            Self::I32Extend8S => writer.write_all(&[0xc0]),
            Self::I32Extend16S => writer.write_all(&[0xc1]),
            Self::I64Extend8S => writer.write_all(&[0xc2]),
            Self::I64Extend16S => writer.write_all(&[0xc3]),
            Self::I64Extend32S => writer.write_all(&[0xc4]),
            Self::I32TruncSatF32S => writer.write_all(&[0xfc, 0x00]),
            Self::I32TruncSatF32U => writer.write_all(&[0xfc, 0x01]),
            Self::I32TruncSatF64S => writer.write_all(&[0xfc, 0x02]),
            Self::I32TruncSatF64U => writer.write_all(&[0xfc, 0x03]),
            Self::I64TruncSatF32S => writer.write_all(&[0xfc, 0x04]),
            Self::I64TruncSatF32U => writer.write_all(&[0xfc, 0x05]),
            Self::I64TruncSatF64S => writer.write_all(&[0xfc, 0x06]),
            Self::I64TruncSatF64U => writer.write_all(&[0xfc, 0x07]),
        }
    }
}

impl Serializer for BlockType {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        match self {
            Self::None => writer.write_all(&[0x40]),
            Self::ValTy(val_type) => val_type.serialize(writer),
            Self::Ty(type_id) => type_id.serialize(writer),
        }
    }
}

impl Serializer for MemArg {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        self.align.serialize(writer)?;
        self.offset.serialize(writer)
    }
}

impl<T1, T2> Serializer for (T1, T2)
where
    T1: Serializer,
    T2: Serializer,
{
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        self.0.serialize(writer)?;
        self.1.serialize(writer)
    }
}

struct NameSection<'a> {
    pub functions: Vec<(FuncIdx, &'a str)>,
}

impl<'a> Serializer for NameSection<'a> {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        0u8.serialize(writer)?;

        let mut buff = Vec::<u8>::default();
        "name".serialize(&mut buff)?;

        if !self.functions.is_empty() {
            let mut subsec_buff = Vec::<u8>::default();
            self.functions.serialize(&mut subsec_buff)?;

            1u8.serialize(&mut buff)?;
            subsec_buff.len().serialize(&mut buff)?;
            buff.extend(subsec_buff);
        }

        buff.len().serialize(writer)?;
        writer.write_all(&buff)
    }
}

impl Serializer for Data {
    fn serialize<W>(&self, writer: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write + ?Sized,
    {
        match &self.mode {
            DataMode::Active { memory: 0, offset } => {
                writer.write_all(&[0x00])?;
                offset.serialize(writer)?;
                self.init.serialize(writer)
            }
            DataMode::Passive => {
                writer.write_all(&[0x01])?;
                self.init.serialize(writer)
            }
            DataMode::Active { memory, offset } => {
                writer.write_all(&[0x02])?;
                memory.serialize(writer)?;
                offset.serialize(writer)?;
                self.init.serialize(writer)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_serialize {
        ($name: ident, $module: expr, $expected: expr) => {
            #[test]
            fn $name() {
                let module = $module;
                let mut buff = Vec::<u8>::default();
                module.serialize(&mut buff).unwrap();

                let expectation: &[u8] = $expected;
                assert_eq!(buff.as_slice(), expectation);
            }
        };
    }

    test_serialize! {
        add_two,
        Module {
            types: vec![FuncType {
                parameters: vec![ValType::Num(NumType::I32), ValType::Num(NumType::I32)],
                returns: vec![ValType::Num(NumType::I32)],
            }],
            funcs: vec![Func {
                name: "".to_string(),
                ty: 0,
                locals: vec![],
                body: Expr(vec![Instr::LocalGet(0), Instr::LocalGet(1), Instr::I32Add]),
            }],
            tables: vec![],
            mems: vec![],
            globals: vec![],
            elems: vec![],
            datas: vec![],
            start: None,
            imports: vec![],
            exports: vec![Export {
                name: String::from("addTwo"),
                desc: ExportDesc::Func(0),
            }],
        },
        &[
            0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x07, 0x01, 0x60, 0x02, 0x7f,
            0x7f, 0x01, 0x7f, 0x03, 0x02, 0x01, 0x00, 0x07, 0x0a, 0x01, 0x06, 0x61, 0x64, 0x64,
            0x54, 0x77, 0x6f, 0x00, 0x00, 0x0c, 0x01, 0x00, 0x0a, 0x09, 0x01, 0x07, 0x00, 0x20,
            0x00, 0x20, 0x01, 0x6a, 0x0b, 0x00, 0x0a, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x01, 0x03,
            0x01, 0x00, 0x00
        ]
    }

    test_serialize! {
        factorial,
        Module {
            types: vec![FuncType {
                parameters: vec![ValType::Num(NumType::F64)],
                returns: vec![ValType::Num(NumType::F64)],
            }],
            funcs: vec![Func {
                name: "".to_string(),
                ty: 0,
                locals: vec![],
                body: Expr(vec![
                    Instr::LocalGet(0),
                    Instr::F64Const(1f64),
                    Instr::F64Lt,
                    Instr::If(
                        BlockType::ValTy(ValType::Num(NumType::F64)),
                        vec![Instr::F64Const(1f64)],
                        vec![
                            Instr::LocalGet(0),
                            Instr::LocalGet(0),
                            Instr::F64Const(1f64),
                            Instr::F64Sub,
                            Instr::Call(0),
                            Instr::F64Mul,
                        ],
                    ),
                ]),
            }],
            tables: vec![],
            mems: vec![],
            globals: vec![],
            elems: vec![],
            datas: vec![],
            start: None,
            imports: vec![],
            exports: vec![Export {
                name: String::from("fac"),
                desc: ExportDesc::Func(0),
            }],
        },
        &[
            0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x06, 0x01, 0x60, 0x01, 0x7c, 0x01, 0x7c,
            0x03, 0x02, 0x01, 0x00, 0x07, 0x07, 0x01, 0x03, 0x66, 0x61, 0x63, 0x00, 0x00, 0x0c, 0x01, 0x00,
            0x0a, 0x2e, 0x01, 0x2c, 0x00, 0x20, 0x00, 0x44, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f,
            0x63, 0x04, 0x7c, 0x44, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f, 0x05, 0x20, 0x00, 0x20,
            0x00, 0x44, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f, 0xa1, 0x10, 0x00, 0xa2, 0x0b, 0x0b,
            0x00, 0x0a, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x01, 0x03, 0x01, 0x00, 0x00,
        ]
    }

    test_serialize! {
        stuff,
        Module{
            types: vec![
                FuncType {
                    parameters: vec![ValType::Num(NumType::I32)],
                    returns: vec![ValType::Num(NumType::I32)],
                },
                FuncType {
                    parameters: vec![ValType::Num(NumType::F32)],
                    returns: vec![],
                },
                FuncType {
                    parameters: vec![],
                    returns: vec![],
                },
            ],
            funcs: vec![
                Func {
                    name: "".to_string(),
                    ty: 2,
                    locals: vec![],
                    body: Expr(vec![]),
                },
                Func {
                    name: "".to_string(),
                    ty: 1,
                    locals: vec![],
                    body: Expr(vec![Instr::I32Const(42), Instr::Drop]),
                },
            ],
            tables: vec![TableType {
                limits: Limits {
                    min: 0,
                    max: Some(1),
                },
                ref_type: RefType::FuncRef,
            }],
            mems: vec![Mem {
                min: 1,
                max: Some(1),
            }],
            globals: vec![],
            elems: vec![],
            datas: vec![Data {
                init: "hi".into(),
                mode: DataMode::Active {
                    memory: 0,
                    offset: Expr(vec![Instr::I32Const(0)]),
                },
            }],
            start: Some(1),
            imports: vec![Import {
                module: String::from("foo"),
                name: String::from("bar"),
                desc: ImportDesc::Func(1),
            }],
            exports: vec![Export {
                name: String::from("e"),
                desc: ExportDesc::Func(1),
            }],
        },
        &[
            0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x0d, 0x03, 0x60, 0x01, 0x7f, 0x01, 0x7f,
            0x60, 0x01, 0x7d, 0x00, 0x60, 0x00, 0x00, 0x02, 0x0b, 0x01, 0x03, 0x66, 0x6f, 0x6f, 0x03, 0x62,
            0x61, 0x72, 0x00, 0x01, 0x03, 0x03, 0x02, 0x02, 0x01, 0x04, 0x05, 0x01, 0x70, 0x01, 0x00, 0x01,
            0x05, 0x04, 0x01, 0x01, 0x01, 0x01, 0x07, 0x05, 0x01, 0x01, 0x65, 0x00, 0x01, 0x08, 0x01, 0x01,
            0x0c, 0x01, 0x01, 0x0a, 0x0a, 0x02, 0x02, 0x00, 0x0b, 0x05, 0x00, 0x41, 0x2a, 0x1a, 0x0b, 0x0b,
            0x08, 0x01, 0x00, 0x41, 0x00, 0x0b, 0x02, 0x68, 0x69, 0x00, 0x15, 0x04, 0x6e, 0x61, 0x6d, 0x65,
            0x01, 0x0e, 0x03, 0x00, 0x07, 0x66, 0x6f, 0x6f, 0x2e, 0x62, 0x61, 0x72, 0x01, 0x00, 0x02, 0x00,
        ]
    }

    test_serialize! {
        mutable_global,
        Module{
            types: vec![FuncType{parameters: vec![], returns: vec![]}],
            funcs: vec![Func{
                name: "".to_string(),
                ty: 0,
                locals: vec![],
                body: Expr(vec![
                    Instr::I32Const(100),
                    Instr::GlobalSet(0),
                ]),
            }],
            tables: vec![],
            mems: vec![],
            globals: vec![],
            elems: vec![],
            datas: vec![],
            start: None,
            imports: vec![Import{
                module: "env".to_string(),
                name: "g".to_string(),
                desc: ImportDesc::Global(GlobalType{mutability: Mut::Var, ty: ValType::Num(NumType::I32)}),
            }],
            exports: vec![Export{
                name: "f".to_string(),
                desc: ExportDesc::Func(0),
            }],
        },
        &[

            0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x60, 0x00, 0x00,
            0x02, 0x0a, 0x01, 0x03, 0x65, 0x6e, 0x76, 0x01, 0x67, 0x03, 0x7f, 0x01, 0x03, 0x02,
            0x01, 0x00, 0x07, 0x05, 0x01, 0x01, 0x66, 0x00, 0x00, 0x0c, 0x01, 0x00, 0x0a, 0x09,
            0x01, 0x07, 0x00, 0x41, 0xe4, 0x00, 0x24, 0x00, 0x0b, 0x00, 0x0a, 0x04, 0x6e, 0x61,
            0x6d, 0x65, 0x01, 0x03, 0x01, 0x00, 0x0,
        ]
    }

    test_serialize! {
        empty_func,
        Module{
            types: vec![FuncType{parameters: vec![], returns: vec![]}],
            funcs: vec![Func{
                name: "nothing".to_string(),
                ty: 0,
                locals: vec![],
                body: Expr(vec![]),
            }],
            tables: vec![],
            mems: vec![],
            globals: vec![],
            elems: vec![],
            datas: vec![],
            start: None,
            imports: vec![],
            exports: vec![],
        },
        &[
            0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x60, 0x00, 0x00, 0x03, 0x02,
            0x01, 0x00, 0x0c, 0x01, 0x00, 0x0a, 0x04, 0x01, 0x02, 0x00, 0x0b, 0x00, 0x11, 0x04, 0x6e, 0x61,
            0x6d, 0x65, 0x01, 0x0a, 0x01, 0x00, 0x07, 0x6e, 0x6f, 0x74, 0x68, 0x69, 0x6e, 0x67
        ]
    }

    test_serialize! {leb128_u8_0, 0u8, &[0x00]}
    test_serialize! {leb128_u8_1, 1u8, &[0x01]}
    test_serialize! {leb128_u8_127, 127u8, &[0x7f]}
    test_serialize! {leb128_u8_128, 128u8, &[0x80, 0x01]}
    test_serialize! {leb128_u8_255, 255u8, &[0xff, 0x01]}
    test_serialize! {leb128_u16_0, 0u16, &[0x00]}
    test_serialize! {leb128_u16_1, 1u16, &[0x01]}
    test_serialize! {leb128_u16_127, 127u16, &[0x7f]}
    test_serialize! {leb128_u16_128, 128u16, &[0x80, 0x01]}
    test_serialize! {leb128_u16_255, 255u16, &[0xff, 0x01]}
    test_serialize! {leb128_u16_65535, 65535u16, &[0xff, 0xff, 0x03]}
    test_serialize! {leb128_u32_0, 0u32, &[0x00]}
    test_serialize! {leb128_u32_1, 1u32, &[0x01]}
    test_serialize! {leb128_u32_127, 127u32, &[0x7f]}
    test_serialize! {leb128_u32_128, 128u32, &[0x80, 0x01]}
    test_serialize! {leb128_u32_255, 255u32, &[0xff, 0x01]}
    test_serialize! {leb128_u32_65535, 65535u32, &[0xff, 0xff, 0x03]}
    test_serialize! {leb128_u32_4294967295, 4294967295u32, &[0xff, 0xff, 0xff, 0xff, 0x0f]}
    test_serialize! {leb128_u64_0, 0u64, &[0x00]}
    test_serialize! {leb128_u64_1, 1u64, &[0x01]}
    test_serialize! {leb128_u64_127, 127u64, &[0x7f]}
    test_serialize! {leb128_u64_128, 128u64, &[0x80, 0x01]}
    test_serialize! {leb128_u64_255, 255u64, &[0xff, 0x01]}
    test_serialize! {leb128_u64_65535, 65535u64, &[0xff, 0xff, 0x03]}
    test_serialize! {leb128_u64_4294967295, 4294967295u64, &[0xff, 0xff, 0xff, 0xff, 0x0f]}
    test_serialize! {leb128_u64_18446744073709551615, 18446744073709551615u64, &[0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01]}

    test_serialize! {leb128_i8_s128, -128i8, &[0x80, 0x7f]}
    test_serialize! {leb128_i8_s127, -127i8, &[0x81, 0x7f]}
    test_serialize! {leb128_i8_s1, -1i8, &[0x7f]}
    test_serialize! {leb128_i8_0, 0i8, &[0x00]}
    test_serialize! {leb128_i8_1, 1i8, &[0x01]}
    test_serialize! {leb128_i8_127, 127i8, &[0xff, 0x00]}
    test_serialize! {leb128_i16_s32768, -32768i16, &[0x80, 0x80, 0x7e]}
    test_serialize! {leb128_i16_s128, -128i16, &[0x80, 0x7f]}
    test_serialize! {leb128_i16_s127, -127i16, &[0x81, 0x7f]}
    test_serialize! {leb128_i16_s1, -1i16, &[0x7f]}
    test_serialize! {leb128_i16_0, 0i16, &[0x00]}
    test_serialize! {leb128_i16_1, 1i16, &[0x01]}
    test_serialize! {leb128_i16_127, 127i16, &[0xff, 0x00]}
    test_serialize! {leb128_i16_32767, 32767i16, &[0xff, 0xff, 0x01]}
    test_serialize! {leb128_i32_s2147483648, -2147483648i32, &[0x80, 0x80, 0x80, 0x80, 0x78]}
    test_serialize! {leb128_i32_s32768, -32768i32, &[0x80, 0x80, 0x7e]}
    test_serialize! {leb128_i32_s128, -128i32, &[0x80, 0x7f]}
    test_serialize! {leb128_i32_s127, -127i32, &[0x81, 0x7f]}
    test_serialize! {leb128_i32_s1, -1i32, &[0x7f]}
    test_serialize! {leb128_i32_0, 0i32, &[0x00]}
    test_serialize! {leb128_i32_1, 1i32, &[0x01]}
    test_serialize! {leb128_i32_100, 100i32, &[0xe4, 0x00]}
    test_serialize! {leb128_i32_127, 127i32, &[0xff, 0x00]}
    test_serialize! {leb128_i32_32767, 32767i32, &[0xff, 0xff, 0x01]}
    test_serialize! {leb128_i32_2147483647, 2147483647i32, &[0xff, 0xff, 0xff, 0xff, 0x07]}
    test_serialize! {leb128_i64_s2147483648, -2147483648i64, &[0x80, 0x80, 0x80, 0x80, 0x78]}
    test_serialize! {leb128_i64_s9223372036854775808, -9223372036854775808i64, &[0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x7f]}
    test_serialize! {leb128_i64_s32768, -32768i64, &[0x80, 0x80, 0x7e]}
    test_serialize! {leb128_i64_s128, -128i64, &[0x80, 0x7f]}
    test_serialize! {leb128_i64_s127, -127i64, &[0x81, 0x7f]}
    test_serialize! {leb128_i64_s1, -1i64, &[0x7f]}
    test_serialize! {leb128_i64_0, 0i64, &[0x00]}
    test_serialize! {leb128_i64_1, 1i64, &[0x01]}
    test_serialize! {leb128_i64_32767, 32767i64, &[0xff, 0xff, 0x01]}
    test_serialize! {leb128_i64_2147483647, 2147483647i64, &[0xff, 0xff, 0xff, 0xff, 0x07]}
    test_serialize! {leb128_i64_9223372036854775807, 9223372036854775807i64, &[0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00]}
}
