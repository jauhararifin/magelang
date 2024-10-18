use crate::data::DataManager;
use crate::errors::CodegenError;
use crate::func::{FuncId, FuncMapper};
use crate::ty::{build_val_type, AlignNormalize, PrimitiveType, StackComponent, TypeManager};
use crate::var::{GlobalManager, LocalManager};
use magelang_syntax::ErrorReporter;
use magelang_typecheck::{BitSize, DefId, Expr, ExprKind, FloatType, Type, TypeArgs, TypeRepr};
use std::iter::zip;
use wasm_helper as wasm;

pub(crate) struct ExprBuilder<'a, 'ctx, E> {
    pub(crate) errors: &'ctx E,
    pub(crate) data: &'a DataManager<'ctx, E>,
    pub(crate) types: &'a TypeManager<'ctx>,
    pub(crate) funcs: &'a FuncMapper<'ctx>,
    pub(crate) locals: &'a LocalManager,
    pub(crate) globals: &'a GlobalManager<'ctx>,
}

#[derive(Debug)]
pub(crate) struct ExprInstr {
    pub(crate) header: Vec<wasm::Instr>,
    pub(crate) components: Vec<Vec<wasm::Instr>>,
}

impl From<wasm::Instr> for ExprInstr {
    fn from(instr: wasm::Instr) -> Self {
        Self {
            header: Vec::default(),
            components: vec![vec![instr]],
        }
    }
}

impl From<Vec<wasm::Instr>> for ExprInstr {
    fn from(instrs: Vec<wasm::Instr>) -> Self {
        Self {
            header: Vec::default(),
            components: vec![instrs],
        }
    }
}

impl From<Vec<Vec<wasm::Instr>>> for ExprInstr {
    fn from(components: Vec<Vec<wasm::Instr>>) -> Self {
        Self {
            header: Vec::default(),
            components,
        }
    }
}

impl ExprInstr {
    pub(crate) fn flatten(self) -> Vec<wasm::Instr> {
        let mut result = self.header;
        for instrs in self.components {
            result.extend(instrs);
        }
        result
    }

    pub(crate) fn slice(self, from: usize, len: usize) -> Self {
        Self {
            header: self.header,
            components: self.components.into_iter().skip(from).take(len).collect(),
        }
    }
}

impl<'a, 'ctx, E: ErrorReporter> ExprBuilder<'a, 'ctx, E> {
    pub(crate) fn build(&self, expr: &Expr<'ctx>) -> ExprInstr {
        match &expr.kind {
            ExprKind::Invalid => unreachable!("found invalid expr"),
            ExprKind::ConstI8(val) => wasm::Instr::I32Const(*val as i32).into(),
            ExprKind::ConstI16(val) => wasm::Instr::I32Const(*val as i32).into(),
            ExprKind::ConstI32(val) => wasm::Instr::I32Const(*val as i32).into(),
            ExprKind::ConstI64(val) => wasm::Instr::I64Const(*val as i64).into(),
            ExprKind::ConstIsize(val) => wasm::Instr::I32Const(*val as i32).into(),
            ExprKind::ConstF32(val) => wasm::Instr::F32Const(**val).into(),
            ExprKind::ConstF64(val) => wasm::Instr::F64Const(**val).into(),
            ExprKind::ConstBool(val) => wasm::Instr::I32Const(*val as i32).into(),
            ExprKind::Zero => build_zero_type(expr.ty).into(),

            ExprKind::StructLit(_, values) => self.build_struct_lit(expr.ty, values),
            ExprKind::Bytes(bytes) => self.build_bytes(bytes),

            ExprKind::Local(idx) => self.build_local(expr.ty, *idx),
            ExprKind::Global(def_id) => self.build_global(expr.ty, *def_id),
            ExprKind::Func(def_id) => self.build_func(*def_id),
            ExprKind::FuncInst(def_id, typeargs) => self.build_func_inst(*def_id, typeargs),

            ExprKind::GetElement(struct_expr, field) => self.build_get_element(struct_expr, *field),
            ExprKind::GetElementAddr(addr, field) => self.build_get_element_addr(addr, *field),
            ExprKind::GetIndex(arr, index) => self.build_get_index(arr, index),
            ExprKind::Deref(addr) => self.build_deref(addr),
            ExprKind::Call(callee, arguments) => self.build_call(callee, arguments),

            ExprKind::Add(a, b) => self.build_add(expr.ty, a, b),
            ExprKind::Sub(a, b) => self.build_sub(expr.ty, a, b),
            ExprKind::Mul(a, b) => self.build_mul(expr.ty, a, b),
            ExprKind::Div(a, b) => self.build_div(expr.ty, a, b),
            ExprKind::Mod(a, b) => self.build_mod(expr.ty, a, b),
            ExprKind::BitOr(a, b) => self.build_bit_or(expr.ty, a, b),
            ExprKind::BitAnd(a, b) => self.build_bit_and(expr.ty, a, b),
            ExprKind::BitXor(a, b) => self.build_bit_xor(expr.ty, a, b),
            ExprKind::ShiftLeft(a, b) => self.build_bit_shl(expr.ty, a, b),
            ExprKind::ShiftRight(a, b) => self.build_bit_shr(expr.ty, a, b),
            ExprKind::And(a, b) => self.build_and(a, b),
            ExprKind::Or(a, b) => self.build_or(a, b),
            ExprKind::Eq(a, b) => self.build_eq(a, b),
            ExprKind::NEq(a, b) => self.build_ne(a, b),
            ExprKind::Gt(a, b) => self.build_gt(a, b),
            ExprKind::GEq(a, b) => self.build_ge(a, b),
            ExprKind::Lt(a, b) => self.build_lt(a, b),
            ExprKind::LEq(a, b) => self.build_le(a, b),
            ExprKind::Neg(value) => self.build_neg(expr.ty, value),
            ExprKind::BitNot(value) => self.build_bit_not(expr.ty, value),
            ExprKind::Not(value) => self.build_not(value),
            ExprKind::Cast(value, ty) => self.build_cast(ty, value),
        }
    }

    fn build_struct_lit(&self, ty: &Type, values: &[Expr<'ctx>]) -> ExprInstr {
        let mut header = values
            .iter()
            .flat_map(|expr| self.build(expr).flatten())
            .collect::<Vec<_>>();

        let types = build_val_type(ty);
        let temps = self.locals.get_temporary_locals(types);
        for id in temps.iter().rev() {
            header.push(wasm::Instr::LocalSet(*id));
        }

        let mut components = Vec::default();
        for id in temps.iter() {
            components.push(vec![wasm::Instr::LocalGet(*id)]);
        }

        ExprInstr { header, components }
    }

    fn build_bytes(&self, bytes: &[u8]) -> ExprInstr {
        vec![vec![wasm::Instr::I32Const(
            self.data.get_bytes(bytes).unwrap() as i32,
        )]]
        .into()
    }

    fn build_local(&self, ty: &Type, idx: usize) -> ExprInstr {
        let wasm_id = self.locals.get_local(idx);

        let types = build_val_type(ty);
        let mut result = Vec::default();
        for i in 0..types.len() {
            result.push(vec![wasm::Instr::LocalGet(wasm_id + i as u32)]);
        }

        result.into()
    }

    fn build_global(&self, ty: &Type, def_id: DefId<'ctx>) -> ExprInstr {
        let id = self.globals.get(def_id);

        let types = build_val_type(ty);
        let mut result = Vec::default();
        for i in 0..types.len() {
            result.push(vec![wasm::Instr::GlobalGet(id + i as u32)]);
        }

        result.into()
    }

    fn build_func(&self, def_id: DefId<'ctx>) -> ExprInstr {
        let func_id = FuncId {
            def_id,
            typeargs: None,
        };
        vec![vec![wasm::Instr::I32Const(
            *self.funcs.func_map.get(&func_id).unwrap() as i32,
        )]]
        .into()
    }

    fn build_func_inst(&self, def_id: DefId<'ctx>, typeargs: &'ctx TypeArgs<'ctx>) -> ExprInstr {
        let func_id = FuncId {
            def_id,
            typeargs: Some(typeargs),
        };
        vec![vec![wasm::Instr::I32Const(
            *self.funcs.func_map.get(&func_id).unwrap() as i32,
        )]]
        .into()
    }

    fn build_get_element(&self, struct_expr: &Expr<'ctx>, field: usize) -> ExprInstr {
        let struct_layout = self.types.get_stack_layout(struct_expr.ty);
        let field_idx = struct_layout.field_index[field];
        let StackComponent { offset, size } = struct_layout.components[field_idx];
        let offset = offset as usize;
        let size = size as usize;
        let expr_instr = self.build(struct_expr);
        expr_instr.slice(offset, size)
    }

    fn build_get_element_addr(&self, addr: &Expr<'ctx>, field: usize) -> ExprInstr {
        let TypeRepr::Ptr(element_type) = addr.ty.repr else {
            unreachable!()
        };
        let Some(struct_layout) = self.types.get_mem_layout(element_type) else {
            self.errors.dereferencing_opaque(addr.pos);
            return vec![wasm::Instr::Unreachable].into();
        };
        let field_idx = struct_layout.field_idx[field];
        let offset = struct_layout.components[field_idx].offset;

        let mut result = self.build(addr).flatten();
        result.push(wasm::Instr::I32Const(offset as i32));
        result.push(wasm::Instr::I32Add);
        result.into()
    }

    fn build_get_index(&self, arr: &Expr<'ctx>, index: &Expr<'ctx>) -> ExprInstr {
        let TypeRepr::ArrayPtr(element_type) = arr.ty.repr else {
            unreachable!()
        };
        let Some(layout) = self.types.get_mem_layout(element_type) else {
            self.errors.dereferencing_opaque(arr.pos);
            return vec![wasm::Instr::Unreachable].into();
        };
        let size = layout.size;

        let mut result = self.build(arr).flatten();
        result.push(wasm::Instr::I32Const(size as i32));
        result.extend(self.build(index).flatten());

        if let TypeRepr::Int(_, bit_size) = index.ty.repr {
            if bit_size == BitSize::I64 {
                result.push(wasm::Instr::I32WrapI64);
            }
        }

        result.push(wasm::Instr::I32Mul);
        result.push(wasm::Instr::I32Add);
        result.into()
    }

    fn build_deref(&self, addr: &Expr<'ctx>) -> ExprInstr {
        let TypeRepr::Ptr(element_type) = &addr.ty.repr else {
            unreachable!()
        };

        let Some(layout) = self.types.get_mem_layout(element_type) else {
            self.errors.dereferencing_opaque(addr.pos);
            return vec![wasm::Instr::Unreachable].into();
        };
        let val_types = build_val_type(element_type);
        assert_eq!(layout.components.len(), val_types.len());

        let mut header = self.build(addr).flatten();
        let temp = self.locals.get_temporary_locals(vec![PrimitiveType::U32]);
        let temp_var = *temp.first().unwrap();
        header.push(wasm::Instr::LocalSet(temp_var));

        let mut components = Vec::default();
        for (component_layout, val_type) in zip(&layout.components, val_types) {
            let mem_arg = wasm::MemArg {
                offset: component_layout.offset,
                align: component_layout.align.normalize(),
            };
            let load_instr = match val_type {
                PrimitiveType::I8 => wasm::Instr::I32Load8S,
                PrimitiveType::U8 => wasm::Instr::I32Load8U,
                PrimitiveType::I16 => wasm::Instr::I32Load16S,
                PrimitiveType::U16 => wasm::Instr::I32Load16U,
                PrimitiveType::I32 | PrimitiveType::U32 => wasm::Instr::I32Load,
                PrimitiveType::I64 | PrimitiveType::U64 => wasm::Instr::I64Load,
                PrimitiveType::F32 => wasm::Instr::F32Load,
                PrimitiveType::F64 => wasm::Instr::F64Load,
                PrimitiveType::Extern => {
                    self.errors.dereferencing_opaque(addr.pos);
                    return vec![wasm::Instr::Unreachable].into();
                }
            };
            let load_instr = load_instr(mem_arg);

            components.push(vec![wasm::Instr::LocalGet(temp_var), load_instr]);
        }

        ExprInstr { header, components }
    }

    fn build_call(&self, callee: &Expr<'ctx>, arguments: &[Expr<'ctx>]) -> ExprInstr {
        let mut header = Vec::default();

        for arg in arguments.iter() {
            header.extend(self.build(arg).flatten());
        }

        let ty = callee.ty;
        let TypeRepr::Func(func_type) = &ty.repr else {
            unreachable!("cannot call non-function expression")
        };

        match callee.kind {
            ExprKind::Func(def_id) => {
                let func_id = FuncId {
                    def_id,
                    typeargs: None,
                };
                header.push(wasm::Instr::Call(
                    *self.funcs.func_map.get(&func_id).unwrap(),
                ))
            }
            ExprKind::FuncInst(def_id, typeargs) => {
                let func_id = FuncId {
                    def_id,
                    typeargs: Some(typeargs),
                };
                header.push(wasm::Instr::Call(
                    *self.funcs.func_map.get(&func_id).unwrap(),
                ))
            }
            _ => {
                let mut parameters = Vec::default();
                for param in func_type.params {
                    let val_types = build_val_type(param);
                    parameters.extend(val_types.into_iter().map(Into::<wasm::ValType>::into));
                }
                let returns = build_val_type(func_type.return_type)
                    .into_iter()
                    .map(PrimitiveType::into)
                    .collect();
                let wasm_type_id = self.types.get_func_type(wasm::FuncType {
                    parameters,
                    returns,
                });

                header.extend(self.build(callee).flatten());
                header.push(wasm::Instr::CallIndirect(0, wasm_type_id));
            }
        }

        let return_types = build_val_type(func_type.return_type);
        let temps = self.locals.get_temporary_locals(return_types);
        for id in temps.iter().rev() {
            header.push(wasm::Instr::LocalSet(*id));
        }

        let mut components = Vec::default();
        for id in temps.iter() {
            components.push(vec![wasm::Instr::LocalGet(*id)]);
        }

        ExprInstr { header, components }
    }

    fn build_add(&self, ty: &Type, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8) => {
                vec![wasm::Instr::I32Add, wasm::Instr::I32Extend8S]
            }
            TypeRepr::Int(true, BitSize::I16) => {
                vec![wasm::Instr::I32Add, wasm::Instr::I32Extend16S]
            }
            TypeRepr::Int(true, BitSize::I32) => vec![wasm::Instr::I32Add],
            TypeRepr::Int(true, BitSize::ISize) => vec![wasm::Instr::I32Add],
            TypeRepr::Int(true, BitSize::I64) => vec![wasm::Instr::I64Add],

            TypeRepr::Int(false, BitSize::I8) => vec![
                wasm::Instr::I32Add,
                wasm::Instr::I32Const(0xff),
                wasm::Instr::I32And,
            ],
            TypeRepr::Int(false, BitSize::I16) => vec![
                wasm::Instr::I32Add,
                wasm::Instr::I32Const(0xffff),
                wasm::Instr::I32And,
            ],
            TypeRepr::Int(false, BitSize::I32) => vec![wasm::Instr::I32Add],
            TypeRepr::Int(false, BitSize::ISize) => vec![wasm::Instr::I32Add],
            TypeRepr::Int(false, BitSize::I64) => vec![wasm::Instr::I64Add],

            TypeRepr::Float(FloatType::F32) => vec![wasm::Instr::F32Add],
            TypeRepr::Float(FloatType::F64) => vec![wasm::Instr::F32Add],
            _ => {
                unreachable!("cannot perform add on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_sub(&self, ty: &Type, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8) => {
                vec![wasm::Instr::I32Sub, wasm::Instr::I32Extend8S]
            }
            TypeRepr::Int(true, BitSize::I16) => {
                vec![wasm::Instr::I32Sub, wasm::Instr::I32Extend16S]
            }
            TypeRepr::Int(true, BitSize::I32) => vec![wasm::Instr::I32Sub],
            TypeRepr::Int(true, BitSize::ISize) => vec![wasm::Instr::I32Sub],
            TypeRepr::Int(true, BitSize::I64) => vec![wasm::Instr::I64Sub],

            TypeRepr::Int(false, BitSize::I8) => vec![
                wasm::Instr::I32Sub,
                wasm::Instr::I32Const(0xff),
                wasm::Instr::I32And,
            ],
            TypeRepr::Int(false, BitSize::I16) => vec![
                wasm::Instr::I32Sub,
                wasm::Instr::I32Const(0xffff),
                wasm::Instr::I32And,
            ],
            TypeRepr::Int(false, BitSize::I32) => vec![wasm::Instr::I32Sub],
            TypeRepr::Int(false, BitSize::ISize) => vec![wasm::Instr::I32Sub],
            TypeRepr::Int(false, BitSize::I64) => vec![wasm::Instr::I64Sub],

            TypeRepr::Float(FloatType::F32) => vec![wasm::Instr::F32Sub],
            TypeRepr::Float(FloatType::F64) => vec![wasm::Instr::F32Sub],
            _ => {
                unreachable!("cannot perform sub on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_mul(&self, ty: &Type, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8) => {
                vec![wasm::Instr::I32Mul, wasm::Instr::I32Extend8S]
            }
            TypeRepr::Int(true, BitSize::I16) => {
                vec![wasm::Instr::I32Mul, wasm::Instr::I32Extend16S]
            }
            TypeRepr::Int(true, BitSize::I32) => vec![wasm::Instr::I32Mul],
            TypeRepr::Int(true, BitSize::ISize) => vec![wasm::Instr::I32Mul],
            TypeRepr::Int(true, BitSize::I64) => vec![wasm::Instr::I64Mul],

            TypeRepr::Int(false, BitSize::I8) => vec![
                wasm::Instr::I32Mul,
                wasm::Instr::I32Const(0xff),
                wasm::Instr::I32And,
            ],
            TypeRepr::Int(false, BitSize::I16) => vec![
                wasm::Instr::I32Mul,
                wasm::Instr::I32Const(0xffff),
                wasm::Instr::I32And,
            ],
            TypeRepr::Int(false, BitSize::I32) => vec![wasm::Instr::I32Mul],
            TypeRepr::Int(false, BitSize::ISize) => vec![wasm::Instr::I32Mul],
            TypeRepr::Int(false, BitSize::I64) => vec![wasm::Instr::I64Mul],

            TypeRepr::Float(FloatType::F32) => vec![wasm::Instr::F32Mul],
            TypeRepr::Float(FloatType::F64) => vec![wasm::Instr::F32Mul],
            _ => {
                unreachable!("cannot perform mul on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_div(&self, ty: &Type, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8) => {
                vec![wasm::Instr::I32DivS, wasm::Instr::I32Extend8S]
            }
            TypeRepr::Int(true, BitSize::I16) => {
                vec![wasm::Instr::I32DivS, wasm::Instr::I32Extend16S]
            }
            TypeRepr::Int(true, BitSize::I32) => vec![wasm::Instr::I32DivS],
            TypeRepr::Int(true, BitSize::ISize) => vec![wasm::Instr::I32DivS],
            TypeRepr::Int(true, BitSize::I64) => vec![wasm::Instr::I64DivS],

            TypeRepr::Int(false, BitSize::I8) => vec![
                wasm::Instr::I32DivU,
                wasm::Instr::I32Const(0xff),
                wasm::Instr::I32And,
            ],
            TypeRepr::Int(false, BitSize::I16) => vec![
                wasm::Instr::I32DivU,
                wasm::Instr::I32Const(0xffff),
                wasm::Instr::I32And,
            ],
            TypeRepr::Int(false, BitSize::I32) => vec![wasm::Instr::I32DivU],
            TypeRepr::Int(false, BitSize::ISize) => vec![wasm::Instr::I32DivU],
            TypeRepr::Int(false, BitSize::I64) => vec![wasm::Instr::I64DivU],

            TypeRepr::Float(FloatType::F32) => vec![wasm::Instr::F32Div],
            TypeRepr::Float(FloatType::F64) => vec![wasm::Instr::F32Div],
            _ => {
                unreachable!("cannot perform div on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_mod(&self, ty: &Type, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8) => {
                vec![wasm::Instr::I32RemS, wasm::Instr::I32Extend8S]
            }
            TypeRepr::Int(true, BitSize::I16) => {
                vec![wasm::Instr::I32RemS, wasm::Instr::I32Extend16S]
            }
            TypeRepr::Int(true, BitSize::I32) => vec![wasm::Instr::I32RemS],
            TypeRepr::Int(true, BitSize::ISize) => vec![wasm::Instr::I32RemS],
            TypeRepr::Int(true, BitSize::I64) => vec![wasm::Instr::I64RemS],

            TypeRepr::Int(false, BitSize::I8) => vec![
                wasm::Instr::I32RemU,
                wasm::Instr::I32Const(0xff),
                wasm::Instr::I32And,
            ],
            TypeRepr::Int(false, BitSize::I16) => vec![
                wasm::Instr::I32RemU,
                wasm::Instr::I32Const(0xffff),
                wasm::Instr::I32And,
            ],
            TypeRepr::Int(false, BitSize::I32) => vec![wasm::Instr::I32RemU],
            TypeRepr::Int(false, BitSize::ISize) => vec![wasm::Instr::I32RemU],
            TypeRepr::Int(false, BitSize::I64) => vec![wasm::Instr::I64RemU],
            _ => {
                unreachable!("cannot perform mod on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_bit_or(&self, ty: &Type, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8) => vec![wasm::Instr::I32Or, wasm::Instr::I32Extend8S],
            TypeRepr::Int(true, BitSize::I16) => {
                vec![wasm::Instr::I32Or, wasm::Instr::I32Extend16S]
            }
            TypeRepr::Int(false, BitSize::I8) => vec![wasm::Instr::I32Or],
            TypeRepr::Int(false, BitSize::I16) => vec![wasm::Instr::I32Or],
            TypeRepr::Int(_, BitSize::I32) => vec![wasm::Instr::I32Or],
            TypeRepr::Int(_, BitSize::ISize) => vec![wasm::Instr::I32Or],
            TypeRepr::Int(_, BitSize::I64) => vec![wasm::Instr::I64Or],
            _ => {
                unreachable!("cannot perform or on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_bit_and(&self, ty: &Type, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8) => vec![wasm::Instr::I32And, wasm::Instr::I32Extend8S],
            TypeRepr::Int(true, BitSize::I16) => {
                vec![wasm::Instr::I32And, wasm::Instr::I32Extend16S]
            }
            TypeRepr::Int(_, BitSize::I8) => vec![wasm::Instr::I32And],
            TypeRepr::Int(_, BitSize::I16) => vec![wasm::Instr::I32And],
            TypeRepr::Int(_, BitSize::I32) => vec![wasm::Instr::I32And],
            TypeRepr::Int(_, BitSize::ISize) => vec![wasm::Instr::I32And],
            TypeRepr::Int(_, BitSize::I64) => vec![wasm::Instr::I64And],
            _ => {
                unreachable!("cannot perform and on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_bit_xor(&self, ty: &Type, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8) => vec![wasm::Instr::I32Xor, wasm::Instr::I32Extend8S],
            TypeRepr::Int(true, BitSize::I16) => {
                vec![wasm::Instr::I32Xor, wasm::Instr::I32Extend16S]
            }
            TypeRepr::Int(_, BitSize::I8) => vec![wasm::Instr::I32Xor],
            TypeRepr::Int(_, BitSize::I16) => vec![wasm::Instr::I32Xor],
            TypeRepr::Int(_, BitSize::I32) => vec![wasm::Instr::I32Xor],
            TypeRepr::Int(_, BitSize::ISize) => vec![wasm::Instr::I32Xor],
            TypeRepr::Int(_, BitSize::I64) => vec![wasm::Instr::I64Xor],
            _ => {
                unreachable!("cannot perform xor on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_bit_shl(&self, ty: &Type, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8) => vec![wasm::Instr::I32Shl, wasm::Instr::I32Extend8S],
            TypeRepr::Int(true, BitSize::I16) => {
                vec![wasm::Instr::I32Shl, wasm::Instr::I32Extend16S]
            }
            TypeRepr::Int(false, BitSize::I8) => vec![
                wasm::Instr::I32Shl,
                wasm::Instr::I32Const(0xff),
                wasm::Instr::I32And,
            ],
            TypeRepr::Int(false, BitSize::I16) => vec![
                wasm::Instr::I32Shl,
                wasm::Instr::I32Const(0xffff),
                wasm::Instr::I32And,
            ],
            TypeRepr::Int(_, BitSize::I32) => vec![wasm::Instr::I32Shl],
            TypeRepr::Int(_, BitSize::ISize) => vec![wasm::Instr::I32Shl],
            TypeRepr::Int(_, BitSize::I64) => vec![wasm::Instr::I64Shl],
            _ => {
                unreachable!("cannot perform shl on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_bit_shr(&self, ty: &Type, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8) => vec![wasm::Instr::I32ShrS],
            TypeRepr::Int(true, BitSize::I16) => vec![wasm::Instr::I32ShrS],
            TypeRepr::Int(true, BitSize::I32) => vec![wasm::Instr::I32ShrS],
            TypeRepr::Int(true, BitSize::ISize) => vec![wasm::Instr::I32ShrS],
            TypeRepr::Int(true, BitSize::I64) => vec![wasm::Instr::I64ShrS],

            TypeRepr::Int(false, BitSize::I8) => vec![wasm::Instr::I32ShrU],
            TypeRepr::Int(false, BitSize::I16) => vec![wasm::Instr::I32ShrU],
            TypeRepr::Int(false, BitSize::I32) => vec![wasm::Instr::I32ShrU],
            TypeRepr::Int(false, BitSize::ISize) => vec![wasm::Instr::I32ShrU],
            TypeRepr::Int(false, BitSize::I64) => vec![wasm::Instr::I64ShrU],

            TypeRepr::Float(FloatType::F32) => vec![wasm::Instr::F32Div],
            TypeRepr::Float(FloatType::F64) => vec![wasm::Instr::F64Div],
            _ => {
                unreachable!("cannot perform div on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_and(&self, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();

        result.push(wasm::Instr::I32Eqz);
        result.push(wasm::Instr::If(
            wasm::BlockType::ValTy(wasm::ValType::Num(wasm::NumType::I32)),
            vec![wasm::Instr::I32Const(0)],
            self.build(b).flatten(),
        ));

        result.into()
    }

    fn build_or(&self, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();

        result.push(wasm::Instr::I32Eqz);
        result.push(wasm::Instr::If(
            wasm::BlockType::ValTy(wasm::ValType::Num(wasm::NumType::I32)),
            self.build(b).flatten(),
            vec![wasm::Instr::I32Const(1)],
        ));

        result.into()
    }

    fn build_eq(&self, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let a_instr = self.build(a).flatten();
        let b_instr = self.build(b).flatten();

        let ty = a.ty;
        let op_instr = match ty.repr {
            TypeRepr::Int(_, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize)
            | TypeRepr::Ptr(..)
            | TypeRepr::ArrayPtr(..)
            | TypeRepr::Func(..) => vec![wasm::Instr::I32Eq],
            TypeRepr::Int(_, BitSize::I64) => vec![wasm::Instr::I64Eq],

            TypeRepr::Float(FloatType::F32) => vec![wasm::Instr::F32Eq],
            TypeRepr::Float(FloatType::F64) => vec![wasm::Instr::F64Eq],

            TypeRepr::Bool => vec![wasm::Instr::I32Eq],

            TypeRepr::Opaque => {
                let mut result = Vec::default();
                if !matches!(a.kind, ExprKind::Zero) {
                    result.extend(a_instr);
                    result.push(wasm::Instr::RefIsNull);
                } else if !matches!(b.kind, ExprKind::Zero) {
                    result.extend(b_instr);
                    result.push(wasm::Instr::RefIsNull);
                } else {
                    result.push(wasm::Instr::I32Const(1));
                }
                return result.into();
            }

            _ => {
                unreachable!("cannot perform eq on {ty:?}");
            }
        };

        let mut result = Vec::default();
        result.extend(a_instr);
        result.extend(b_instr);
        result.extend(op_instr);
        result.into()
    }

    fn build_ne(&self, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let a_instr = self.build(a).flatten();
        let b_instr = self.build(b).flatten();

        let ty = a.ty;
        let op_instr = match ty.repr {
            TypeRepr::Int(_, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                vec![wasm::Instr::I32Eq]
            }
            TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..) | TypeRepr::Func(..) => {
                vec![wasm::Instr::I32Eq]
            }
            TypeRepr::Int(_, BitSize::I64) => vec![wasm::Instr::I64Eq],

            TypeRepr::Float(FloatType::F32) => vec![wasm::Instr::F32Eq],
            TypeRepr::Float(FloatType::F64) => vec![wasm::Instr::F64Eq],

            TypeRepr::Bool => vec![wasm::Instr::I32Eq],

            TypeRepr::Opaque => {
                let mut result = Vec::default();
                if !matches!(a.kind, ExprKind::Zero) {
                    result.extend(a_instr);
                    result.push(wasm::Instr::RefIsNull);
                } else if !matches!(b.kind, ExprKind::Zero) {
                    result.extend(b_instr);
                    result.push(wasm::Instr::RefIsNull);
                } else {
                    result.push(wasm::Instr::I32Const(1));
                }
                result.push(wasm::Instr::I32Eqz);
                return result.into();
            }

            _ => {
                unreachable!("cannot perform neq on {ty:?}");
            }
        };
        let mut result = Vec::default();

        result.extend(a_instr);
        result.extend(b_instr);
        result.extend(op_instr);
        result.push(wasm::Instr::I32Eqz);
        result.into()
    }

    fn build_gt(&self, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let ty = a.ty;
        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                vec![wasm::Instr::I32GtS]
            }
            TypeRepr::Int(true, BitSize::I64) => vec![wasm::Instr::I64GtS],

            TypeRepr::Int(false, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                vec![wasm::Instr::I32GtU]
            }
            TypeRepr::Int(false, BitSize::I64) => vec![wasm::Instr::I64GtU],

            TypeRepr::Float(FloatType::F32) => vec![wasm::Instr::F32Gt],
            TypeRepr::Float(FloatType::F64) => vec![wasm::Instr::F64Gt],
            _ => {
                unreachable!("cannot perform > on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_ge(&self, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let ty = a.ty;
        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                vec![wasm::Instr::I32GeS]
            }
            TypeRepr::Int(true, BitSize::I64) => vec![wasm::Instr::I64GeS],

            TypeRepr::Int(false, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                vec![wasm::Instr::I32GeU]
            }
            TypeRepr::Int(false, BitSize::I64) => vec![wasm::Instr::I64GeU],

            TypeRepr::Float(FloatType::F32) => vec![wasm::Instr::F32Ge],
            TypeRepr::Float(FloatType::F64) => vec![wasm::Instr::F64Ge],
            _ => {
                unreachable!("cannot perform ge on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_lt(&self, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let ty = a.ty;
        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                vec![wasm::Instr::I32LtS]
            }
            TypeRepr::Int(true, BitSize::I64) => {
                vec![wasm::Instr::I64LtS]
            }

            TypeRepr::Int(false, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                vec![wasm::Instr::I32LtU]
            }
            TypeRepr::Int(false, BitSize::I64) => vec![wasm::Instr::I64LtU],

            TypeRepr::Float(FloatType::F32) => vec![wasm::Instr::F32Lt],
            TypeRepr::Float(FloatType::F64) => vec![wasm::Instr::F64Lt],
            _ => {
                unreachable!("cannot perform lt on {ty:?}\n{a:?}\n{b:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_le(&self, a: &Expr<'ctx>, b: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(a).flatten();
        result.extend(self.build(b).flatten());

        let ty = a.ty;
        let instrs = match ty.repr {
            TypeRepr::Int(true, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                vec![wasm::Instr::I32LeS]
            }
            TypeRepr::Int(true, BitSize::I64) => vec![wasm::Instr::I64LeS],

            TypeRepr::Int(false, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                vec![wasm::Instr::I32LeU]
            }
            TypeRepr::Int(false, BitSize::I64) => vec![wasm::Instr::I64LeU],

            TypeRepr::Float(FloatType::F32) => vec![wasm::Instr::F32Le],
            TypeRepr::Float(FloatType::F64) => vec![wasm::Instr::F64Le],
            _ => {
                unreachable!("cannot perform le on {ty:?}");
            }
        };

        result.extend(instrs);
        result.into()
    }

    fn build_neg(&self, ty: &Type, value: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(value).flatten();
        let instrs = match ty.repr {
            TypeRepr::Int(_, BitSize::I64) => {
                vec![wasm::Instr::I64Const(-1), wasm::Instr::I64Mul]
            }

            TypeRepr::Int(true, BitSize::I8) => {
                vec![
                    wasm::Instr::I32Const(-1),
                    wasm::Instr::I32Mul,
                    wasm::Instr::I32Extend8S,
                ]
            }
            TypeRepr::Int(true, BitSize::I16) => {
                vec![
                    wasm::Instr::I32Const(-1),
                    wasm::Instr::I32Mul,
                    wasm::Instr::I32Extend16S,
                ]
            }
            TypeRepr::Int(false, BitSize::I8) => {
                vec![
                    wasm::Instr::I32Const(-1),
                    wasm::Instr::I32Mul,
                    wasm::Instr::I32Const(0xff),
                    wasm::Instr::I32And,
                ]
            }
            TypeRepr::Int(false, BitSize::I16) => {
                vec![
                    wasm::Instr::I32Const(-1),
                    wasm::Instr::I32Mul,
                    wasm::Instr::I32Const(0xffff),
                    wasm::Instr::I32And,
                ]
            }

            TypeRepr::Int(..) => {
                vec![wasm::Instr::I32Const(-1), wasm::Instr::I32Mul]
            }
            TypeRepr::Float(FloatType::F32) => {
                vec![wasm::Instr::F32Const(-1.0), wasm::Instr::F32Mul]
            }
            TypeRepr::Float(FloatType::F64) => {
                vec![wasm::Instr::F64Const(-1.0), wasm::Instr::F64Mul]
            }
            _ => {
                unreachable!("cannot perform neg on {ty:?}");
            }
        };
        result.extend(instrs);
        result.into()
    }

    fn build_bit_not(&self, ty: &Type, value: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(value).flatten();
        let instrs = match ty.repr {
            TypeRepr::Int(_, BitSize::I64) => {
                vec![wasm::Instr::I64Const(-1), wasm::Instr::I64Xor]
            }
            TypeRepr::Int(..) => {
                vec![wasm::Instr::I32Const(-1), wasm::Instr::I32Xor]
            }
            _ => {
                unreachable!("cannot perform bitnot on {ty:?}");
            }
        };
        result.extend(instrs);
        result.into()
    }

    fn build_not(&self, value: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(value).flatten();
        result.push(wasm::Instr::I32Eqz);
        result.into()
    }

    fn build_cast(&self, ty: &Type, value: &Expr<'ctx>) -> ExprInstr {
        let mut result = self.build(value).flatten();

        let source_type = value.ty;
        let target_type = ty;

        let instrs = match source_type.repr {
            TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..) => match target_type.repr {
                TypeRepr::Int(_, BitSize::I64) => vec![wasm::Instr::I64ExtendI32U],
                TypeRepr::Int(..) | TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..) => vec![],
                _ => unreachable!(),
            },
            TypeRepr::Int(source_sign, source_size) => {
                match (source_sign, source_size, &target_type.repr) {
                    // i64, u64 -> i8, i16, i32, isize, u8, u16, u32, usize, *T, [*]T
                    (
                        _,
                        BitSize::I64,
                        TypeRepr::Int(
                            _,
                            BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                        )
                        | TypeRepr::Ptr(..)
                        | TypeRepr::ArrayPtr(..),
                    ) => vec![wasm::Instr::I32WrapI64],

                    // i8, i16, i32, isize -> i64, u64
                    (
                        true,
                        BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                        TypeRepr::Int(_, BitSize::I64),
                    ) => vec![wasm::Instr::I64ExtendI32S],

                    // u8, u16, u32, usize -> i64, u64
                    (
                        false,
                        BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                        TypeRepr::Int(_, BitSize::I64),
                    ) => vec![wasm::Instr::I64ExtendI32U],

                    // u8 -> i8, i16, i32, isize
                    (
                        false,
                        BitSize::I8,
                        TypeRepr::Int(
                            true,
                            BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                        ),
                    ) => vec![wasm::Instr::I32Extend8S],

                    // u16 -> i16, i32, isize
                    (
                        false,
                        BitSize::I16,
                        TypeRepr::Int(true, BitSize::I16 | BitSize::I32 | BitSize::ISize),
                    ) => vec![wasm::Instr::I32Extend16S],
                    // i16 -> u16
                    (true, BitSize::I16, TypeRepr::Int(false, BitSize::I16)) => {
                        vec![wasm::Instr::I32Const(0x0000ffff), wasm::Instr::I32And]
                    }

                    // integer -> integer
                    (_, _, TypeRepr::Int(..) | TypeRepr::ArrayPtr(..) | TypeRepr::Ptr(..)) => {
                        vec![]
                    }

                    // i64 -> f32
                    (true, BitSize::I64, TypeRepr::Float(FloatType::F32)) => {
                        vec![wasm::Instr::F32ConvertI64S]
                    }
                    // u64 -> f32
                    (false, BitSize::I64, TypeRepr::Float(FloatType::F32)) => {
                        vec![wasm::Instr::F32ConvertI64U]
                    }
                    // i8, i16, i32, isize -> f32
                    (true, _, TypeRepr::Float(FloatType::F32)) => {
                        vec![wasm::Instr::F32ConvertI32S]
                    }
                    // u8, u16, u32, usize -> f32
                    (false, _, TypeRepr::Float(FloatType::F32)) => {
                        vec![wasm::Instr::F32ConvertI32U]
                    }

                    // i64 -> f64
                    (true, BitSize::I64, TypeRepr::Float(FloatType::F64)) => {
                        vec![wasm::Instr::F64ConvertI64S]
                    }
                    // u64 -> f64
                    (false, BitSize::I64, TypeRepr::Float(FloatType::F64)) => {
                        vec![wasm::Instr::F64ConvertI64U]
                    }
                    // i8, i16, i32, isize -> f64
                    (true, _, TypeRepr::Float(FloatType::F64)) => {
                        vec![wasm::Instr::F64ConvertI32S]
                    }
                    // u8, u16, u32, usize -> f64
                    (false, _, TypeRepr::Float(FloatType::F64)) => {
                        vec![wasm::Instr::F64ConvertI32U]
                    }

                    _ => unreachable!("{source_sign:?} {source_size:?} {target_type:?}"),
                }
            }
            TypeRepr::Float(source_float_type) => match target_type.repr {
                TypeRepr::Float(target_float_type) => {
                    match (source_float_type, target_float_type) {
                        (FloatType::F32, FloatType::F64) => {
                            vec![wasm::Instr::F64PromoteF32]
                        }
                        (FloatType::F64, FloatType::F32) => {
                            vec![wasm::Instr::F32DemoteF64]
                        }
                        _ => vec![],
                    }
                }
                TypeRepr::Int(sign, size) => match (source_float_type, sign, size) {
                    (
                        FloatType::F32,
                        true,
                        BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    ) => {
                        vec![wasm::Instr::I32TruncF32S]
                    }
                    (FloatType::F32, true, BitSize::I64) => {
                        vec![wasm::Instr::I64TruncF32S]
                    }
                    (
                        FloatType::F32,
                        false,
                        BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    ) => {
                        vec![wasm::Instr::I32TruncF32U]
                    }
                    (FloatType::F32, false, BitSize::I64) => {
                        vec![wasm::Instr::I64TruncF32U]
                    }
                    (
                        FloatType::F64,
                        true,
                        BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    ) => {
                        vec![wasm::Instr::I32TruncF64S]
                    }
                    (FloatType::F64, true, BitSize::I64) => {
                        vec![wasm::Instr::I64TruncF64S]
                    }
                    (
                        FloatType::F64,
                        false,
                        BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    ) => {
                        vec![wasm::Instr::I32TruncF64U]
                    }
                    (FloatType::F64, false, BitSize::I64) => {
                        vec![wasm::Instr::I64TruncF64U]
                    }
                },
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        result.extend(instrs);

        result.into()
    }
}

fn build_zero_type(ty: &Type<'_>) -> Vec<Vec<wasm::Instr>> {
    match &ty.repr {
        TypeRepr::Unknown | TypeRepr::TypeArg(..) => unreachable!("found invalid type"),
        TypeRepr::Struct(struct_type) => struct_type
            .body
            .get()
            .expect("missing struct body")
            .fields
            .values()
            .flat_map(|ty| build_zero_type(ty))
            .collect(),
        TypeRepr::Func(..) => vec![vec![wasm::Instr::I32Const(0)]],
        TypeRepr::Void => vec![vec![]],
        TypeRepr::Opaque => vec![vec![wasm::Instr::RefNull(wasm::RefType::ExternRef)]],
        TypeRepr::Bool => vec![vec![wasm::Instr::I32Const(0)]],
        TypeRepr::Int(_, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
            vec![vec![wasm::Instr::I32Const(0)]]
        }
        TypeRepr::Int(_, BitSize::I64) => vec![vec![wasm::Instr::I64Const(0)]],
        TypeRepr::Float(FloatType::F32) => vec![vec![wasm::Instr::F32Const(0f32)]],
        TypeRepr::Float(FloatType::F64) => vec![vec![wasm::Instr::F64Const(0f64)]],
        TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..) => vec![vec![wasm::Instr::I32Const(0)]],
    }
}
