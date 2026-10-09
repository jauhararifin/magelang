use anyhow::{bail, ensure, Result};
use cranelift_codegen::ir::{
    condcodes::{FloatCC, IntCC},
    types, InstBuilder, TrapCode, Value,
};
use cranelift_module::{DataDescription, Linkage, Module};
use magelang_syntax::BinaryOp;
use magelang_typecheck::{Expr, ExprKind, Type, TypeRepr};

use crate::code::Codegen;
use crate::layout::aggregate;

impl<'b, 'a> Codegen<'b, 'a> {
    pub fn scalar(&mut self, expr: &Expr<'a>) -> Result<Value> {
        let values = self.expr(expr)?;
        ensure!(values.len() == 1, "expected a scalar native value");
        Ok(values[0])
    }

    pub fn expr(&mut self, expr: &Expr<'a>) -> Result<Vec<Value>> {
        use ExprKind::*;
        let constant = match expr.kind {
            ConstI8(value) => Some(value as i64),
            ConstI16(value) => Some(value as i64),
            ConstI32(value) => Some(value as i64),
            ConstI64(value) | ConstIsize(value) => Some(value as i64),
            ConstBool(value) => Some(value as i64),
            _ => None,
        };
        if let Some(value) = constant {
            let ty = self.backend.layouts.get(expr.ty)?.components[0].ty;
            return Ok(vec![self.builder.ins().iconst(ty, value)]);
        }
        let value = match &expr.kind {
            ConstF32(value) => self.builder.ins().f32const(**value),
            ConstF64(value) => self.builder.ins().f64const(**value),
            Zero => {
                return Ok(self
                    .backend
                    .layouts
                    .get(expr.ty)?
                    .components
                    .iter()
                    .map(|c| {
                        if c.ty == types::F32 {
                            self.builder.ins().f32const(0.0)
                        } else if c.ty == types::F64 {
                            self.builder.ins().f64const(0.0)
                        } else {
                            self.builder.ins().iconst(c.ty, 0)
                        }
                    })
                    .collect());
            }
            StructLit(_, fields) => {
                let mut values = Vec::new();
                for field in *fields {
                    values.extend(self.expr(field)?);
                }
                return Ok(values);
            }
            Bytes(bytes) => {
                let id = if let Some(id) = self.backend.strings.get(*bytes) {
                    *id
                } else {
                    let id = self.backend.object.declare_data(
                        &format!("__magelang_string_{}", self.backend.strings.len()),
                        Linkage::Local,
                        true,
                        false,
                    )?;
                    let mut data = DataDescription::new();
                    data.define(bytes.to_vec().into_boxed_slice());
                    self.backend.object.define_data(id, &data)?;
                    self.backend.strings.insert(bytes.to_vec(), id);
                    id
                };
                let data = self.backend.object.declare_data_in_func(id, self.builder.func);
                self.builder.ins().symbol_value(self.backend.layouts.pointer, data)
            }
            Local(id) => {
                return Ok(self.locals[id].iter().map(|variable| self.builder.use_var(*variable)).collect());
            }
            Global(name) => {
                let address = self.global_address(*name);
                return self.load(expr.ty, address);
            }
            Func(name) | FuncInst(name, _) => {
                let typeargs = if let FuncInst(_, args) = expr.kind { Some(args) } else { None };
                let id = self.backend.functions[&(*name, typeargs)];
                let reference = self.backend.object.declare_func_in_func(id, self.builder.func);
                self.builder.ins().func_addr(self.backend.layouts.pointer, reference)
            }
            GetElement(base, index) => {
                let values = self.expr(base)?;
                let layout = self.backend.layouts.get(base.ty)?;
                return Ok(values[layout.fields[*index].components.clone()].to_vec());
            }
            GetElementAddr(base, index) => {
                let TypeRepr::Ptr(ty) = base.ty.repr else { bail!("expected struct pointer") };
                let layout = self.backend.layouts.get(ty)?;
                let address = self.scalar(base)?;
                self.builder.ins().iadd_imm_s(address, layout.fields[*index].offset as i64)
            }
            GetIndex(base, index) => {
                let (TypeRepr::ArrayPtr(element) | TypeRepr::SlicePtr(element)) = base.ty.repr else {
                    bail!("expected array or slice pointer")
                };
                let values = self.expr(base)?;
                let raw_index = self.scalar(index)?;
                let signed = matches!(index.ty.repr, TypeRepr::Int(true, _));
                let wide_index = self.integer_cast(raw_index, types::I64, signed);
                if matches!(base.ty.repr, TypeRepr::SlicePtr(..)) {
                    if signed {
                        let zero = self.builder.ins().iconst(types::I64, 0);
                        let negative = self.builder.ins().icmp(IntCC::SignedLessThan, wide_index, zero);
                        self.builder.ins().trapnz(negative, TrapCode::unwrap_user(2));
                    }
                    let len = self.integer_cast(values[1], types::I64, false);
                    let invalid = self.builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, wide_index, len);
                    self.builder.ins().trapnz(invalid, TrapCode::unwrap_user(2));
                }
                let index = self.integer_cast(wide_index, self.backend.layouts.pointer, false);
                let size = self.backend.layouts.get(element)?.size;
                let offset = self.builder.ins().imul_imm_s(index, size as i64);
                self.builder.ins().iadd(values[0], offset)
            }
            Deref(address) => {
                let address = self.scalar(address)?;
                return self.load(expr.ty, address);
            }
            Call(callee, arguments) => {
                let ty = callee.ty.as_func().unwrap();
                let buffer = if aggregate(ty.return_type) { Some(self.stack_buffer(ty.return_type)?) } else { None };
                let mut args: Vec<_> = buffer.into_iter().collect();
                for argument in *arguments {
                    args.extend(self.expr(argument)?);
                }
                let direct = match callee.kind {
                    Func(name) => Some((name, None)),
                    FuncInst(name, args) => Some((name, Some(args))),
                    _ => None,
                };
                let call = if let Some(key) = direct {
                    let reference =
                        self.backend.object.declare_func_in_func(self.backend.functions[&key], self.builder.func);
                    self.builder.ins().call(reference, &args)
                } else {
                    let pointer = self.scalar(callee)?;
                    let signature = self.backend.layouts.signature(&self.backend.object, ty)?;
                    let signature = self.builder.import_signature(signature);
                    self.builder.ins().call_indirect(signature, pointer, &args)
                };
                return if let Some(buffer) = buffer {
                    self.load(ty.return_type, buffer)
                } else {
                    Ok(self.builder.inst_results(call).to_vec())
                };
            }
            Neg(value) => {
                let value = self.scalar(value)?;
                if expr.ty.is_float() {
                    self.builder.ins().fneg(value)
                } else {
                    self.builder.ins().ineg(value)
                }
            }
            BitNot(value) => {
                let value = self.scalar(value)?;
                self.builder.ins().bnot(value)
            }
            Not(value) => {
                let value = self.scalar(value)?;
                let zero = self.builder.ins().iconst(types::I8, 0);
                self.builder.ins().icmp(IntCC::Equal, value, zero)
            }
            Cast(value, into) => {
                let raw = self.scalar(value)?;
                let from = self.builder.func.dfg.value_type(raw);
                let into_layout = self.backend.layouts.get(into)?;
                let into_type = into_layout.components[0].ty;
                let signed = matches!(value.ty.repr, TypeRepr::Int(true, _));
                if from == into_type {
                    raw
                } else if from.is_int() && into_type.is_int() {
                    self.integer_cast(raw, into_type, signed)
                } else if from.is_float() && into_type.is_float() {
                    if from.bits() < into_type.bits() {
                        self.builder.ins().fpromote(into_type, raw)
                    } else {
                        self.builder.ins().fdemote(into_type, raw)
                    }
                } else if from.is_int() {
                    let raw = if from.bits() < 32 { self.integer_cast(raw, types::I32, signed) } else { raw };
                    if signed {
                        self.builder.ins().fcvt_from_sint(into_type, raw)
                    } else {
                        self.builder.ins().fcvt_from_uint(into_type, raw)
                    }
                } else {
                    let conversion_type = if into_type.bits() < 32 { types::I32 } else { into_type };
                    let converted = if into_layout.components[0].signed {
                        self.builder.ins().fcvt_to_sint(conversion_type, raw)
                    } else {
                        self.builder.ins().fcvt_to_uint(conversion_type, raw)
                    };
                    self.integer_cast(converted, into_type, false)
                }
            }
            Add(a, b) => return self.binary(BinaryOp::Add, a, b),
            Sub(a, b) => return self.binary(BinaryOp::Sub, a, b),
            Mul(a, b) => return self.binary(BinaryOp::Mul, a, b),
            Div(a, b) => return self.binary(BinaryOp::Div, a, b),
            Mod(a, b) => return self.binary(BinaryOp::Mod, a, b),
            BitOr(a, b) => return self.binary(BinaryOp::BitOr, a, b),
            BitAnd(a, b) => return self.binary(BinaryOp::BitAnd, a, b),
            BitXor(a, b) => return self.binary(BinaryOp::BitXor, a, b),
            ShiftLeft(a, b) => return self.binary(BinaryOp::ShiftLeft, a, b),
            ShiftRight(a, b) => return self.binary(BinaryOp::ShiftRight, a, b),
            And(a, b) => return self.binary(BinaryOp::And, a, b),
            Or(a, b) => return self.binary(BinaryOp::Or, a, b),
            Eq(a, b) => return self.binary(BinaryOp::Eq, a, b),
            NEq(a, b) => return self.binary(BinaryOp::NEq, a, b),
            Gt(a, b) => return self.binary(BinaryOp::Gt, a, b),
            GEq(a, b) => return self.binary(BinaryOp::GEq, a, b),
            Lt(a, b) => return self.binary(BinaryOp::Lt, a, b),
            LEq(a, b) => return self.binary(BinaryOp::LEq, a, b),
            _ => bail!("unresolved expression in native codegen"),
        };
        Ok(vec![value])
    }

    fn binary(&mut self, op: BinaryOp, a: &Expr<'a>, b: &Expr<'a>) -> Result<Vec<Value>> {
        let values = self.expr(a)?;
        self.binary_rhs(op, a.ty, values, b)
    }

    pub fn binary_rhs(&mut self, op: BinaryOp, ty: &Type<'_>, a: Vec<Value>, b: &Expr<'a>) -> Result<Vec<Value>> {
        use BinaryOp::*;
        if matches!(op, And | Or) {
            let rhs = self.builder.create_block();
            let merge = self.builder.create_block();
            self.builder.append_block_param(merge, types::I8);
            if op == And {
                self.builder.ins().brif(a[0], rhs, &[], merge, &[a[0].into()]);
            } else {
                self.builder.ins().brif(a[0], merge, &[a[0].into()], rhs, &[]);
            }
            self.builder.switch_to_block(rhs);
            let b = self.scalar(b)?;
            self.builder.ins().jump(merge, &[b.into()]);
            self.builder.switch_to_block(merge);
            return Ok(vec![self.builder.block_params(merge)[0]]);
        }
        let b = self.expr(b)?;
        if matches!(op, Eq | NEq) {
            ensure!(a.len() == b.len(), "equality layout mismatch");
            let mut equal = self.builder.ins().iconst(types::I8, 1);
            for (a, b) in a.iter().zip(&b) {
                let component = if self.builder.func.dfg.value_type(*a).is_float() {
                    self.builder.ins().fcmp(FloatCC::Equal, *a, *b)
                } else {
                    self.builder.ins().icmp(IntCC::Equal, *a, *b)
                };
                equal = self.builder.ins().band(equal, component);
            }
            if op == NEq {
                let zero = self.builder.ins().iconst(types::I8, 0);
                equal = self.builder.ins().icmp(IntCC::Equal, equal, zero);
            }
            return Ok(vec![equal]);
        }
        ensure!(a.len() == 1 && b.len() == 1, "binary operation requires scalar operands");
        let (mut a, mut b) = (a[0], b[0]);
        let value_type = self.builder.func.dfg.value_type(a);
        let signed = matches!(ty.repr, TypeRepr::Int(true, _));
        let widen = value_type.is_int() && value_type.bits() < 32 && matches!(op, Div | Mod | ShiftLeft | ShiftRight);
        if widen {
            a = self.integer_cast(a, types::I32, signed);
            if matches!(op, Div | Mod) {
                b = self.integer_cast(b, types::I32, signed);
            }
        }
        let value = if value_type.is_float() {
            match op {
                Add => self.builder.ins().fadd(a, b),
                Sub => self.builder.ins().fsub(a, b),
                Mul => self.builder.ins().fmul(a, b),
                Div => self.builder.ins().fdiv(a, b),
                Lt => self.builder.ins().fcmp(FloatCC::LessThan, a, b),
                LEq => self.builder.ins().fcmp(FloatCC::LessThanOrEqual, a, b),
                Gt => self.builder.ins().fcmp(FloatCC::GreaterThan, a, b),
                GEq => self.builder.ins().fcmp(FloatCC::GreaterThanOrEqual, a, b),
                _ => bail!("unsupported floating point operation: {op:?}"),
            }
        } else {
            match op {
                Add => self.builder.ins().iadd(a, b),
                Sub => self.builder.ins().isub(a, b),
                Mul => self.builder.ins().imul(a, b),
                Div if signed => self.builder.ins().sdiv(a, b),
                Div => self.builder.ins().udiv(a, b),
                Mod if signed => self.builder.ins().srem(a, b),
                Mod => self.builder.ins().urem(a, b),
                BitOr => self.builder.ins().bor(a, b),
                BitAnd => self.builder.ins().band(a, b),
                BitXor => self.builder.ins().bxor(a, b),
                ShiftLeft => self.builder.ins().ishl(a, b),
                ShiftRight if signed => self.builder.ins().sshr(a, b),
                ShiftRight => self.builder.ins().ushr(a, b),
                Lt => {
                    self.builder.ins().icmp(if signed { IntCC::SignedLessThan } else { IntCC::UnsignedLessThan }, a, b)
                }
                LEq => self.builder.ins().icmp(
                    if signed { IntCC::SignedLessThanOrEqual } else { IntCC::UnsignedLessThanOrEqual },
                    a,
                    b,
                ),
                Gt => self.builder.ins().icmp(
                    if signed { IntCC::SignedGreaterThan } else { IntCC::UnsignedGreaterThan },
                    a,
                    b,
                ),
                GEq => self.builder.ins().icmp(
                    if signed { IntCC::SignedGreaterThanOrEqual } else { IntCC::UnsignedGreaterThanOrEqual },
                    a,
                    b,
                ),
                _ => bail!("unsupported integer operation: {op:?}"),
            }
        };
        Ok(vec![if widen { self.integer_cast(value, value_type, signed) } else { value }])
    }
}
