use anyhow::{bail, ensure, Result};
use magelang_syntax::BinaryOp;
use magelang_typecheck::{Expr, ExprKind, Type, TypeRepr};
use std::fmt::Write;

use crate::code::Codegen;
use crate::layout::{aggregate, layout, signature, Scalar, Value};

impl<'b, 'a> Codegen<'b, 'a> {
    pub fn scalar(&mut self, expr: &Expr<'a>) -> Result<Value> {
        let values = self.expr(expr)?;
        ensure!(values.len() == 1, "expected a scalar LLVM value");
        Ok(values.into_iter().next().unwrap())
    }

    pub fn expr(&mut self, expr: &Expr<'a>) -> Result<Vec<Value>> {
        use ExprKind::*;
        let constant = match expr.kind {
            ConstI8(value) => Some(value as u64),
            ConstI16(value) => Some(value as u64),
            ConstI32(value) => Some(value as u64),
            ConstI64(value) | ConstIsize(value) => Some(value),
            ConstBool(value) => Some(value as u64),
            _ => None,
        };
        if let Some(value) = constant {
            return Ok(vec![match layout(expr.ty)?.components[0].ty.scalar {
                Scalar::Int(bits) => Value::int(bits, value),
                Scalar::Pointer => self.instruction(Scalar::Pointer, format!("inttoptr i64 {value} to ptr")),
                _ => bail!("invalid integer constant"),
            }]);
        }
        let value = match &expr.kind {
            ConstF32(value) => Value::float(32, **value as f64),
            ConstF64(value) => Value::float(64, **value),
            Zero => {
                return Ok(layout(expr.ty)?
                    .components
                    .iter()
                    .map(|c| match c.ty.scalar {
                        Scalar::Int(bits) => Value::int(bits, 0),
                        Scalar::Float(bits) => Value::float(bits, 0.0),
                        Scalar::Pointer => Value::pointer("null".into()),
                    })
                    .collect())
            }
            StructLit(_, fields) => {
                let mut values = Vec::new();
                for field in *fields {
                    values.extend(self.expr(field)?);
                }
                return Ok(values);
            }
            Bytes(bytes) => {
                if let Some(value) = self.backend.strings.get(*bytes) {
                    value.clone()
                } else {
                    let name = format!("@__magelang_string_{}", self.backend.strings.len());
                    let mut contents = String::new();
                    for byte in *bytes {
                        write!(contents, "\\{byte:02X}").unwrap();
                    }
                    writeln!(
                        self.backend.data,
                        "{name} = internal global [{} x i8] c\"{contents}\", align 1",
                        bytes.len()
                    )
                    .unwrap();
                    let value = Value::pointer(name);
                    self.backend.strings.insert(bytes.to_vec(), value.clone());
                    value
                }
            }
            Local(id) => {
                let address = self.locals[id].clone();
                return self.load(expr.ty, &address);
            }
            Global(name) => {
                let address = self.backend.globals[name].clone();
                return self.load(expr.ty, &address);
            }
            Func(name) => self.backend.functions[&(*name, None)].clone(),
            FuncInst(name, args) => self.backend.functions[&(*name, Some(*args))].clone(),
            GetElement(base, index) => {
                let values = self.expr(base)?;
                return Ok(values[layout(base.ty)?.fields[*index].components.clone()].to_vec());
            }
            GetElementAddr(base, index) => {
                let TypeRepr::Ptr(ty) = base.ty.repr else { bail!("expected struct pointer") };
                let address = self.scalar(base)?;
                self.offset(&address, layout(ty)?.fields[*index].offset)
            }
            GetIndex(base, index) => {
                let (TypeRepr::ArrayPtr(element) | TypeRepr::SlicePtr(element)) = base.ty.repr else {
                    bail!("expected array or slice pointer")
                };
                let values = self.expr(base)?;
                let raw_index = self.scalar(index)?;
                let signed = matches!(index.ty.repr, TypeRepr::Int(true, _));
                let index = self.integer_cast(raw_index, 64, signed);
                if matches!(base.ty.repr, TypeRepr::SlicePtr(..)) {
                    if signed {
                        let invalid = self.instruction(Scalar::Int(1), format!("icmp slt {index}, 0"));
                        self.trap_if(&invalid);
                    }
                    let invalid = self.instruction(Scalar::Int(1), format!("icmp uge {index}, {}", values[1].name));
                    self.trap_if(&invalid);
                }
                let offset = self.instruction(Scalar::Int(64), format!("mul {index}, {}", layout(element)?.size));
                self.instruction(Scalar::Pointer, format!("getelementptr i8, {}, {offset}", values[0]))
            }
            Deref(address) => {
                let address = self.scalar(address)?;
                return self.load(expr.ty, &address);
            }
            Call(callee, arguments) => {
                let ty = callee.ty.as_func().unwrap();
                let buffer = if aggregate(ty.return_type) { Some(self.stack_buffer(ty.return_type)?) } else { None };
                let mut args: Vec<_> = buffer.iter().cloned().collect();
                for argument in *arguments {
                    args.extend(self.expr(argument)?);
                }
                let callee = self.scalar(callee)?;
                let signature = signature(ty)?;
                ensure!(signature.params.len() == args.len(), "LLVM call layout mismatch");
                let args = signature
                    .params
                    .iter()
                    .zip(&args)
                    .map(|(ty, value)| ty.parameter(&value.name))
                    .collect::<Vec<_>>()
                    .join(", ");
                let call = format!("call {} {}({args})", signature.result(), callee.name);
                let result = if let Some(ty) = signature.result {
                    vec![self.instruction(ty.scalar, call)]
                } else {
                    self.emit(call);
                    vec![]
                };
                return if let Some(buffer) = buffer { self.load(ty.return_type, &buffer) } else { Ok(result) };
            }
            Neg(value) => {
                let value = self.scalar(value)?;
                if expr.ty.is_float() {
                    self.instruction(value.ty, format!("fneg {value}"))
                } else {
                    self.instruction(value.ty, format!("sub {} 0, {}", value.ty, value.name))
                }
            }
            BitNot(value) => {
                let value = self.scalar(value)?;
                self.instruction(value.ty, format!("xor {value}, -1"))
            }
            Not(value) => {
                let value = self.scalar(value)?;
                self.instruction(Scalar::Int(1), format!("xor {value}, true"))
            }
            Cast(value, into) => return Ok(vec![self.cast(value, into)?]),
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
            _ => bail!("unresolved expression in LLVM codegen"),
        };
        Ok(vec![value])
    }

    fn cast(&mut self, value: &Expr<'a>, into: &Type<'_>) -> Result<Value> {
        let mut raw = self.scalar(value)?;
        let mut into = layout(into)?.components[0].ty;
        let into_pointer = into.scalar == Scalar::Pointer;
        if raw.ty == Scalar::Pointer {
            if into_pointer {
                return Ok(raw);
            }
            raw = self.instruction(Scalar::Int(64), format!("ptrtoint {raw} to i64"));
        }
        if into_pointer {
            into.scalar = Scalar::Int(64);
        }
        let signed = matches!(value.ty.repr, TypeRepr::Int(true, _));
        let result = match (raw.ty, into.scalar) {
            (from, into) if from == into => raw,
            (Scalar::Int(_), Scalar::Int(bits)) => self.integer_cast(raw, bits, signed),
            (Scalar::Float(from), Scalar::Float(bits)) => {
                let op = if from < bits { "fpext" } else { "fptrunc" };
                self.instruction(into.scalar, format!("{op} {raw} to {}", into.scalar))
            }
            (Scalar::Int(_), Scalar::Float(_)) => {
                let op = if signed { "sitofp" } else { "uitofp" };
                self.instruction(into.scalar, format!("{op} {raw} to {}", into.scalar))
            }
            (Scalar::Float(from), Scalar::Int(bits)) => {
                let width = bits.max(32);
                let value = self.instruction(raw.ty, format!("call {} @llvm.trunc.f{from}({raw})", raw.ty));
                let exponent = if into.signed { width - 1 } else { width };
                let upper = 2f64.powi(exponent as i32);
                let lower = Value::float(from, if into.signed { -upper } else { 0.0 });
                let upper = Value::float(from, upper);
                // Out-of-range/NaN fptosi/fptoui is poison in LLVM, but Magelang must trap.
                let below = self.instruction(Scalar::Int(1), format!("fcmp ult {value}, {}", lower.name));
                let above = self.instruction(Scalar::Int(1), format!("fcmp uge {value}, {}", upper.name));
                let invalid = self.instruction(Scalar::Int(1), format!("or {below}, {}", above.name));
                self.trap_if(&invalid);
                let op = if into.signed { "fptosi" } else { "fptoui" };
                let value = self.instruction(Scalar::Int(width), format!("{op} {value} to i{width}"));
                self.integer_cast(value, bits, false)
            }
            _ => bail!("unsupported LLVM cast"),
        };
        Ok(if into_pointer { self.instruction(Scalar::Pointer, format!("inttoptr {result} to ptr")) } else { result })
    }

    fn binary(&mut self, op: BinaryOp, a: &Expr<'a>, b: &Expr<'a>) -> Result<Vec<Value>> {
        let values = self.expr(a)?;
        self.binary_rhs(op, a.ty, values, b)
    }

    pub fn binary_rhs(&mut self, op: BinaryOp, ty: &Type<'_>, a: Vec<Value>, b: &Expr<'a>) -> Result<Vec<Value>> {
        use BinaryOp::*;
        if matches!(op, And | Or) {
            let rhs = self.new_block();
            let merge = self.new_block();
            let lhs_block = self.current_block.clone();
            if op == And {
                self.branch(&a[0], &rhs, &merge);
            } else {
                self.branch(&a[0], &merge, &rhs);
            }
            self.start_block(&rhs);
            let b = self.scalar(b)?;
            let rhs_block = self.current_block.clone();
            self.jump(&merge);
            self.start_block(&merge);
            return Ok(vec![self.instruction(
                Scalar::Int(1),
                format!("phi i1 [ {}, %{lhs_block} ], [ {}, %{rhs_block} ]", a[0].name, b.name),
            )]);
        }
        let b = self.expr(b)?;
        if matches!(op, Eq | NEq) {
            ensure!(a.len() == b.len(), "equality layout mismatch");
            let mut equal = Value::int(1, 1);
            for (a, b) in a.iter().zip(&b) {
                let op = if matches!(a.ty, Scalar::Float(_)) { "fcmp oeq" } else { "icmp eq" };
                let component = self.instruction(Scalar::Int(1), format!("{op} {a}, {}", b.name));
                equal = self.instruction(Scalar::Int(1), format!("and {equal}, {}", component.name));
            }
            if op == NEq {
                equal = self.instruction(Scalar::Int(1), format!("xor {equal}, true"));
            }
            return Ok(vec![equal]);
        }
        ensure!(a.len() == 1 && b.len() == 1, "binary operation requires scalar operands");
        let (mut a, mut b) = (a.into_iter().next().unwrap(), b.into_iter().next().unwrap());
        let original_type = a.ty;
        let signed = matches!(ty.repr, TypeRepr::Int(true, _));
        if let Scalar::Int(bits) = a.ty {
            if bits < 32 && matches!(op, Div | Mod | ShiftLeft | ShiftRight) {
                a = self.integer_cast(a, 32, signed);
                if matches!(op, Div | Mod) {
                    b = self.integer_cast(b, 32, signed);
                }
            }
        }
        if let Scalar::Int(bits) = a.ty {
            if matches!(op, ShiftLeft | ShiftRight) {
                // LLVM shifts by the bit width or more are poison; Magelang masks the count.
                b = self.integer_cast(b, bits, false);
                b = self.instruction(a.ty, format!("and {b}, {}", bits - 1));
            }
            if matches!(op, Div | Mod) {
                let zero = self.instruction(Scalar::Int(1), format!("icmp eq {b}, 0"));
                self.trap_if(&zero);
                if signed {
                    let min = self.instruction(Scalar::Int(1), format!("icmp eq {a}, {}", 1u64 << (bits - 1)));
                    let minus_one = self.instruction(Scalar::Int(1), format!("icmp eq {b}, -1"));
                    let overflow = self.instruction(Scalar::Int(1), format!("and {min}, {}", minus_one.name));
                    if op == Div {
                        self.trap_if(&overflow);
                    } else {
                        // LLVM srem(MIN, -1) is poison even though the language remainder is zero.
                        b = self.instruction(a.ty, format!("select {overflow}, {} 1, {b}", b.ty));
                    }
                }
            }
        }
        let opcode = if matches!(a.ty, Scalar::Float(_)) {
            match op {
                Add => "fadd",
                Sub => "fsub",
                Mul => "fmul",
                Div => "fdiv",
                Lt => "fcmp olt",
                LEq => "fcmp ole",
                Gt => "fcmp ogt",
                GEq => "fcmp oge",
                _ => bail!("unsupported floating point operation: {op:?}"),
            }
        } else {
            match op {
                Add => "add",
                Sub => "sub",
                Mul => "mul",
                Div => {
                    if signed {
                        "sdiv"
                    } else {
                        "udiv"
                    }
                }
                Mod => {
                    if signed {
                        "srem"
                    } else {
                        "urem"
                    }
                }
                BitAnd => "and",
                BitOr => "or",
                BitXor => "xor",
                ShiftLeft => "shl",
                ShiftRight => {
                    if signed {
                        "ashr"
                    } else {
                        "lshr"
                    }
                }
                Lt => {
                    if signed {
                        "icmp slt"
                    } else {
                        "icmp ult"
                    }
                }
                LEq => {
                    if signed {
                        "icmp sle"
                    } else {
                        "icmp ule"
                    }
                }
                Gt => {
                    if signed {
                        "icmp sgt"
                    } else {
                        "icmp ugt"
                    }
                }
                GEq => {
                    if signed {
                        "icmp sge"
                    } else {
                        "icmp uge"
                    }
                }
                _ => bail!("unsupported integer operation: {op:?}"),
            }
        };
        let result_type = if matches!(op, Lt | LEq | Gt | GEq) { Scalar::Int(1) } else { a.ty };
        let mut result = self.instruction(result_type, format!("{opcode} {a}, {}", b.name));
        if original_type != a.ty {
            let Scalar::Int(bits) = original_type else { unreachable!() };
            result = self.integer_cast(result, bits, signed);
        }
        Ok(vec![result])
    }
}
