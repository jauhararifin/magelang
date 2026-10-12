use crate::code::FunctionBuilder;
use crate::layout::{Layout, Scalar};
use magelang_syntax::BinaryOp;
use magelang_typecheck::{Expr, ExprKind, Type, TypeRepr};

impl<'a> FunctionBuilder<'_, 'a> {
    pub(crate) fn expr(&mut self, expr: &'a Expr<'a>) {
        match &expr.kind {
            ExprKind::ConstI8(value) => self.constant(*value as u64, expr.ty),
            ExprKind::ConstI16(value) => self.constant(*value as u64, expr.ty),
            ExprKind::ConstI32(value) => self.constant(*value as u64, expr.ty),
            ExprKind::ConstI64(value) | ExprKind::ConstIsize(value) => self.constant(*value, expr.ty),
            ExprKind::ConstF32(value) => self.constant(value.to_bits() as u64, expr.ty),
            ExprKind::ConstF64(value) => self.constant(value.to_bits(), expr.ty),
            ExprKind::ConstBool(value) => self.constant(u64::from(*value), expr.ty),
            ExprKind::Zero => {
                for _ in &Layout::of(expr.ty).components {
                    self.asm.push("xzr");
                }
            }
            ExprKind::StructLit(_, fields) => {
                for field in *fields {
                    self.expr(field);
                }
            }
            ExprKind::Bytes(bytes) => {
                let symbol = self.ctx.bytes(bytes);
                self.asm.symbol("x9", &symbol);
                self.asm.push("x9");
            }
            ExprKind::Local(..) | ExprKind::Global(..) | ExprKind::Deref(..) => {
                self.address(expr);
                self.asm.pop("x10");
                self.load_value(&Layout::of(expr.ty));
            }
            ExprKind::Func(id) => {
                self.asm.symbol("x9", &self.ctx.functions[&(*id, None)]);
                self.asm.push("x9");
            }
            ExprKind::FuncInst(id, args) => {
                self.asm.symbol("x9", &self.ctx.functions[&(*id, Some(*args))]);
                self.asm.push("x9");
            }
            ExprKind::GetElement(value, index) => {
                self.expr(value);
                let layout = Layout::of(value.ty);
                let field = &layout.fields[*index];
                let word = Scalar::Int { signed: false, bits: 64 };
                // Copy toward higher addresses before dropping the unused components.
                for i in 0..field.count {
                    self.asm.load(word, "sp", ((layout.components.len() - field.first - i - 1) * 16) as isize);
                    self.asm.store(word, "sp", ((layout.components.len() - i - 1) * 16) as isize);
                }
                self.asm.adjust_stack(((layout.components.len() - field.count) * 16) as isize);
            }
            ExprKind::GetElementAddr(value, index) => {
                let TypeRepr::Ptr(ty) = value.ty.repr else { unreachable!() };
                self.expr(value);
                self.asm.pop("x9");
                self.asm.address("x9", "x9", Layout::of(ty).fields[*index].offset as isize);
                self.asm.push("x9");
            }
            ExprKind::GetIndex(value, index) => {
                self.expr(value);
                self.expr(index);
                self.asm.pop("x11");
                let element = match value.ty.repr {
                    TypeRepr::SlicePtr(element) => {
                        self.asm.pop("x12");
                        let trap = self.label();
                        let valid = self.label();
                        if Layout::of(index.ty).components[0].scalar.signed() {
                            self.asm.emit(format!("    tbnz x11, #63, {trap}"));
                        }
                        self.asm.emit(format!("    cmp x11, x12\n    b.lo {valid}\n{trap}:\n    brk #0\n{valid}:"));
                        element
                    }
                    TypeRepr::ArrayPtr(element) => element,
                    _ => unreachable!(),
                };
                self.asm.pop("x10");
                self.asm.immediate("x9", Layout::of(element).size as u64);
                self.asm.emit("    madd x9, x11, x9, x10");
                self.asm.push("x9");
            }
            ExprKind::Call(callee, arguments) => {
                self.asm.adjust_stack(-(Layout::of(expr.ty).stack_size() as isize));
                self.expr(callee);
                let mut argument_size = 0;
                for arg in *arguments {
                    self.expr(arg);
                    argument_size += Layout::of(arg.ty).stack_size();
                }
                self.asm.address("x9", "sp", argument_size as isize);
                self.asm.emit("    ldr x9, [x9]\n    blr x9");
                self.asm.adjust_stack((argument_size + 16) as isize);
            }
            ExprKind::Add(a, b) => self.binary(BinaryOp::Add, a, b),
            ExprKind::Sub(a, b) => self.binary(BinaryOp::Sub, a, b),
            ExprKind::Mul(a, b) => self.binary(BinaryOp::Mul, a, b),
            ExprKind::Div(a, b) => self.binary(BinaryOp::Div, a, b),
            ExprKind::Mod(a, b) => self.binary(BinaryOp::Mod, a, b),
            ExprKind::BitOr(a, b) => self.binary(BinaryOp::BitOr, a, b),
            ExprKind::BitAnd(a, b) => self.binary(BinaryOp::BitAnd, a, b),
            ExprKind::BitXor(a, b) => self.binary(BinaryOp::BitXor, a, b),
            ExprKind::ShiftLeft(a, b) => self.binary(BinaryOp::ShiftLeft, a, b),
            ExprKind::ShiftRight(a, b) => self.binary(BinaryOp::ShiftRight, a, b),
            ExprKind::And(a, b) => self.binary(BinaryOp::And, a, b),
            ExprKind::Or(a, b) => self.binary(BinaryOp::Or, a, b),
            ExprKind::Eq(a, b) => self.binary(BinaryOp::Eq, a, b),
            ExprKind::NEq(a, b) => self.binary(BinaryOp::NEq, a, b),
            ExprKind::Gt(a, b) => self.binary(BinaryOp::Gt, a, b),
            ExprKind::GEq(a, b) => self.binary(BinaryOp::GEq, a, b),
            ExprKind::Lt(a, b) => self.binary(BinaryOp::Lt, a, b),
            ExprKind::LEq(a, b) => self.binary(BinaryOp::LEq, a, b),
            ExprKind::Neg(value) | ExprKind::BitNot(value) | ExprKind::Not(value) => {
                self.expr(value);
                self.asm.pop("x9");
                let scalar = Layout::of(value.ty).components[0].scalar;
                match expr.kind {
                    ExprKind::Neg(..) if scalar.float() => {
                        let (fp, gp) = if matches!(scalar, Scalar::F32) { ("s0", "w9") } else { ("d0", "x9") };
                        self.asm.emit(format!("    fmov {fp}, {gp}\n    fneg {fp}, {fp}\n    fmov {gp}, {fp}"));
                    }
                    ExprKind::Neg(..) => self.asm.emit("    neg x9, x9"),
                    ExprKind::BitNot(..) => self.asm.emit("    mvn x9, x9"),
                    ExprKind::Not(..) => self.asm.emit("    cmp x9, #0\n    cset x9, eq"),
                    _ => unreachable!(),
                }
                self.asm.normalize(scalar);
                self.asm.push("x9");
            }
            ExprKind::Cast(value, into) => self.cast(value, into),
            ExprKind::Invalid | ExprKind::ConstInt(..) | ExprKind::ConstFloat(..) => {
                unreachable!("unresolved expression in valid module")
            }
        }
    }

    fn constant(&mut self, value: u64, ty: &Type<'_>) {
        self.asm.immediate("x9", value);
        self.asm.normalize(Layout::of(ty).components[0].scalar);
        self.asm.push("x9");
    }

    pub(crate) fn address(&mut self, value: &'a Expr<'a>) {
        match &value.kind {
            ExprKind::Local(id) => self.asm.address("x9", "x29", self.locals[id]),
            ExprKind::Global(id) => self.asm.symbol("x9", &self.ctx.globals[id]),
            ExprKind::Deref(ptr) => {
                self.expr(ptr);
                return;
            }
            ExprKind::GetElement(base, index) => {
                self.address(base);
                self.asm.pop("x9");
                self.asm.address("x9", "x9", Layout::of(base.ty).fields[*index].offset as isize);
            }
            _ => unreachable!("invalid assignment target"),
        }
        self.asm.push("x9");
    }

    fn binary(&mut self, op: BinaryOp, a: &'a Expr<'a>, b: &'a Expr<'a>) {
        self.expr(a);
        self.binary_rhs(op, a.ty, b);
    }

    pub(crate) fn binary_rhs(&mut self, op: BinaryOp, ty: &Type<'_>, rhs: &'a Expr<'a>) {
        if matches!(op, BinaryOp::And | BinaryOp::Or) {
            let end = self.label();
            self.asm.pop("x9");
            let branch = if op == BinaryOp::And { "cbz" } else { "cbnz" };
            self.asm.emit(format!("    {branch} x9, {end}"));
            self.expr(rhs);
            self.asm.pop("x9");
            self.asm.emit(format!("{end}:"));
            self.asm.push("x9");
            return;
        }
        self.expr(rhs);
        let layout = Layout::of(ty);
        if matches!(op, BinaryOp::Eq | BinaryOp::NEq) {
            self.asm.emit("    mov x12, #1");
            for (i, component) in layout.components.iter().enumerate() {
                self.asm.load(component.scalar, "sp", ((2 * layout.components.len() - i - 1) * 16) as isize);
                self.asm.emit("    mov x11, x9");
                self.asm.load(component.scalar, "sp", ((layout.components.len() - i - 1) * 16) as isize);
                match component.scalar {
                    Scalar::F32 => self.asm.emit("    fmov s0, w11\n    fmov s1, w9\n    fcmp s0, s1"),
                    Scalar::F64 => self.asm.emit("    fmov d0, x11\n    fmov d1, x9\n    fcmp d0, d1"),
                    _ => self.asm.emit("    cmp x11, x9"),
                }
                self.asm.emit("    cset x9, eq\n    and x12, x12, x9");
            }
            self.asm.adjust_stack((2 * layout.stack_size()) as isize);
            if op == BinaryOp::NEq {
                self.asm.emit("    eor x12, x12, #1");
            }
            self.asm.push("x12");
            return;
        }
        self.asm.pop("x10");
        self.asm.pop("x9");
        let scalar = layout.components[0].scalar;
        let comparison = matches!(op, BinaryOp::Gt | BinaryOp::GEq | BinaryOp::Lt | BinaryOp::LEq);
        if scalar.float() {
            let (f, g) = if matches!(scalar, Scalar::F32) { ("s", "w") } else { ("d", "x") };
            self.asm.emit(format!("    fmov {f}0, {g}9\n    fmov {f}1, {g}10"));
            if comparison {
                let condition = match op {
                    BinaryOp::Gt => "gt",
                    BinaryOp::GEq => "ge",
                    BinaryOp::Lt => "mi",
                    BinaryOp::LEq => "ls",
                    _ => unreachable!(),
                };
                self.asm.emit(format!("    fcmp {f}0, {f}1\n    cset x9, {condition}"));
            } else {
                let instruction = match op {
                    BinaryOp::Add => "fadd",
                    BinaryOp::Sub => "fsub",
                    BinaryOp::Mul => "fmul",
                    BinaryOp::Div => "fdiv",
                    _ => unreachable!(),
                };
                self.asm.emit(format!("    {instruction} {f}0, {f}0, {f}1\n    fmov {g}9, {f}0"));
            }
        } else if comparison {
            let condition = match (op, scalar.signed()) {
                (BinaryOp::Gt, true) => "gt",
                (BinaryOp::Gt, false) => "hi",
                (BinaryOp::GEq, true) => "ge",
                (BinaryOp::GEq, false) => "hs",
                (BinaryOp::Lt, true) => "lt",
                (BinaryOp::Lt, false) => "lo",
                (BinaryOp::LEq, true) => "le",
                (BinaryOp::LEq, false) => "ls",
                _ => unreachable!(),
            };
            self.asm.emit(format!("    cmp x9, x10\n    cset x9, {condition}"));
        } else {
            match op {
                BinaryOp::Div | BinaryOp::Mod => {
                    let nonzero = self.label();
                    self.asm.emit(format!("    cbnz x10, {nonzero}\n    brk #0\n{nonzero}:"));
                    if op == BinaryOp::Div && scalar.signed() {
                        let valid = self.label();
                        self.asm.emit(format!("    cmn x10, #1\n    b.ne {valid}"));
                        let min = if scalar.size() == 8 { i64::MIN } else { i32::MIN as i64 };
                        self.asm.immediate("x11", min as u64);
                        self.asm.emit(format!("    cmp x9, x11\n    b.ne {valid}\n    brk #0\n{valid}:"));
                    }
                    let instruction = if scalar.signed() { "sdiv" } else { "udiv" };
                    if op == BinaryOp::Div {
                        self.asm.emit(format!("    {instruction} x9, x9, x10"));
                    } else {
                        self.asm.emit(format!("    {instruction} x11, x9, x10\n    msub x9, x11, x10, x9"));
                    }
                }
                _ => {
                    if matches!(op, BinaryOp::ShiftLeft | BinaryOp::ShiftRight) && scalar.size() <= 4 {
                        self.asm.emit("    and x10, x10, #31");
                    }
                    let instruction = match op {
                        BinaryOp::Add => "add",
                        BinaryOp::Sub => "sub",
                        BinaryOp::Mul => "mul",
                        BinaryOp::BitOr => "orr",
                        BinaryOp::BitAnd => "and",
                        BinaryOp::BitXor => "eor",
                        BinaryOp::ShiftLeft => "lsl",
                        BinaryOp::ShiftRight if scalar.signed() => "asr",
                        BinaryOp::ShiftRight => "lsr",
                        _ => unreachable!(),
                    };
                    self.asm.emit(format!("    {instruction} x9, x9, x10"));
                }
            }
        }
        if !comparison {
            self.asm.normalize(scalar);
        }
        self.asm.push("x9");
    }

    fn cast(&mut self, value: &'a Expr<'a>, into: &Type<'_>) {
        self.expr(value);
        self.asm.pop("x9");
        let source = Layout::of(value.ty).components[0].scalar;
        let target = Layout::of(into).components[0].scalar;
        let (sf, sg) = if matches!(source, Scalar::F32) { ("s", "w") } else { ("d", "x") };
        let (tf, tg) = if matches!(target, Scalar::F32) { ("s", "w") } else { ("d", "x") };
        match (source.float(), target.float()) {
            (true, true) => {
                self.asm.emit(format!("    fmov {sf}0, {sg}9"));
                if sf != tf {
                    self.asm.emit(format!("    fcvt {tf}0, {sf}0"));
                }
                self.asm.emit(format!("    fmov {tg}9, {tf}0"));
            }
            (false, true) => {
                let op = if source.signed() { "scvtf" } else { "ucvtf" };
                self.asm.emit(format!("    {op} {tf}0, x9\n    fmov {tg}9, {tf}0"));
            }
            (true, false) => {
                let trap = self.label();
                let valid = self.label();
                let bits = if target.size() <= 4 { 32 } else { 64 };
                let upper = 2_f64.powi(if target.signed() { bits - 1 } else { bits });
                let lower = if target.signed() { -upper } else { 0.0 };
                self.asm.emit(format!("    fmov {sf}0, {sg}9\n    frintz {sf}0, {sf}0"));
                self.asm.immediate(
                    "x10",
                    if source.size() == 4 { (lower as f32).to_bits() as u64 } else { lower.to_bits() },
                );
                self.asm
                    .emit(format!("    fmov {sf}1, {sg}10\n    fcmp {sf}0, {sf}1\n    b.vs {trap}\n    b.mi {trap}"));
                self.asm.immediate(
                    "x10",
                    if source.size() == 4 { (upper as f32).to_bits() as u64 } else { upper.to_bits() },
                );
                self.asm.emit(format!(
                    "    fmov {sf}1, {sg}10\n    fcmp {sf}0, {sf}1\n    b.mi {valid}\n{trap}:\n    brk #0\n{valid}:"
                ));
                let op = if target.signed() { "fcvtzs" } else { "fcvtzu" };
                let reg = if bits == 32 { "w9" } else { "x9" };
                self.asm.emit(format!("    {op} {reg}, {sf}0"));
            }
            (false, false) => {}
        }
        self.asm.normalize(target);
        self.asm.push("x9");
    }
}
