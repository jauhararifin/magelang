use crate::asm::Assembly;
use crate::layout::{align_up, Layout, Scalar};
use crate::Context;
use magelang_syntax::BinaryOp;
use magelang_typecheck::{Expr, Func, FuncType, Global, Statement};
use std::collections::HashMap;

pub(crate) struct Loop {
    continue_label: String,
    break_label: String,
    defer_mark: usize,
}

pub(crate) struct FunctionBuilder<'ctx, 'a> {
    pub(crate) ctx: &'ctx mut Context<'a>,
    pub(crate) asm: Assembly,
    pub(crate) locals: HashMap<usize, isize>,
    symbol: String,
    ty: Option<&'a FuncType<'a>>,
    frame_size: usize,
    next_label: usize,
    defers: Vec<&'a Statement<'a>>,
    loops: Vec<Loop>,
}

impl<'ctx, 'a> FunctionBuilder<'ctx, 'a> {
    pub(crate) fn new(ctx: &'ctx mut Context<'a>, symbol: String, ty: &'a FuncType<'a>) -> Self {
        Self {
            ctx,
            symbol,
            ty: Some(ty),
            asm: Assembly::default(),
            locals: HashMap::new(),
            frame_size: 0,
            next_label: 0,
            defers: vec![],
            loops: vec![],
        }
    }

    pub(crate) fn entry(ctx: &'ctx mut Context<'a>) -> Self {
        Self {
            ctx,
            symbol: "_main".into(),
            ty: None,
            asm: Assembly::default(),
            locals: HashMap::new(),
            frame_size: 0,
            next_label: 0,
            defers: vec![],
            loops: vec![],
        }
    }

    pub(crate) fn label(&mut self) -> String {
        let label = format!("L{}_{}", self.symbol, self.next_label);
        self.next_label += 1;
        label
    }

    fn reserve_local(&mut self, id: usize, layout: &Layout) -> isize {
        // A deferred block may be emitted on several exits, but its locals need only one frame slot.
        if let Some(offset) = self.locals.get(&id) {
            return *offset;
        }
        self.frame_size += align_up(layout.size.max(1), 16);
        let offset = -(self.frame_size as isize);
        self.locals.insert(id, offset);
        offset
    }

    pub(crate) fn parameters(&mut self) {
        let ty = self.ty.unwrap();
        let count: usize = ty.params.iter().map(|ty| Layout::of(ty).components.len()).sum();
        let mut index = 0;
        for (id, ty) in ty.params.iter().enumerate() {
            let layout = Layout::of(ty);
            let local = self.reserve_local(id, &layout);
            for component in &layout.components {
                self.asm.load(component.scalar, "x29", (16 + (count - index - 1) * 16) as isize);
                self.asm.store(component.scalar, "x29", local + component.offset as isize);
                index += 1;
            }
        }
    }

    pub(crate) fn finish(mut self) -> String {
        if self.ty.is_some_and(|ty| !ty.return_type.is_void()) {
            self.asm.emit("    brk #0");
        }
        self.asm.emit(format!("L{}_return:", self.symbol));
        if self.ty.is_none() {
            self.asm.emit("    mov w0, #0");
        }
        self.asm.emit("    mov sp, x29\n    ldp x29, x30, [sp], #16\n    ret");
        let mut result = Assembly::default();
        if self.ty.is_none() {
            result.emit(".globl _main");
        }
        result.emit(format!(".p2align 2\n{}:\n    stp x29, x30, [sp, #-16]!\n    mov x29, sp", self.symbol));
        result.adjust_stack(-(self.frame_size as isize));
        result.emit(self.asm.0);
        result.0
    }

    pub(crate) fn statement(&mut self, stmt: &'a Statement<'a>) {
        match stmt {
            Statement::Native => unreachable!("native function requires a bridge or intrinsic"),
            Statement::NewLocal { id, value } => {
                let layout = Layout::of(value.ty);
                let offset = self.reserve_local(*id, &layout);
                self.asm.address("x9", "x29", offset);
                self.asm.push("x9");
                self.expr(value);
                self.store_value(&layout);
            }
            Statement::Block(statements) => {
                let mark = self.defers.len();
                for stmt in *statements {
                    self.statement(stmt);
                }
                self.run_defers(mark);
                self.defers.truncate(mark);
            }
            Statement::If(stmt) => {
                let otherwise = self.label();
                let end = self.label();
                self.expr(&stmt.cond);
                self.asm.pop("x9");
                self.asm.emit(format!("    cbz x9, {otherwise}"));
                self.statement(&stmt.body);
                self.asm.emit(format!("    b {end}\n{otherwise}:"));
                if let Some(stmt) = &stmt.else_stmt {
                    self.statement(stmt);
                }
                self.asm.emit(format!("{end}:"));
            }
            Statement::While(stmt) => {
                let start = self.label();
                let end = self.label();
                self.asm.emit(format!("{start}:"));
                self.expr(&stmt.cond);
                self.asm.pop("x9");
                self.asm.emit(format!("    cbz x9, {end}"));
                self.loops.push(Loop {
                    continue_label: start.clone(),
                    break_label: end.clone(),
                    defer_mark: self.defers.len(),
                });
                self.statement(&stmt.body);
                self.loops.pop();
                self.asm.emit(format!("    b {start}\n{end}:"));
            }
            Statement::For(stmt) => {
                let start = self.label();
                let update = self.label();
                let end = self.label();
                if let Some(init) = &stmt.init {
                    self.statement(init);
                }
                self.asm.emit(format!("{start}:"));
                if let Some(cond) = &stmt.cond {
                    self.expr(cond);
                    self.asm.pop("x9");
                    self.asm.emit(format!("    cbz x9, {end}"));
                }
                self.loops.push(Loop {
                    continue_label: update.clone(),
                    break_label: end.clone(),
                    defer_mark: self.defers.len(),
                });
                self.statement(&stmt.body);
                self.asm.emit(format!("{update}:"));
                if let Some(update) = &stmt.update {
                    self.statement(update);
                }
                self.loops.pop();
                self.asm.emit(format!("    b {start}\n{end}:"));
            }
            Statement::Defer(stmt) => self.defers.push(stmt),
            Statement::Return(value) => {
                if let Some(value) = value {
                    self.expr(value);
                }
                self.run_defers(0);
                self.return_value();
            }
            Statement::Break | Statement::Continue => {
                let current = self.loops.last().expect("jump inside a loop");
                let mark = current.defer_mark;
                let label =
                    if matches!(stmt, Statement::Break) { &current.break_label } else { &current.continue_label }
                        .clone();
                self.run_defers(mark);
                self.asm.emit(format!("    b {label}"));
            }
            Statement::Expr(value) => {
                self.expr(value);
                self.asm.adjust_stack(Layout::of(value.ty).stack_size() as isize);
            }
            Statement::Assign { target, value } => self.assign(target, None, value),
            Statement::AssignOp { target, op, value } => self.assign(target, Some(*op), value),
        }
    }

    fn run_defers(&mut self, mark: usize) {
        let saved = self.defers.clone();
        while self.defers.len() > mark {
            let stmt = self.defers.pop().unwrap();
            self.statement(stmt);
        }
        self.defers = saved;
    }

    fn assign(&mut self, target: &'a Expr<'a>, op: Option<BinaryOp>, value: &'a Expr<'a>) {
        let layout = Layout::of(target.ty);
        self.address(target);
        if let Some(op) = op {
            self.asm.emit("    ldr x10, [sp]");
            self.load_value(&layout);
            self.binary_rhs(op, target.ty, value);
        } else {
            self.expr(value);
        }
        self.store_value(&layout);
    }

    pub(crate) fn load_value(&mut self, layout: &Layout) {
        for component in &layout.components {
            self.asm.load(component.scalar, "x10", component.offset as isize);
            self.asm.push("x9");
        }
    }

    // The destination is below the value so it survives calls made while evaluating the RHS.
    pub(crate) fn store_value(&mut self, layout: &Layout) {
        self.asm.address("x10", "sp", layout.stack_size() as isize);
        self.asm.emit("    ldr x10, [x10]");
        for component in layout.components.iter().rev() {
            self.asm.pop("x9");
            self.asm.store(component.scalar, "x10", component.offset as isize);
        }
        self.asm.adjust_stack(16);
    }

    pub(crate) fn global_initializer(&mut self, global: &'a Global<'a>, embedded: Option<(&str, usize)>) {
        self.asm.symbol("x9", &self.ctx.globals[&global.name]);
        self.asm.push("x9");
        if let Some((symbol, len)) = embedded {
            self.asm.symbol("x9", symbol);
            self.asm.push("x9");
            self.asm.immediate("x9", len as u64);
            self.asm.push("x9");
        } else {
            self.expr(&global.value);
        }
        self.store_value(&Layout::of(global.ty));
    }

    fn return_value(&mut self) {
        let ty = self.ty.unwrap();
        let arguments: usize = ty.params.iter().map(|ty| Layout::of(ty).stack_size()).sum();
        let result = Layout::of(ty.return_type);
        for i in (0..result.components.len()).rev() {
            self.asm.pop("x9");
            let offset = 32 + arguments + (result.components.len() - i - 1) * 16;
            self.asm.store(Scalar::Int { signed: false, bits: 64 }, "x29", offset as isize);
        }
        self.asm.emit(format!("    b L{}_return", self.symbol));
    }

    pub(crate) fn intrinsic(&mut self, func: &Func<'a>, name: &str) {
        match name {
            "unreachable" => self.asm.emit("    brk #0"),
            "size_of" | "align_of" => {
                let layout = Layout::of(func.typeargs.unwrap()[0]);
                self.asm.immediate("x9", if name == "size_of" { layout.size } else { layout.align } as u64);
                self.asm.push("x9");
                self.return_value();
            }
            _ => {
                let (fp, gp) = if name.starts_with("f32") { ("s0", "w9") } else { ("d0", "x9") };
                let op = if name.ends_with("floor") { "frintm" } else { "frintp" };
                self.asm.emit(format!(
                    "    ldr x9, [x29, #16]\n    fmov {fp}, {gp}\n    {op} {fp}, {fp}\n    fmov {gp}, {fp}"
                ));
                self.asm.push("x9");
                self.return_value();
            }
        }
    }

    pub(crate) fn native_import(&mut self, symbol: &str) {
        let ty = self.ty.unwrap();
        let mut integer = 0;
        let mut float = 0;
        let mut stack_size = 0;
        let mut locations = vec![];
        for param in ty.params {
            let scalar = Layout::of(param).components[0].scalar;
            let next = if scalar.float() { &mut float } else { &mut integer };
            if *next < 8 {
                locations.push((scalar, Some(*next), 0));
                *next += 1;
            } else {
                // Apple packs stack arguments using their natural size and alignment.
                stack_size = align_up(stack_size, scalar.size());
                locations.push((scalar, None, stack_size));
                stack_size += scalar.size();
            }
        }
        self.asm.adjust_stack(-(align_up(stack_size, 16) as isize));
        for (index, (scalar, register, offset)) in locations.iter().enumerate() {
            self.asm.load(*scalar, "x29", (16 + (ty.params.len() - index - 1) * 16) as isize);
            if let Some(register) = register {
                match scalar {
                    Scalar::F32 => self.asm.emit(format!("    fmov s{register}, w9")),
                    Scalar::F64 => self.asm.emit(format!("    fmov d{register}, x9")),
                    _ => self.asm.emit(format!("    mov x{register}, x9")),
                }
            } else {
                self.asm.store(*scalar, "sp", *offset as isize);
            }
        }
        self.asm.emit(format!("    bl _{symbol}"));
        self.asm.adjust_stack(align_up(stack_size, 16) as isize);
        if let Some(component) = Layout::of(ty.return_type).components.first() {
            match component.scalar {
                Scalar::F32 => self.asm.emit("    fmov w9, s0"),
                Scalar::F64 => self.asm.emit("    fmov x9, d0"),
                _ => self.asm.emit("    mov x9, x0"),
            }
            self.asm.normalize(component.scalar);
            self.asm.push("x9");
        }
        self.return_value();
    }
}
