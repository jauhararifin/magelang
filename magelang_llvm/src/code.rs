use anyhow::{bail, ensure, Result};
use magelang_typecheck::{Expr, ExprKind, Func, Statement, Type};
use std::collections::HashMap;
use std::fmt::Write;

use crate::layout::{aggregate, layout, signature, Scalar, Value};
use crate::Backend;

pub(crate) struct Codegen<'b, 'a> {
    pub backend: &'b mut Backend<'a>,
    pub header: String,
    pub current_block: String,
    pub locals: HashMap<usize, Value>,
    body: String,
    allocas: String,
    next_value: usize,
    next_block: usize,
    live: bool,
    return_buffer: Option<Value>,
    return_type: Option<&'a Type<'a>>,
    defers: Vec<&'a Statement<'a>>,
    loops: Vec<(String, String, usize)>,
}

impl<'b, 'a> Codegen<'b, 'a> {
    pub fn new(backend: &'b mut Backend<'a>) -> Self {
        Self {
            backend,
            header: String::new(),
            current_block: "entry".into(),
            locals: HashMap::new(),
            body: String::new(),
            allocas: String::new(),
            next_value: 0,
            next_block: 0,
            live: true,
            return_buffer: None,
            return_type: None,
            defers: vec![],
            loops: vec![],
        }
    }

    pub fn finish(self) -> String {
        format!("{} {{\nentry:\n{}{} }}\n\n", self.header, self.allocas, self.body)
    }

    pub fn emit(&mut self, instruction: impl AsRef<str>) {
        writeln!(self.body, "  {}", instruction.as_ref()).unwrap();
    }

    fn value(&mut self, ty: Scalar) -> Value {
        let name = format!("%v{}", self.next_value);
        self.next_value += 1;
        Value { ty, name }
    }

    pub fn instruction(&mut self, ty: Scalar, instruction: impl AsRef<str>) -> Value {
        let value = self.value(ty);
        self.emit(format!("{} = {}", value.name, instruction.as_ref()));
        value
    }

    pub fn new_block(&mut self) -> String {
        let label = format!("bb{}", self.next_block);
        self.next_block += 1;
        label
    }

    pub fn start_block(&mut self, label: &str) {
        writeln!(self.body, "{label}:").unwrap();
        self.current_block = label.into();
        self.live = true;
    }

    pub fn jump(&mut self, label: &str) {
        self.emit(format!("br label %{label}"));
        self.live = false;
    }

    pub fn branch(&mut self, cond: &Value, yes: &str, no: &str) {
        self.emit(format!("br {cond}, label %{yes}, label %{no}"));
        self.live = false;
    }

    pub fn trap(&mut self) {
        self.emit("call void @llvm.trap()");
        self.emit("unreachable");
        self.live = false;
    }

    pub fn trap_if(&mut self, condition: &Value) {
        let bad = self.new_block();
        let good = self.new_block();
        self.branch(condition, &bad, &good);
        self.start_block(&bad);
        self.trap();
        self.start_block(&good);
    }

    pub fn function(&mut self, func: &'a Func<'a>, name: &str, intrinsic: Option<&str>) -> Result<()> {
        let ty = func.ty.as_func().unwrap();
        self.return_type = Some(ty.return_type);
        let signature = signature(ty)?;
        let params: Vec<_> = signature
            .params
            .iter()
            .enumerate()
            .map(|(i, ty)| Value { ty: ty.scalar, name: format!("%arg{i}") })
            .collect();
        let parameter_list = signature
            .params
            .iter()
            .zip(&params)
            .map(|(ty, value)| ty.parameter(&value.name))
            .collect::<Vec<_>>()
            .join(", ");
        self.header = format!("define internal {} {name}({parameter_list})", signature.result());
        let mut offset = 0;
        if aggregate(ty.return_type) {
            self.return_buffer = Some(params[0].clone());
            offset = 1;
        }
        for (id, param) in ty.params.iter().enumerate() {
            let count = layout(param)?.components.len();
            let address = self.stack_buffer(param)?;
            self.store(param, &address, &params[offset..offset + count])?;
            self.locals.insert(id, address);
            offset += count;
        }
        if let Some(intrinsic) = intrinsic {
            self.intrinsic(func, intrinsic, &params)?;
        } else {
            self.statement(func.statement)?;
            if self.live {
                if ty.return_type.is_void() {
                    self.emit("ret void");
                } else {
                    self.trap();
                }
            }
        }
        Ok(())
    }

    fn intrinsic(&mut self, func: &Func<'a>, name: &str, params: &[Value]) -> Result<()> {
        let ty = func.ty.as_func().unwrap();
        let result = match name {
            "size_of" | "align_of" => {
                ensure!(func.typeargs.is_some_and(|args| args.len() == 1) && ty.params.is_empty() && ty.return_type.is_usize(), "invalid {name} signature");
                let layout = layout(func.typeargs.unwrap()[0])?;
                Value::int(64, if name == "size_of" { layout.size } else { layout.align } as u64)
            }
            "f32.floor" | "f64.floor" | "f32.ceil" | "f64.ceil" => {
                let valid_type = |ty: &Type<'_>| if name.starts_with("f32") { ty.is_f32() } else { ty.is_f64() };
                ensure!(func.typeargs.is_none() && ty.params.len() == 1 && valid_type(ty.params[0]) && valid_type(ty.return_type), "invalid {name} signature");
                let (ty, op) = name.split_once('.').unwrap();
                self.instruction(params[0].ty, format!("call {} @llvm.{op}.{ty}({})", params[0].ty, params[0]))
            }
            "unreachable" => {
                ensure!(func.typeargs.is_none() && ty.params.is_empty() && ty.return_type.is_void(), "invalid unreachable signature");
                self.trap();
                return Ok(());
            }
            _ => bail!("intrinsic {name:?} is not supported by the native backend (Wasm memory/table APIs are not native APIs)"),
        };
        self.emit(format!("ret {result}"));
        Ok(())
    }

    pub fn stack_buffer(&mut self, ty: &Type<'_>) -> Result<Value> {
        let layout = layout(ty)?;
        let address = self.value(Scalar::Pointer);
        // Hoist allocations so loops and deferred code do not grow the stack on each iteration.
        writeln!(self.allocas, "  {} = alloca [{} x i8], align {}", address.name, layout.size.max(1), layout.align)
            .unwrap();
        Ok(address)
    }

    pub fn offset(&mut self, address: &Value, offset: u32) -> Value {
        if offset == 0 {
            return address.clone();
        }
        self.instruction(Scalar::Pointer, format!("getelementptr i8, {address}, i64 {offset}"))
    }

    pub fn load(&mut self, ty: &Type<'_>, address: &Value) -> Result<Vec<Value>> {
        let mut values = Vec::new();
        for component in layout(ty)?.components {
            let address = self.offset(address, component.offset);
            values.push(
                self.instruction(component.ty.scalar, format!("load {}, {address}, align 1", component.ty.scalar)),
            );
        }
        Ok(values)
    }

    pub fn store(&mut self, ty: &Type<'_>, address: &Value, values: &[Value]) -> Result<()> {
        let layout = layout(ty)?;
        ensure!(layout.components.len() == values.len(), "LLVM value layout mismatch");
        for (component, value) in layout.components.iter().zip(values) {
            let address = self.offset(address, component.offset);
            self.emit(format!("store {value}, {address}, align 1"));
        }
        Ok(())
    }

    fn place(&mut self, expr: &Expr<'a>) -> Result<Value> {
        match &expr.kind {
            ExprKind::Local(id) => Ok(self.locals[id].clone()),
            ExprKind::Global(name) => Ok(self.backend.globals[name].clone()),
            ExprKind::Deref(address) => self.scalar(address),
            ExprKind::GetElement(base, index) => {
                let layout = layout(base.ty)?;
                let address = self.place(base)?;
                Ok(self.offset(&address, layout.fields[*index].offset))
            }
            _ => bail!("unsupported LLVM assignment target"),
        }
    }

    fn emit_defers(&mut self, mark: usize) -> Result<()> {
        let pending = self.defers.clone();
        while self.defers.len() > mark {
            let statement = self.defers.pop().unwrap();
            self.statement(statement)?;
        }
        self.defers = pending;
        Ok(())
    }

    fn statement(&mut self, statement: &'a Statement<'a>) -> Result<()> {
        if !self.live {
            return Ok(());
        }
        match statement {
            Statement::Native => bail!("native declaration has no implementation"),
            Statement::NewLocal { id, value } => {
                let values = self.expr(value)?;
                let address = self.stack_buffer(value.ty)?;
                self.store(value.ty, &address, &values)?;
                self.locals.insert(*id, address);
            }
            Statement::Expr(value) => {
                self.expr(value)?;
            }
            Statement::Assign { target, value } => {
                let address = self.place(target)?;
                let values = self.expr(value)?;
                self.store(target.ty, &address, &values)?;
            }
            Statement::AssignOp { target, op, value } => {
                let address = self.place(target)?;
                let current = self.load(target.ty, &address)?;
                let result = self.binary_rhs(*op, target.ty, current, value)?;
                self.store(target.ty, &address, &result)?;
            }
            Statement::Block(statements) => {
                let mark = self.defers.len();
                for statement in *statements {
                    self.statement(statement)?;
                }
                if self.live {
                    self.emit_defers(mark)?;
                }
                self.defers.truncate(mark);
            }
            Statement::Defer(statement) => self.defers.push(statement),
            Statement::Return(value) => {
                let values = value.as_ref().map(|expr| self.expr(expr)).transpose()?.unwrap_or_default();
                self.emit_defers(0)?;
                if let Some(buffer) = self.return_buffer.clone() {
                    self.store(self.return_type.unwrap(), &buffer, &values)?;
                    self.emit("ret void");
                } else if let Some(value) = values.first() {
                    self.emit(format!("ret {value}"));
                } else {
                    self.emit("ret void");
                }
                self.live = false;
            }
            Statement::If(statement) => {
                let cond = self.scalar(&statement.cond)?;
                let yes = self.new_block();
                let no = self.new_block();
                let merge = self.new_block();
                self.branch(&cond, &yes, &no);
                self.start_block(&yes);
                self.statement(&statement.body)?;
                let yes_live = self.live;
                if self.live {
                    self.jump(&merge);
                }
                self.start_block(&no);
                if let Some(statement) = &statement.else_stmt {
                    self.statement(statement)?;
                }
                let no_live = self.live;
                if self.live {
                    self.jump(&merge);
                }
                if yes_live || no_live {
                    self.start_block(&merge);
                }
            }
            Statement::While(statement) => self.loop_statement(Some(&statement.cond), &statement.body, None)?,
            Statement::For(statement) => {
                if let Some(init) = &statement.init {
                    self.statement(init)?;
                }
                self.loop_statement(statement.cond.as_ref(), &statement.body, statement.update.as_deref())?;
            }
            Statement::Break | Statement::Continue => {
                let (next, exit, mark) =
                    self.loops.last().cloned().ok_or_else(|| anyhow::anyhow!("loop control outside loop"))?;
                self.emit_defers(mark)?;
                self.jump(if matches!(statement, Statement::Break) { &exit } else { &next });
            }
        }
        Ok(())
    }

    fn loop_statement(
        &mut self,
        cond: Option<&'a Expr<'a>>,
        body: &'a Statement<'a>,
        update: Option<&'a Statement<'a>>,
    ) -> Result<()> {
        let header = self.new_block();
        let body_block = self.new_block();
        let next = self.new_block();
        let exit = self.new_block();
        self.jump(&header);
        self.start_block(&header);
        if let Some(cond) = cond {
            let value = self.scalar(cond)?;
            self.branch(&value, &body_block, &exit);
        } else {
            self.jump(&body_block);
        }
        self.start_block(&body_block);
        self.loops.push((next.clone(), exit.clone(), self.defers.len()));
        self.statement(body)?;
        if self.live {
            self.jump(&next);
        }
        self.start_block(&next);
        if let Some(update) = update {
            self.statement(update)?;
        }
        if self.live {
            self.jump(&header);
        }
        self.loops.pop();
        self.start_block(&exit);
        Ok(())
    }

    pub fn integer_cast(&mut self, value: Value, bits: u32, signed: bool) -> Value {
        let Scalar::Int(from) = value.ty else { unreachable!() };
        if from == bits {
            return value;
        }
        let op = if from > bits {
            "trunc"
        } else if signed {
            "sext"
        } else {
            "zext"
        };
        self.instruction(Scalar::Int(bits), format!("{op} {value} to i{bits}"))
    }
}
