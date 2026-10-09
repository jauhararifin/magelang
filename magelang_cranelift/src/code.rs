use anyhow::{bail, ensure, Result};
use cranelift_codegen::ir::{self, Block, InstBuilder, MemFlagsData, StackSlotData, StackSlotKind, TrapCode, Value};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::Module;
use magelang_typecheck::{DefId, Expr, ExprKind, Func, Statement, Type};
use std::collections::HashMap;

use crate::layout::aggregate;
use crate::Backend;

pub(crate) enum Place {
    Local(Vec<Variable>),
    Memory(Value),
}

pub(crate) struct Codegen<'b, 'a> {
    pub backend: &'b mut Backend<'a>,
    pub builder: FunctionBuilder<'b>,
    pub locals: HashMap<usize, Vec<Variable>>,
    live: bool,
    return_buffer: Option<Value>,
    return_type: Option<&'a Type<'a>>,
    defers: Vec<&'a Statement<'a>>,
    loops: Vec<(Block, Block, usize)>,
}

impl<'b, 'a> Codegen<'b, 'a> {
    pub fn new(backend: &'b mut Backend<'a>, mut builder: FunctionBuilder<'b>) -> Self {
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        Self {
            backend,
            builder,
            locals: HashMap::new(),
            live: true,
            return_buffer: None,
            return_type: None,
            defers: vec![],
            loops: vec![],
        }
    }

    pub fn finish(mut self) {
        self.builder.seal_all_blocks();
        self.builder.finalize(self.backend.object.target_config());
    }

    pub fn function(&mut self, func: &'a Func<'a>, intrinsic: Option<&str>) -> Result<()> {
        let ty = func.ty.as_func().unwrap();
        self.return_type = Some(ty.return_type);
        let params = self.builder.block_params(self.builder.current_block().unwrap()).to_vec();
        let mut offset = 0;
        if aggregate(ty.return_type) {
            self.return_buffer = Some(params[0]);
            offset = 1;
        }
        for (id, param) in ty.params.iter().enumerate() {
            let count = self.backend.layouts.get(param)?.components.len();
            self.new_local(id, &params[offset..offset + count]);
            offset += count;
        }
        if let Some(intrinsic) = intrinsic {
            self.intrinsic(func, intrinsic, &params)?;
        } else {
            self.statement(func.statement)?;
            if self.live {
                if ty.return_type.is_void() {
                    self.builder.ins().return_(&[]);
                } else {
                    self.builder.ins().trap(TrapCode::unwrap_user(1));
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
                let layout = self.backend.layouts.get(func.typeargs.unwrap()[0])?;
                Some(self.builder.ins().iconst(self.backend.layouts.pointer, if name == "size_of" { layout.size } else { layout.align } as i64))
            }
            "f32.floor" | "f64.floor" | "f32.ceil" | "f64.ceil" => {
                let valid_type = |ty: &Type<'_>| if name.starts_with("f32") { ty.is_f32() } else { ty.is_f64() };
                ensure!(func.typeargs.is_none() && ty.params.len() == 1 && valid_type(ty.params[0]) && valid_type(ty.return_type), "invalid {name} signature");
                Some(if name.ends_with("floor") { self.builder.ins().floor(params[0]) } else { self.builder.ins().ceil(params[0]) })
            }
            "unreachable" => {
                ensure!(func.typeargs.is_none() && ty.params.is_empty() && ty.return_type.is_void(), "invalid unreachable signature");
                self.builder.ins().trap(TrapCode::unwrap_user(1));
                return Ok(());
            }
            _ => bail!("intrinsic {name:?} is not supported by the native backend (Wasm memory/table APIs are not native APIs)"),
        };
        self.builder.ins().return_(&result.into_iter().collect::<Vec<_>>());
        Ok(())
    }

    fn new_local(&mut self, id: usize, values: &[Value]) {
        let variables = values
            .iter()
            .map(|value| {
                let variable = self.builder.declare_var(self.builder.func.dfg.value_type(*value));
                self.builder.def_var(variable, *value);
                variable
            })
            .collect();
        self.locals.insert(id, variables);
    }

    pub fn stack_buffer(&mut self, ty: &Type<'_>) -> Result<Value> {
        let layout = self.backend.layouts.get(ty)?;
        let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            layout.size.max(1),
            layout.align.trailing_zeros() as u8,
        ));
        Ok(self.builder.ins().stack_addr(self.backend.layouts.pointer, slot, 0))
    }

    pub fn global_address(&mut self, name: DefId<'a>) -> Value {
        let data = self.backend.object.declare_data_in_func(self.backend.globals[&name], self.builder.func);
        self.builder.ins().symbol_value(self.backend.layouts.pointer, data)
    }

    pub fn load(&mut self, ty: &Type<'_>, address: Value) -> Result<Vec<Value>> {
        Ok(self
            .backend
            .layouts
            .get(ty)?
            .components
            .iter()
            .map(|c| self.builder.ins().load(c.ty, MemFlagsData::new(), address, c.offset))
            .collect())
    }

    pub fn store(&mut self, ty: &Type<'_>, address: Value, values: &[Value]) -> Result<()> {
        let layout = self.backend.layouts.get(ty)?;
        ensure!(layout.components.len() == values.len(), "native value layout mismatch");
        for (component, value) in layout.components.iter().zip(values) {
            self.builder.ins().store(MemFlagsData::new(), *value, address, component.offset);
        }
        Ok(())
    }

    fn place(&mut self, expr: &Expr<'a>) -> Result<Place> {
        Ok(match &expr.kind {
            ExprKind::Local(id) => Place::Local(self.locals[id].clone()),
            ExprKind::Global(name) => Place::Memory(self.global_address(*name)),
            ExprKind::Deref(address) => Place::Memory(self.scalar(address)?),
            ExprKind::GetElement(base, index) => {
                let layout = self.backend.layouts.get(base.ty)?;
                let field = &layout.fields[*index];
                match self.place(base)? {
                    Place::Local(variables) => Place::Local(variables[field.components.clone()].to_vec()),
                    Place::Memory(address) => {
                        Place::Memory(self.builder.ins().iadd_imm_s(address, field.offset as i64))
                    }
                }
            }
            _ => bail!("unsupported native assignment target"),
        })
    }

    fn read_place(&mut self, ty: &Type<'_>, place: &Place) -> Result<Vec<Value>> {
        match place {
            Place::Local(variables) => Ok(variables.iter().map(|v| self.builder.use_var(*v)).collect()),
            Place::Memory(address) => self.load(ty, *address),
        }
    }

    fn write_place(&mut self, ty: &Type<'_>, place: Place, values: &[Value]) -> Result<()> {
        match place {
            Place::Local(variables) => {
                ensure!(variables.len() == values.len(), "native assignment layout mismatch");
                for (variable, value) in variables.iter().zip(values) {
                    self.builder.def_var(*variable, *value);
                }
                Ok(())
            }
            Place::Memory(address) => self.store(ty, address, values),
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
                self.new_local(*id, &values);
            }
            Statement::Expr(value) => {
                self.expr(value)?;
            }
            Statement::Assign { target, value } => {
                let place = self.place(target)?;
                let values = self.expr(value)?;
                self.write_place(target.ty, place, &values)?;
            }
            Statement::AssignOp { target, op, value } => {
                let place = self.place(target)?;
                let current = self.read_place(target.ty, &place)?;
                let result = self.binary_rhs(*op, target.ty, current, value)?;
                self.write_place(target.ty, place, &result)?;
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
                if let Some(buffer) = self.return_buffer {
                    self.store(self.return_type.unwrap(), buffer, &values)?;
                    self.builder.ins().return_(&[]);
                } else {
                    self.builder.ins().return_(&values);
                }
                self.live = false;
            }
            Statement::If(statement) => {
                let cond = self.scalar(&statement.cond)?;
                let yes = self.builder.create_block();
                let no = self.builder.create_block();
                let merge = self.builder.create_block();
                self.builder.ins().brif(cond, yes, &[], no, &[]);
                self.builder.switch_to_block(yes);
                self.statement(&statement.body)?;
                let yes_live = self.live;
                if self.live {
                    self.builder.ins().jump(merge, &[]);
                }
                self.builder.switch_to_block(no);
                self.live = true;
                if let Some(statement) = &statement.else_stmt {
                    self.statement(statement)?;
                }
                if self.live {
                    self.builder.ins().jump(merge, &[]);
                }
                self.live |= yes_live;
                if self.live {
                    self.builder.switch_to_block(merge);
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
                    *self.loops.last().ok_or_else(|| anyhow::anyhow!("loop control outside loop"))?;
                self.emit_defers(mark)?;
                self.builder.ins().jump(if matches!(statement, Statement::Break) { exit } else { next }, &[]);
                self.live = false;
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
        let header = self.builder.create_block();
        let body_block = self.builder.create_block();
        let next = self.builder.create_block();
        let exit = self.builder.create_block();
        self.builder.ins().jump(header, &[]);
        self.builder.switch_to_block(header);
        if let Some(cond) = cond {
            let value = self.scalar(cond)?;
            self.builder.ins().brif(value, body_block, &[], exit, &[]);
        } else {
            self.builder.ins().jump(body_block, &[]);
        }
        self.builder.switch_to_block(body_block);
        self.loops.push((next, exit, self.defers.len()));
        self.statement(body)?;
        if self.live {
            self.builder.ins().jump(next, &[]);
        }
        self.builder.switch_to_block(next);
        self.live = true;
        if let Some(update) = update {
            self.statement(update)?;
        }
        if self.live {
            self.builder.ins().jump(header, &[]);
        }
        self.loops.pop();
        self.builder.switch_to_block(exit);
        self.live = true;
        Ok(())
    }

    pub fn integer_cast(&mut self, value: Value, into: ir::Type, signed: bool) -> Value {
        let from = self.builder.func.dfg.value_type(value);
        if from.bits() < into.bits() {
            if signed {
                self.builder.ins().sextend(into, value)
            } else {
                self.builder.ins().uextend(into, value)
            }
        } else if from.bits() > into.bits() {
            self.builder.ins().ireduce(into, value)
        } else {
            value
        }
    }
}
