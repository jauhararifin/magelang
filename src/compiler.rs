use std::{collections::HashMap, rc::Rc};

use crate::{
    bytecode::{Instruction, Object, Value, ValueKind},
    errors::Error,
    semantic::{
        Assign, AssignOp, BinOp, Binary, BlockStatement, Expr, ExprKind, FnDecl, FunctionCall, If, Return, Selector,
        Statement, Type, Unit, Var, While,
    },
};

pub struct SimpleCompiler {
    unit: Unit,

    name_to_index: HashMap<Rc<String>, usize>,
}

impl SimpleCompiler {
    pub fn new(unit: Unit) -> Self {
        Self {
            unit,

            name_to_index: HashMap::new(),
        }
    }
}

impl SimpleCompiler {
    pub fn compile(&mut self) -> Result<Object, Error> {
        let mut values = Vec::new();

        for (id, fn_decl) in self.unit.functions.iter().enumerate() {
            let name = Rc::clone(&fn_decl.header.name.clone());
            self.name_to_index.insert(name, id);
        }

        for fn_decl in self.unit.functions.iter() {
            values.push(self.compile_func(fn_decl)?);
        }

        Ok(Object {
            symbol_table: std::mem::take(&mut self.name_to_index),
            values,
        })
    }

    fn compile_func(&self, fn_decl: &FnDecl) -> Result<Value, Error> {
        let mut instructions = Vec::new();
        let mut ctx = FnContext::new(&fn_decl)?;

        if fn_decl.header.native {
            instructions.push(Instruction::CallNative(fn_decl.header.name.clone()));
            instructions.push(Instruction::Ret);
        } else {
            instructions.extend(self.compile_statement(&mut ctx, fn_decl.body.as_ref().unwrap())?);
        }

        Ok(Value {
            id: 0,
            kind: ValueKind::Fn(instructions),
        })
    }

    fn compile_statement(&self, ctx: &mut FnContext, expr: &Statement) -> Result<Vec<Instruction>, Error> {
        match expr {
            Statement::Block(stmt) => self.compile_block_stmt(ctx, stmt),
            Statement::Var(var) => self.compile_var(ctx, var),
            Statement::Assign(stmt) => self.compile_assign(ctx, stmt),
            Statement::Return(stmt) => self.compile_return(ctx, stmt),
            Statement::If(stmt) => self.compile_if(ctx, stmt),
            Statement::While(stmt) => self.compile_while(ctx, stmt),
            Statement::Expr(expr) => self.compile_expr_stmt(ctx, expr),
        }
    }

    fn compile_block_stmt(&self, ctx: &mut FnContext, stmt: &BlockStatement) -> Result<Vec<Instruction>, Error> {
        let mut instructions = Vec::new();
        ctx.add_block();

        for statement in stmt.body.iter() {
            instructions.extend(self.compile_statement(ctx, statement)?);
        }

        let num_locals = ctx.pop_block();
        instructions.push(Instruction::Pop(num_locals));

        Ok(instructions)
    }

    fn compile_var(&self, ctx: &mut FnContext, var: &Var) -> Result<Vec<Instruction>, Error> {
        let name = Rc::clone(&var.header.name);
        ctx.add_symbol(name);

        if let Some(value) = &var.value {
            self.compile_expr(ctx, value)
        } else {
            Ok(vec![Instruction::Constant(self.empty_value(&var.header.typ)?)])
        }
    }

    fn compile_assign(&self, ctx: &mut FnContext, stmt: &Assign) -> Result<Vec<Instruction>, Error> {
        let mut instructions = self.compile_expr(ctx, &stmt.value)?;

        if !matches!(stmt.op, AssignOp::Assign) {
            let mut receiver = self.compile_expr(ctx, &stmt.receiver)?;
            let op = match stmt.op {
                AssignOp::PlusAssign => Instruction::Add,
                AssignOp::MinusAssign => Instruction::Sub,
                AssignOp::MulAssign => Instruction::Mul,
                AssignOp::DivAssign => Instruction::Div,
                AssignOp::ModAssign => Instruction::Mod,
                AssignOp::BitAndAssign => Instruction::And,
                AssignOp::BitOrAssign => Instruction::Or,
                AssignOp::BitXorAssign => Instruction::Xor,
                AssignOp::ShlAssign => Instruction::Shl,
                AssignOp::ShrAssign => Instruction::Shr,
                _ => unreachable!("{:?}", stmt.op),
            };

            receiver.extend(instructions);
            receiver.push(op);
            instructions = receiver;
        }

        instructions.extend(self.compile_assign_receiver(ctx, &stmt.receiver)?);

        Ok(instructions)
    }

    fn compile_assign_receiver(&self, ctx: &mut FnContext, expr: &Expr) -> Result<Vec<Instruction>, Error> {
        match &expr.kind {
            ExprKind::Ident(name) => self.compile_assign_ident_expr(ctx, name),
            _ => unreachable!(),
        }
    }

    fn compile_assign_ident_expr(&self, ctx: &mut FnContext, name: &String) -> Result<Vec<Instruction>, Error> {
        if let Some(index_type) = ctx.find_symbol(name) {
            Ok(vec![Instruction::SetLocal(index_type.index)])
        } else if let Some(index) = self.name_to_index.get(name) {
            Ok(vec![Instruction::SetGlobal(index.clone())])
        } else {
            unreachable!();
        }
    }

    fn compile_assign_selector_expr(
        &self,
        ctx: &mut FnContext,
        selector: &Selector,
    ) -> Result<Vec<Instruction>, Error> {
        let mut instructions = self.compile_expr(ctx, selector.source.as_ref())?;
        instructions.push(Instruction::SetProp(selector.selection_index));

        Ok(instructions)
    }

    fn compile_return(&self, ctx: &mut FnContext, stmt: &Return) -> Result<Vec<Instruction>, Error> {
        let mut instructions = if let Some(value) = &stmt.value {
            self.compile_expr(ctx, value)?
        } else {
            vec![Instruction::Constant(Value {
                id: 0,
                kind: ValueKind::Void,
            })]
        };

        instructions.push(Instruction::Ret);
        Ok(instructions)
    }

    fn compile_if(&self, ctx: &mut FnContext, stmt: &If) -> Result<Vec<Instruction>, Error> {
        // 1. cond
        // 2. jump_if_false (4.)
        // 3. body
        // 4. <the next instruction>

        let cond = self.compile_expr(ctx, &stmt.cond)?;
        let body = self.compile_statement(ctx, stmt.body.as_ref())?;
        let exit = Instruction::JumpIfFalse(body.len() as isize + 1);

        let mut result = vec![];
        result.extend(cond);
        result.push(exit);
        result.extend(body);

        Ok(result)
    }

    fn compile_while(&self, ctx: &mut FnContext, stmt: &While) -> Result<Vec<Instruction>, Error> {
        // 1. cond
        // 2. jump_if_false (5.)
        // 3. body
        // 4. jump (1.)
        // 5. <the next instruction>

        let cond = self.compile_expr(ctx, &stmt.cond)?;
        let body = self.compile_statement(ctx, stmt.body.as_ref())?;
        let exit = Instruction::JumpIfFalse(body.len() as isize + 1 + 1);
        let go_back = Instruction::Jump(-(body.len() as isize + 1 + cond.len() as isize));

        let mut result = vec![];
        result.extend(cond);
        result.push(exit);
        result.extend(body);
        result.push(go_back);

        Ok(result)
    }

    fn compile_expr_stmt(&self, ctx: &mut FnContext, expr: &Expr) -> Result<Vec<Instruction>, Error> {
        let mut instructions = self.compile_expr(ctx, expr)?;
        instructions.push(Instruction::Pop(1));
        Ok(instructions)
    }

    fn compile_expr(&self, ctx: &FnContext, expr: &Expr) -> Result<Vec<Instruction>, Error> {
        match &expr.kind {
            ExprKind::Ident(name) => self.compile_ident(ctx, name),
            ExprKind::I8(val) => Ok(vec![Instruction::Constant(Value::constant(ValueKind::I8(val.clone())))]),
            ExprKind::I16(val) => Ok(vec![Instruction::Constant(Value::constant(ValueKind::I16(
                val.clone(),
            )))]),
            ExprKind::I32(val) => Ok(vec![Instruction::Constant(Value::constant(ValueKind::I32(
                val.clone(),
            )))]),
            ExprKind::I64(val) => Ok(vec![Instruction::Constant(Value::constant(ValueKind::I64(
                val.clone(),
            )))]),
            ExprKind::U8(val) => Ok(vec![Instruction::Constant(Value::constant(ValueKind::U8(val.clone())))]),
            ExprKind::U16(val) => Ok(vec![Instruction::Constant(Value::constant(ValueKind::U16(
                val.clone(),
            )))]),
            ExprKind::U32(val) => Ok(vec![Instruction::Constant(Value::constant(ValueKind::U32(
                val.clone(),
            )))]),
            ExprKind::U64(val) => Ok(vec![Instruction::Constant(Value::constant(ValueKind::U64(
                val.clone(),
            )))]),
            ExprKind::F32(val) => Ok(vec![Instruction::Constant(Value::constant(ValueKind::F32(
                val.clone(),
            )))]),
            ExprKind::F64(val) => Ok(vec![Instruction::Constant(Value::constant(ValueKind::F64(
                val.clone(),
            )))]),
            ExprKind::Bool(val) => Ok(vec![Instruction::Constant(Value::constant(ValueKind::Bool(
                val.clone(),
            )))]),
            ExprKind::Binary(binary) => self.compile_binary(ctx, binary),
            ExprKind::Unary(_) => todo!(),
            ExprKind::FunctionCall(fn_call) => self.compile_func_call(ctx, fn_call),
            ExprKind::Cast(_) => todo!(),
        }
    }

    fn compile_ident(&self, ctx: &FnContext, name: &String) -> Result<Vec<Instruction>, Error> {
        if let Some(index_type) = ctx.find_symbol(name) {
            Ok(vec![Instruction::GetLocal(index_type.index)])
        } else if let Some(index) = self.name_to_index.get(name) {
            Ok(vec![Instruction::GetGlobal(index.clone())])
        } else {
            unreachable!();
        }
    }

    fn compile_binary(&self, ctx: &FnContext, binary: &Binary) -> Result<Vec<Instruction>, Error> {
        let mut instructions = self.compile_expr(ctx, binary.a.as_ref())?;
        instructions.extend(self.compile_expr(ctx, binary.b.as_ref())?);

        let ins = match binary.op {
            BinOp::Plus => Instruction::Add,
            BinOp::Minus => Instruction::Sub,
            BinOp::Mul => Instruction::Mul,
            BinOp::Div => Instruction::Div,
            BinOp::Mod => Instruction::Mod,
            BinOp::BitAnd => Instruction::And,
            BinOp::BitOr => Instruction::Or,
            BinOp::BitXor => Instruction::Xor,
            BinOp::Shl => Instruction::Shl,
            BinOp::Shr => Instruction::Shr,
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
            BinOp::GT => Instruction::GT,
            BinOp::LT => Instruction::LT,
            BinOp::GTEq => Instruction::GTEq,
            BinOp::LTEq => Instruction::LTEq,
            BinOp::Eq => Instruction::Eq,
            BinOp::NotEq => Instruction::NEq,
        };
        instructions.push(ins);

        Ok(instructions)
    }

    fn compile_func_call(&self, ctx: &FnContext, fn_call: &FunctionCall) -> Result<Vec<Instruction>, Error> {
        let mut instructions = Vec::new();
        for arg in fn_call.args.iter() {
            instructions.extend(self.compile_expr(ctx, arg)?);
        }
        instructions.extend(self.compile_expr(ctx, &fn_call.func)?);
        instructions.push(Instruction::Call(fn_call.args.len()));
        // instructions.push(Instruction::Pop(fn_call.args.len()));

        Ok(instructions)
    }

    fn empty_value(&self, typ: &Type) -> Result<Value, Error> {
        Ok(match typ {
            Type::Int(int_type) => match (int_type.signed, int_type.size) {
                (true, 8) => Value::constant(ValueKind::I8(0)),
                (true, 16) => Value::constant(ValueKind::I16(0)),
                (true, 32) => Value::constant(ValueKind::I32(0)),
                (true, 64) => Value::constant(ValueKind::I64(0)),
                (false, 8) => Value::constant(ValueKind::U8(0)),
                (false, 16) => Value::constant(ValueKind::U16(0)),
                (false, 32) => Value::constant(ValueKind::U32(0)),
                (false, 64) => Value::constant(ValueKind::U64(0)),
                _ => unreachable!(),
            },
            Type::Float(float_type) => match float_type.size {
                32 => Value::constant(ValueKind::F32(0.0)),
                64 => Value::constant(ValueKind::F64(0.0)),
                _ => unreachable!(),
            },
            Type::Bool => Value::constant(ValueKind::Bool(false)),
            Type::Void => Value::constant(ValueKind::Void),
            Type::Fn(_) => todo!(),
            _ => unreachable!(),
        })
    }
}

struct FnContext {
    symbol_tables: Vec<HashMap<Rc<String>, Rc<Symbol>>>,
    counter: isize,
}

#[derive(Debug, Clone)]
struct Symbol {
    index: isize,
}

impl FnContext {
    fn new(fn_decl: &FnDecl) -> Result<Self, Error> {
        let fn_type = &fn_decl.header.fn_type;
        let mut table = HashMap::new();
        for (index, param) in fn_type.arguments.iter().rev().enumerate() {
            let name = param.name.clone();
            table.insert(name, Rc::new(Symbol { index: index as isize }));
        }

        Ok(Self {
            symbol_tables: vec![table],
            counter: fn_type.arguments.len() as isize,
        })
    }

    fn add_block(&mut self) {
        self.symbol_tables.push(HashMap::new());
    }

    fn pop_block(&mut self) -> usize {
        self.symbol_tables.pop().unwrap().len()
    }

    fn add_symbol(&mut self, name: Rc<String>) {
        let index = self.counter;
        self.symbol_tables
            .last_mut()
            .unwrap()
            .insert(name, Rc::new(Symbol { index }));
        self.counter += 1;
    }

    fn find_symbol(&self, name: &String) -> Option<Rc<Symbol>> {
        for block in self.symbol_tables.iter().rev() {
            if let Some(sym) = block.get(name) {
                return Some(Rc::clone(sym));
            }
        }
        None
    }
}
