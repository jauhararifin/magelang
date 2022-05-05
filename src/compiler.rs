use std::{collections::HashMap, rc::Rc};

use crate::{
    bytecode::{Instruction, Object, Value, ValueKind},
    semantic::{
        Assign, AssignOp, BinOp, Binary, BlockStatement, Cast, Expr, ExprKind, FnDecl, FunctionCall, If, Return,
        Statement, Type, Unary, UnaryOp, Unit, Var, While,
    },
};

pub struct Compiler();

impl Compiler {
    pub fn new() -> Self {
        Self()
    }

    pub fn compile(&self, unit: &Unit) -> Object {
        let mut compiler = SimpleCompiler::new(unit);
        compiler.compile()
    }
}

struct SimpleCompiler<'a> {
    unit: &'a Unit,
    name_to_index: HashMap<Rc<String>, usize>,
}

impl<'a> SimpleCompiler<'a> {
    fn new(unit: &'a Unit) -> Self {
        Self {
            unit,
            name_to_index: HashMap::new(),
        }
    }

    fn compile(&mut self) -> Object {
        let mut values = Vec::new();

        for (id, fn_decl) in self.unit.functions.iter().enumerate() {
            let name = Rc::clone(&fn_decl.header.name.clone());
            self.name_to_index.insert(name, id);
        }

        for fn_decl in self.unit.functions.iter() {
            values.push(self.compile_func(fn_decl));
        }

        Object {
            symbol_table: std::mem::take(&mut self.name_to_index),
            values,
        }
    }

    fn compile_func(&self, fn_decl: &FnDecl) -> Value {
        let mut instructions = Vec::new();
        let mut ctx = FnContext::new(&fn_decl);

        if fn_decl.header.native {
            instructions.push(Instruction::CallNative(fn_decl.header.name.clone()));
            instructions.push(Instruction::Ret);
        } else {
            instructions.extend(self.compile_statement(&mut ctx, fn_decl.body.as_ref().unwrap()));
        }

        Value {
            kind: ValueKind::Fn(instructions),
        }
    }

    fn compile_statement(&self, ctx: &mut FnContext, expr: &Statement) -> Vec<Instruction> {
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

    fn compile_block_stmt(&self, ctx: &mut FnContext, stmt: &BlockStatement) -> Vec<Instruction> {
        ctx.add_block();

        let mut instructions = Vec::new();
        for statement in stmt.body.iter() {
            instructions.extend(self.compile_statement(ctx, statement));
        }

        let num_locals = ctx.pop_block();
        instructions.push(Instruction::Pop(num_locals));

        instructions
    }

    fn compile_var(&self, ctx: &mut FnContext, var: &Var) -> Vec<Instruction> {
        let name = Rc::clone(&var.header.name);
        ctx.add_symbol(name);

        if let Some(value) = &var.value {
            self.compile_expr(ctx, value)
        } else {
            vec![Instruction::Constant(self.empty_value(&var.header.typ))]
        }
    }

    fn compile_assign(&self, ctx: &mut FnContext, stmt: &Assign) -> Vec<Instruction> {
        let mut instructions = self.compile_expr(ctx, &stmt.value);

        let (signed, size, is_float) = match stmt.receiver.typ.as_ref() {
            Type::Int(t) => (t.signed, t.size, false),
            Type::Float(t) => (false, t.size, true),
            _ => unreachable!(),
        };

        if !matches!(stmt.op, AssignOp::Assign) {
            let mut receiver = self.compile_expr(ctx, &stmt.receiver);
            let op = match (is_float, stmt.op) {
                (false, AssignOp::PlusAssign) => Instruction::Add(signed, size),
                (false, AssignOp::MinusAssign) => Instruction::Sub(signed, size),
                (false, AssignOp::MulAssign) => Instruction::Mul(signed, size),
                (false, AssignOp::DivAssign) => Instruction::Div(signed, size),
                (true, AssignOp::PlusAssign) => Instruction::AddFloat(size),
                (true, AssignOp::MinusAssign) => Instruction::SubFloat(size),
                (true, AssignOp::MulAssign) => Instruction::MulFloat(size),
                (true, AssignOp::DivAssign) => Instruction::DivFloat(size),
                (_, AssignOp::ModAssign) => Instruction::Mod(signed, size),
                (_, AssignOp::BitAndAssign) => Instruction::And(size),
                (_, AssignOp::BitOrAssign) => Instruction::Or(size),
                (_, AssignOp::BitXorAssign) => Instruction::Xor(size),
                (_, AssignOp::ShlAssign) => Instruction::Shl(size),
                (_, AssignOp::ShrAssign) => Instruction::Shr(size),
                _ => unreachable!(),
            };

            receiver.extend(instructions);
            receiver.push(op);
            instructions = receiver;
        }

        instructions.extend(self.compile_assign_receiver(ctx, &stmt.receiver));
        instructions
    }

    fn compile_assign_receiver(&self, ctx: &mut FnContext, expr: &Expr) -> Vec<Instruction> {
        match &expr.kind {
            ExprKind::Ident(name) => self.compile_assign_ident_expr(ctx, name),
            _ => unreachable!(),
        }
    }

    fn compile_assign_ident_expr(&self, ctx: &mut FnContext, name: &String) -> Vec<Instruction> {
        if let Some(index_type) = ctx.find_symbol(name) {
            vec![Instruction::SetLocal(index_type.index)]
        } else if let Some(index) = self.name_to_index.get(name) {
            vec![Instruction::SetGlobal(index.clone())]
        } else {
            unreachable!();
        }
    }

    fn compile_return(&self, ctx: &mut FnContext, stmt: &Return) -> Vec<Instruction> {
        let mut instructions = if let Some(value) = &stmt.value {
            self.compile_expr(ctx, value)
        } else {
            vec![Instruction::Constant(Value { kind: ValueKind::Void })]
        };

        instructions.push(Instruction::Ret);
        instructions
    }

    fn compile_if(&self, ctx: &mut FnContext, stmt: &If) -> Vec<Instruction> {
        // 1. cond
        // 2. jump_if_false (4.)
        // 3. body
        // 4. <the next instruction>

        let cond = self.compile_expr(ctx, &stmt.cond);
        let body = self.compile_statement(ctx, stmt.body.as_ref());
        let exit = Instruction::JumpIfFalse(body.len() as isize + 1);

        let mut result = vec![];
        result.extend(cond);
        result.push(exit);
        result.extend(body);

        result
    }

    fn compile_while(&self, ctx: &mut FnContext, stmt: &While) -> Vec<Instruction> {
        // 1. cond
        // 2. jump_if_false (5.)
        // 3. body
        // 4. jump (1.)
        // 5. <the next instruction>

        let cond = self.compile_expr(ctx, &stmt.cond);
        let body = self.compile_statement(ctx, stmt.body.as_ref());
        let exit = Instruction::JumpIfFalse(body.len() as isize + 1 + 1);
        let go_back = Instruction::Jump(-(body.len() as isize + 1 + cond.len() as isize));

        let mut result = vec![];
        result.extend(cond);
        result.push(exit);
        result.extend(body);
        result.push(go_back);

        result
    }

    fn compile_expr_stmt(&self, ctx: &mut FnContext, expr: &Expr) -> Vec<Instruction> {
        let mut instructions = self.compile_expr(ctx, expr);
        instructions.push(Instruction::Pop(1));
        instructions
    }

    fn compile_expr(&self, ctx: &FnContext, expr: &Expr) -> Vec<Instruction> {
        match &expr.kind {
            ExprKind::Ident(name) => self.compile_ident(ctx, name),
            ExprKind::I8(val) => vec![Instruction::Constant(Value::constant(ValueKind::I8(val.clone())))],
            ExprKind::I16(val) => vec![Instruction::Constant(Value::constant(ValueKind::I16(val.clone())))],
            ExprKind::I32(val) => vec![Instruction::Constant(Value::constant(ValueKind::I32(val.clone())))],
            ExprKind::I64(val) => vec![Instruction::Constant(Value::constant(ValueKind::I64(val.clone())))],
            ExprKind::U8(val) => vec![Instruction::Constant(Value::constant(ValueKind::U8(val.clone())))],
            ExprKind::U16(val) => vec![Instruction::Constant(Value::constant(ValueKind::U16(val.clone())))],
            ExprKind::U32(val) => vec![Instruction::Constant(Value::constant(ValueKind::U32(val.clone())))],
            ExprKind::U64(val) => vec![Instruction::Constant(Value::constant(ValueKind::U64(val.clone())))],
            ExprKind::F32(val) => vec![Instruction::Constant(Value::constant(ValueKind::F32(val.clone())))],
            ExprKind::F64(val) => vec![Instruction::Constant(Value::constant(ValueKind::F64(val.clone())))],
            ExprKind::Bool(val) => vec![Instruction::Constant(Value::constant(ValueKind::Bool(val.clone())))],
            ExprKind::Binary(binary) => self.compile_binary(ctx, binary),
            ExprKind::Unary(unary) => self.compile_unary(ctx, unary),
            ExprKind::FunctionCall(fn_call) => self.compile_func_call(ctx, fn_call),
            ExprKind::Cast(cast) => self.compile_cast(ctx, cast),
        }
    }

    fn compile_ident(&self, ctx: &FnContext, name: &String) -> Vec<Instruction> {
        if let Some(index_type) = ctx.find_symbol(name) {
            vec![Instruction::GetLocal(index_type.index)]
        } else if let Some(index) = self.name_to_index.get(name) {
            vec![Instruction::GetGlobal(index.clone())]
        } else {
            unreachable!();
        }
    }

    fn compile_binary(&self, ctx: &FnContext, binary: &Binary) -> Vec<Instruction> {
        let a = self.compile_expr(ctx, binary.a.as_ref());
        let b = self.compile_expr(ctx, binary.b.as_ref());

        if matches!(binary.op, BinOp::And) {
            // 1. a
            // 2. jump_if_true (5.)
            // 3. false
            // 4. jump (6.)
            // 5. b
            return [
                &a[..],
                &[
                    Instruction::JumpIfTrue(3),
                    Instruction::Constant(Value::constant(false.into())),
                    Instruction::Jump(b.len() as isize + 1),
                ],
                &b[..],
            ]
            .concat();
        }

        if matches!(binary.op, BinOp::Or) {
            // 1. a
            // 2. jump_if_false (5.)
            // 3. true
            // 4. jump (6.)
            // 5. b
            return [
                &a[..],
                &[
                    Instruction::JumpIfFalse(3),
                    Instruction::Constant(Value::constant(true.into())),
                    Instruction::Jump(b.len() as isize + 1),
                ],
                &b[..],
            ]
            .concat();
        }

        let (signed, size, is_float) = match binary.a.typ.as_ref() {
            Type::Int(t) => (t.signed, t.size, false),
            Type::Float(t) => (false, t.size, true),
            Type::Bool => (false, 0, false),
            _ => unreachable!(),
        };

        let ins = match (is_float, binary.op) {
            (true, BinOp::Plus) => Instruction::Add(signed, size),
            (true, BinOp::Minus) => Instruction::Sub(signed, size),
            (true, BinOp::Mul) => Instruction::Mul(signed, size),
            (true, BinOp::Div) => Instruction::Div(signed, size),
            (false, BinOp::Plus) => Instruction::AddFloat(size),
            (false, BinOp::Minus) => Instruction::SubFloat(size),
            (false, BinOp::Mul) => Instruction::MulFloat(size),
            (false, BinOp::Div) => Instruction::DivFloat(size),
            (_, BinOp::Mod) => Instruction::Mod(signed, size),
            (_, BinOp::BitAnd) => Instruction::And(size),
            (_, BinOp::BitOr) => Instruction::Or(size),
            (_, BinOp::BitXor) => Instruction::Xor(size),
            (_, BinOp::Shl) => Instruction::Shl(size),
            (_, BinOp::Shr) => Instruction::Shr(size),
            (true, BinOp::GT) => Instruction::GT(signed, size),
            (true, BinOp::LT) => Instruction::LT(signed, size),
            (true, BinOp::GTEq) => Instruction::GTEq(signed, size),
            (true, BinOp::LTEq) => Instruction::LTEq(signed, size),
            (false, BinOp::GT) => Instruction::GTFloat(size),
            (false, BinOp::LT) => Instruction::LTFloat(size),
            (false, BinOp::GTEq) => Instruction::GTEqFloat(size),
            (false, BinOp::LTEq) => Instruction::LTEqFloat(size),
            (_, BinOp::Eq) => Instruction::Eq,
            (_, BinOp::NotEq) => Instruction::NEq,
            _ => unreachable!(),
        };
        [&a[..], &b[..], &[ins]].concat()
    }

    fn compile_unary(&self, ctx: &FnContext, unary: &Unary) -> Vec<Instruction> {
        let instructions = self.compile_expr(ctx, unary.val.as_ref());

        let (signed, size, is_float) = match unary.val.typ.as_ref() {
            Type::Int(t) => (t.signed, t.size, false),
            Type::Float(t) => (false, t.size, true),
            Type::Bool => (false, 0, false),
            _ => unreachable!(),
        };

        match unary.op {
            UnaryOp::Plus => instructions,
            UnaryOp::Minus => [
                &[
                    Instruction::Constant(self.empty_value(unary.val.typ.as_ref())),
                    if is_float {
                        Instruction::SubFloat(size)
                    } else {
                        Instruction::Sub(signed, size)
                    },
                ],
                &instructions[..],
            ]
            .concat(),
            UnaryOp::BitNot => [&instructions[..], &[Instruction::Not(size)]].concat(),
            UnaryOp::Not => {
                vec![
                    Instruction::JumpIfTrue(3),
                    Instruction::Constant(Value::constant(true.into())),
                    Instruction::Jump(2),
                    Instruction::Constant(Value::constant(false.into())),
                ]
            }
        }
    }

    fn compile_func_call(&self, ctx: &FnContext, fn_call: &FunctionCall) -> Vec<Instruction> {
        let mut instructions = Vec::new();
        for arg in fn_call.args.iter() {
            instructions.extend(self.compile_expr(ctx, arg));
        }
        instructions.extend(self.compile_expr(ctx, &fn_call.func));
        instructions.push(Instruction::Call(fn_call.args.len()));

        instructions
    }

    fn compile_cast(&self, _ctx: &FnContext, _cast: &Cast) -> Vec<Instruction> {
        todo!();
    }

    fn empty_value(&self, typ: &Type) -> Value {
        match typ {
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
            Type::Fn(_) => unreachable!("cannot create empty function on the runtime."),
        }
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
    fn new(fn_decl: &FnDecl) -> Self {
        let fn_type = &fn_decl.header.fn_type;
        let mut table = HashMap::new();
        for (index, param) in fn_type.arguments.iter().rev().enumerate() {
            let name = param.name.clone();
            table.insert(name, Rc::new(Symbol { index: index as isize }));
        }

        Self {
            symbol_tables: vec![table],
            counter: fn_type.arguments.len() as isize,
        }
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
