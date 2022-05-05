use std::{collections::HashMap, rc::Rc};

use crate::{
    bytecode::{BitSize, Function, Instruction, Object},
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
        let mut functions = Vec::new();

        for (id, fn_decl) in self.unit.functions.iter().enumerate() {
            let name = Rc::clone(&fn_decl.header.name.clone());
            self.name_to_index.insert(name, id);
        }

        for fn_decl in self.unit.functions.iter() {
            functions.push(self.compile_func(fn_decl));
        }

        Object {
            // symbol_table: std::mem::take(&mut self.name_to_index),
            functions,
        }
    }

    fn compile_func(&self, fn_decl: &FnDecl) -> Function {
        let mut instructions = Vec::new();
        let mut ctx = FnContext::new(&fn_decl);

        if fn_decl.header.native {
            instructions.push(Instruction::CallNative(fn_decl.header.name.clone()));
            instructions.push(Instruction::Ret);
        } else {
            instructions.extend(self.compile_statement(&mut ctx, fn_decl.body.as_ref().unwrap()));
        }

        Function {
            name: fn_decl.header.name.as_ref().clone(),
            instructions,
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
            vec![self.empty_value(&var.header.typ)]
        }
    }

    fn compile_assign(&self, ctx: &mut FnContext, stmt: &Assign) -> Vec<Instruction> {
        let mut instructions = self.compile_expr(ctx, &stmt.value);

        let (signed, size, is_float) = match stmt.receiver.typ.as_ref() {
            Type::Int(t) => (t.signed, BitSize::from(t.size), false),
            Type::Float(t) => (false, BitSize::from(t.size), true),
            _ => unreachable!(),
        };

        if !matches!(stmt.op, AssignOp::Assign) {
            let mut receiver = self.compile_expr(ctx, &stmt.receiver);
            let op = match (is_float, signed, &stmt.op) {
                (false, _, AssignOp::PlusAssign) => Instruction::Add(size),
                (false, _, AssignOp::MinusAssign) => Instruction::Sub(size),
                (false, _, AssignOp::ModAssign) => Instruction::Mod(size),

                (false, true, AssignOp::DivAssign) => Instruction::SDiv(size),
                (false, false, AssignOp::DivAssign) => Instruction::Div(size),
                (false, true, AssignOp::MulAssign) => Instruction::SMul(size),
                (false, false, AssignOp::MulAssign) => Instruction::Mul(size),

                (true, _, AssignOp::PlusAssign) => Instruction::AddFloat(size),
                (true, _, AssignOp::MinusAssign) => Instruction::SubFloat(size),
                (true, _, AssignOp::MulAssign) => Instruction::MulFloat(size),
                (true, _, AssignOp::DivAssign) => Instruction::DivFloat(size),

                (false, _, AssignOp::BitAndAssign) => Instruction::And(size),
                (false, _, AssignOp::BitOrAssign) => Instruction::Or(size),
                (false, _, AssignOp::BitXorAssign) => Instruction::Xor(size),
                (false, _, AssignOp::ShlAssign) => Instruction::Shl(size),
                (false, _, AssignOp::ShrAssign) => Instruction::Shr(size),
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
        let mut instructions = Vec::new();
        if let Some(value) = &stmt.value {
            instructions.extend(self.compile_expr(ctx, value));
            instructions.push(Instruction::SetLocal(-ctx.argument_size - 1))
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
            ExprKind::I8(val) => vec![Instruction::Constant8(*val as u8)],
            ExprKind::I16(val) => vec![Instruction::Constant16(*val as u16)],
            ExprKind::I32(val) => vec![Instruction::Constant32(*val as u32)],
            ExprKind::I64(val) => vec![Instruction::Constant64(*val as u64)],
            ExprKind::U8(val) => vec![Instruction::Constant8(*val as u8)],
            ExprKind::U16(val) => vec![Instruction::Constant16(*val as u16)],
            ExprKind::U32(val) => vec![Instruction::Constant32(*val as u32)],
            ExprKind::U64(val) => vec![Instruction::Constant64(*val as u64)],
            ExprKind::F32(val) => vec![Instruction::Constant32(u32::from_be_bytes(val.to_be_bytes()))],
            ExprKind::F64(val) => vec![Instruction::Constant64(u64::from_be_bytes(val.to_be_bytes()))],
            ExprKind::Bool(val) => vec![Instruction::Constant8(*val as u8)],
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
                    Instruction::Constant8(0),
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
                    Instruction::Constant8(1),
                    Instruction::Jump(b.len() as isize + 1),
                ],
                &b[..],
            ]
            .concat();
        }

        let (signed, size, is_float) = match binary.a.typ.as_ref() {
            Type::Int(t) => (t.signed, BitSize::from(t.size), false),
            Type::Float(t) => (false, BitSize::from(t.size), true),
            Type::Bool => (false, BitSize::from(8), false),
            _ => unreachable!(),
        };

        let ins = match (is_float, signed, &binary.op) {
            (false, _, BinOp::Plus) => Instruction::Add(size),
            (true, _, BinOp::Plus) => Instruction::AddFloat(size),

            (true, _, BinOp::Minus) => Instruction::SubFloat(size),
            (false, _, BinOp::Minus) => Instruction::Sub(size),

            (_, _, BinOp::Mod) => Instruction::Mod(size),
            (_, _, BinOp::BitAnd) => Instruction::And(size),
            (_, _, BinOp::BitOr) => Instruction::Or(size),
            (_, _, BinOp::BitXor) => Instruction::Xor(size),
            (_, _, BinOp::Shl) => Instruction::Shl(size),
            (_, _, BinOp::Shr) => Instruction::Shr(size),

            (false, false, BinOp::Mul) => Instruction::Mul(size),
            (false, true, BinOp::Mul) => Instruction::SMul(size),
            (true, _, BinOp::Mul) => Instruction::MulFloat(size),

            (false, false, BinOp::Div) => Instruction::Div(size),
            (false, true, BinOp::Div) => Instruction::SDiv(size),
            (true, _, BinOp::Div) => Instruction::DivFloat(size),

            (false, false, BinOp::GT) => Instruction::GT(size),
            (false, true, BinOp::GT) => Instruction::SGT(size),
            (true, _, BinOp::GT) => Instruction::GTFloat(size),

            (false, false, BinOp::GTEq) => Instruction::GTEq(size),
            (false, true, BinOp::GTEq) => Instruction::SGTEq(size),
            (true, _, BinOp::GTEq) => Instruction::GTEqFloat(size),

            (false, false, BinOp::LT) => Instruction::LT(size),
            (false, true, BinOp::LT) => Instruction::SLT(size),
            (true, _, BinOp::LT) => Instruction::LTFloat(size),

            (false, false, BinOp::LTEq) => Instruction::LTEq(size),
            (false, true, BinOp::LTEq) => Instruction::SLTEq(size),
            (true, _, BinOp::LTEq) => Instruction::LTEqFloat(size),

            (false, _, BinOp::Eq) => Instruction::Eq(size),
            (true, _, BinOp::Eq) => Instruction::EqFloat(size),
            (false, _, BinOp::NotEq) => Instruction::NEq(size),
            (true, _, BinOp::NotEq) => Instruction::NEqFloat(size),
            _ => unreachable!(),
        };
        [&a[..], &b[..], &[ins]].concat()
    }

    fn compile_unary(&self, ctx: &FnContext, unary: &Unary) -> Vec<Instruction> {
        let instructions = self.compile_expr(ctx, unary.val.as_ref());

        let (size, is_float) = match unary.val.typ.as_ref() {
            Type::Int(t) => (BitSize::from(t.size), false),
            Type::Float(t) => (BitSize::from(t.size), true),
            Type::Bool => (BitSize::from(8), false),
            _ => unreachable!(),
        };

        match unary.op {
            UnaryOp::Plus => instructions,
            UnaryOp::Minus => [
                &[
                    self.empty_value(unary.val.typ.as_ref()),
                    if is_float {
                        Instruction::SubFloat(size)
                    } else {
                        Instruction::Sub(size)
                    },
                ],
                &instructions[..],
            ]
            .concat(),
            UnaryOp::BitNot => [&instructions[..], &[Instruction::Not(size)]].concat(),
            UnaryOp::Not => {
                vec![
                    Instruction::JumpIfTrue(3),
                    Instruction::Constant8(1),
                    Instruction::Jump(2),
                    Instruction::Constant8(0),
                ]
            }
        }
    }

    fn compile_func_call(&self, ctx: &FnContext, fn_call: &FunctionCall) -> Vec<Instruction> {
        let mut instructions = Vec::new();

        let fn_type = fn_call.func.typ.unwrap_func();
        if let Some(return_type) = fn_type.return_type.as_ref() {
            instructions.push(Instruction::from(self.empty_value(return_type)));
        } else {
            instructions.push(Instruction::Constant8(0)); // void
        }

        for arg in fn_call.args.iter().rev() {
            instructions.extend(self.compile_expr(ctx, arg));
        }

        instructions.extend(self.compile_expr(ctx, &fn_call.func));
        instructions.push(Instruction::Call);
        instructions.push(Instruction::Pop(fn_call.args.len()));
        instructions
    }

    fn compile_cast(&self, _ctx: &FnContext, _cast: &Cast) -> Vec<Instruction> {
        todo!();
    }

    fn empty_value(&self, typ: &Type) -> Instruction {
        match typ {
            Type::Int(int_type) => match (int_type.signed, int_type.size) {
                (true, 8) => Instruction::Constant8(0),
                (true, 16) => Instruction::Constant16(0),
                (true, 32) => Instruction::Constant32(0),
                (true, 64) => Instruction::Constant64(0),
                (false, 8) => Instruction::Constant8(0),
                (false, 16) => Instruction::Constant16(0),
                (false, 32) => Instruction::Constant32(0),
                (false, 64) => Instruction::Constant64(0),
                _ => unreachable!(),
            },
            Type::Float(float_type) => match float_type.size {
                32 => Instruction::Constant32(0),
                64 => Instruction::Constant64(0),
                _ => unreachable!(),
            },
            Type::Bool => Instruction::Constant8(0),
            Type::Void => Instruction::Constant8(0),
            Type::Fn(_) => unreachable!("cannot create empty function on the runtime."),
        }
    }
}

struct FnContext {
    symbol_tables: Vec<HashMap<Rc<String>, Rc<Symbol>>>,
    argument_size: isize,
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
            table.insert(
                name,
                Rc::new(Symbol {
                    index: -(index as isize) - 1,
                }),
            );
        }

        Self {
            symbol_tables: vec![table],
            argument_size: fn_type.arguments.len() as isize,
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
