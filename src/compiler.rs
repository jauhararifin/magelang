use std::{collections::HashMap, rc::Rc};

use crate::{
    bytecode::{Function, Instruction, Object, Value, Variant},
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
        let mut compiler = CompilerHelper::new(unit);
        compiler.compile()
    }
}

struct CompilerHelper<'a> {
    unit: &'a Unit,
    func_to_index: HashMap<Rc<String>, usize>,
    global_to_index: HashMap<Rc<String>, usize>,
}

impl<'a> CompilerHelper<'a> {
    fn new(unit: &'a Unit) -> Self {
        Self {
            unit,
            func_to_index: HashMap::new(),
            global_to_index: HashMap::new(),
        }
    }

    fn compile(&mut self) -> Object {
        let mut functions = Vec::new();

        for (id, fn_decl) in self.unit.functions.iter().enumerate() {
            let name = Rc::clone(&fn_decl.header.name.clone());
            self.func_to_index.insert(name, id);
        }

        for fn_decl in self.unit.functions.iter() {
            functions.push(self.compile_func(fn_decl));
        }

        Object { functions }
    }

    fn compile_func(&self, fn_decl: &FnDecl) -> Function {
        let mut instructions = Vec::new();
        let mut ctx = FnContext::new(&fn_decl);

        if fn_decl.header.native {
            instructions.push(Instruction::CallNative(fn_decl.header.name.clone()));
        } else {
            instructions.extend(self.compile_statement(&mut ctx, fn_decl.body.as_ref().unwrap()));
        }
        instructions.push(Instruction::Ret);

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
            self.empty_value(&var.header.typ)
        }
    }

    fn compile_assign(&self, ctx: &mut FnContext, stmt: &Assign) -> Vec<Instruction> {
        let mut instructions = self.compile_expr(ctx, &stmt.value);

        let variant = match stmt.receiver.typ.as_ref() {
            Type::Int(t) => Variant::int(t.signed, t.size),
            Type::Float(t) => Variant::float(t.size),
            _ => unreachable!(),
        };

        if !matches!(stmt.op, AssignOp::Assign) {
            let mut receiver = self.compile_expr(ctx, &stmt.receiver);
            let op = match &stmt.op {
                AssignOp::PlusAssign => Instruction::Add(variant),
                AssignOp::MinusAssign => Instruction::Sub(variant),
                AssignOp::ModAssign => Instruction::Mod(variant),

                AssignOp::DivAssign => Instruction::Div(variant),
                AssignOp::MulAssign => Instruction::Mul(variant),

                AssignOp::BitAndAssign => Instruction::And(variant),
                AssignOp::BitOrAssign => Instruction::Or(variant),
                AssignOp::BitXorAssign => Instruction::Xor(variant),
                AssignOp::ShlAssign => Instruction::Shl(variant),
                AssignOp::ShrAssign => Instruction::Shr(variant),
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
        } else if let Some(index) = self.func_to_index.get(name) {
            unreachable!("cannot assign a value to a function {} at {}", name, index);
        } else if let Some(_index) = self.global_to_index.get(name) {
            // vec![Instruction::SetGlobal(index.clone())]
            todo!("not support global variable yet");
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
            ExprKind::I8(val) => vec![Instruction::Constant((*val).into())],
            ExprKind::I16(val) => vec![Instruction::Constant((*val).into())],
            ExprKind::I32(val) => vec![Instruction::Constant((*val).into())],
            ExprKind::I64(val) => vec![Instruction::Constant((*val).into())],
            ExprKind::U8(val) => vec![Instruction::Constant((*val).into())],
            ExprKind::U16(val) => vec![Instruction::Constant((*val).into())],
            ExprKind::U32(val) => vec![Instruction::Constant((*val).into())],
            ExprKind::U64(val) => vec![Instruction::Constant((*val).into())],
            ExprKind::F32(val) => vec![Instruction::Constant((*val).into())],
            ExprKind::F64(val) => vec![Instruction::Constant((*val).into())],
            ExprKind::Bool(val) => vec![Instruction::Constant((*val).into())],
            ExprKind::Binary(binary) => self.compile_binary(ctx, binary),
            ExprKind::Unary(unary) => self.compile_unary(ctx, unary),
            ExprKind::FunctionCall(fn_call) => self.compile_func_call(ctx, fn_call),
            ExprKind::Index(_) => todo!(),
            ExprKind::Array(_) => todo!(),
            ExprKind::Cast(cast) => self.compile_cast(ctx, cast),
        }
    }

    fn compile_ident(&self, ctx: &FnContext, name: &String) -> Vec<Instruction> {
        if let Some(index_type) = ctx.find_symbol(name) {
            vec![Instruction::GetLocal(index_type.index)]
        } else if let Some(index) = self.func_to_index.get(name) {
            vec![Instruction::Constant(Value::FnId(*index))]
        } else if let Some(_index) = self.global_to_index.get(name) {
            // vec![Instruction::GetGlobal(index.clone())]
            todo!("not support global variable yet");
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
                    Instruction::Constant(false.into()),
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
                    Instruction::Constant(true.into()),
                    Instruction::Jump(b.len() as isize + 1),
                ],
                &b[..],
            ]
            .concat();
        }

        let variant = match binary.a.typ.as_ref() {
            Type::Int(t) => Variant::int(t.signed, t.size),
            Type::Float(t) => Variant::float(t.size),
            Type::Bool => Variant::U8,
            _ => unreachable!(),
        };

        let ins = match &binary.op {
            BinOp::Plus => Instruction::Add(variant),
            BinOp::Minus => Instruction::Sub(variant),

            BinOp::Mod => Instruction::Mod(variant),
            BinOp::BitAnd => Instruction::And(variant),
            BinOp::BitOr => Instruction::Or(variant),
            BinOp::BitXor => Instruction::Xor(variant),
            BinOp::Shl => Instruction::Shl(variant),
            BinOp::Shr => Instruction::Shr(variant),

            BinOp::Mul => Instruction::Mul(variant),
            BinOp::Div => Instruction::Div(variant),

            BinOp::GT => Instruction::GT(variant),
            BinOp::GTEq => Instruction::GTEq(variant),
            BinOp::LT => Instruction::LT(variant),
            BinOp::LTEq => Instruction::LTEq(variant),
            BinOp::Eq => Instruction::Eq(variant),
            BinOp::NotEq => Instruction::NEq(variant),
            _ => unreachable!(),
        };
        [&a[..], &b[..], &[ins]].concat()
    }

    fn compile_unary(&self, ctx: &FnContext, unary: &Unary) -> Vec<Instruction> {
        let instructions = self.compile_expr(ctx, unary.val.as_ref());

        let variant = match unary.val.typ.as_ref() {
            Type::Int(t) => Variant::int(t.signed, t.size),
            Type::Float(t) => Variant::float(t.size),
            Type::Bool => Variant::U8,
            _ => unreachable!(),
        };

        match unary.op {
            UnaryOp::Plus => instructions,
            UnaryOp::Minus => [
                self.empty_value(unary.val.typ.as_ref()).as_slice(),
                &[Instruction::Sub(variant)],
                &instructions[..],
            ]
            .concat(),
            UnaryOp::BitNot => [&instructions[..], &[Instruction::Not(variant)]].concat(),
            UnaryOp::Not => {
                vec![
                    Instruction::JumpIfTrue(3),
                    Instruction::Constant(true.into()),
                    Instruction::Jump(2),
                    Instruction::Constant(false.into()),
                ]
            }
        }
    }

    fn compile_func_call(&self, ctx: &FnContext, fn_call: &FunctionCall) -> Vec<Instruction> {
        let mut instructions = Vec::new();

        let fn_type = fn_call.func.typ.unwrap_func();
        if let Some(return_type) = fn_type.return_type.as_ref() {
            instructions.extend(self.empty_value(return_type));
        } else {
            instructions.push(Instruction::Constant(Value::Void)); // void
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

    fn empty_value(&self, typ: &Type) -> Vec<Instruction> {
        match typ {
            Type::Int(int_type) => match (int_type.signed, int_type.size) {
                (true, 8) => vec![Instruction::Constant(0i8.into())],
                (true, 16) => vec![Instruction::Constant(0i16.into())],
                (true, 32) => vec![Instruction::Constant(0i32.into())],
                (true, 64) => vec![Instruction::Constant(0i64.into())],
                (false, 8) => vec![Instruction::Constant(0u8.into())],
                (false, 16) => vec![Instruction::Constant(0u16.into())],
                (false, 32) => vec![Instruction::Constant(0u32.into())],
                (false, 64) => vec![Instruction::Constant(0u64.into())],
                _ => unreachable!(),
            },
            Type::Float(float_type) => match float_type.size {
                32 => vec![Instruction::Constant(0.0f32.into())],
                64 => vec![Instruction::Constant(0.0f64.into())],
                _ => unreachable!(),
            },
            Type::Bool => vec![Instruction::Constant(false.into())],
            Type::Void => vec![Instruction::Constant(Value::Void)],
            Type::Array(_) => self.empty_array_value(),
            Type::Fn(_) => unreachable!("cannot create empty function on the runtime."),
        }
    }

    fn empty_array_value(&self) -> Vec<Instruction> {
        todo!();
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
        for (index, param) in fn_type.arguments.iter().enumerate() {
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
            counter: 0,
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
