use std::{collections::HashMap, rc::Rc};

use crate::{
    bytecode::{Function, Instruction, Object},
    semantic::{
        Array, Assign, AssignOp, BinOp, Binary, BlockStatement, Cast, Expr, ExprKind, FloatType, FnDecl, FunctionCall,
        If, Index, IntType, Return, Statement, Type, Unary, UnaryOp, Unit, Var, While,
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

        let variant = Variant::from(stmt.receiver.typ.as_ref());

        if !matches!(stmt.op, AssignOp::Assign) {
            let mut receiver = self.compile_expr(ctx, &stmt.receiver);
            let op = match (&stmt.op, variant) {
                (AssignOp::PlusAssign, Variant::Uint | Variant::Int) => Instruction::AddI64,
                (AssignOp::PlusAssign, Variant::Float) => Instruction::AddF32,
                (AssignOp::PlusAssign, Variant::Double) => Instruction::AddF64,

                (AssignOp::MinusAssign, Variant::Uint | Variant::Int) => Instruction::SubI64,
                (AssignOp::MinusAssign, Variant::Float) => Instruction::SubF32,
                (AssignOp::MinusAssign, Variant::Double) => Instruction::SubF64,

                (AssignOp::ModAssign, Variant::Uint) => Instruction::ModI64,
                (AssignOp::ModAssign, Variant::Int) => Instruction::SModI64,

                (AssignOp::MulAssign, Variant::Uint) => Instruction::MulI64,
                (AssignOp::MulAssign, Variant::Int) => Instruction::SMulI64,
                (AssignOp::MulAssign, Variant::Float) => Instruction::MulF32,
                (AssignOp::MulAssign, Variant::Double) => Instruction::MulF64,

                (AssignOp::DivAssign, Variant::Uint) => Instruction::DivI64,
                (AssignOp::DivAssign, Variant::Int) => Instruction::SDivI64,
                (AssignOp::DivAssign, Variant::Float) => Instruction::DivF32,
                (AssignOp::DivAssign, Variant::Double) => Instruction::DivF64,

                (AssignOp::BitAndAssign, Variant::Uint | Variant::Int) => Instruction::AndI64,
                (AssignOp::BitOrAssign, Variant::Uint | Variant::Int) => Instruction::OrI64,
                (AssignOp::BitXorAssign, Variant::Uint | Variant::Int) => Instruction::XorI64,

                (AssignOp::ShlAssign, Variant::Uint | Variant::Int) => Instruction::ShlI64,
                (AssignOp::ShrAssign, Variant::Uint) => Instruction::ShrI64,
                (AssignOp::ShrAssign, Variant::Int) => Instruction::SShrI64,
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
            ExprKind::Index(index) => self.compile_assign_index_expr(ctx, index),
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

    fn compile_assign_index_expr(&self, ctx: &FnContext, index: &Index) -> Vec<Instruction> {
        let instructions = self.compile_array_index(ctx, index);

        let array_type = if let Type::Array(typ) = index.array.typ.as_ref() {
            typ
        } else {
            unreachable!();
        };

        let variant = Variant::from(array_type.elem_type.as_ref());
        let set_op = match variant {
            Variant::Uint | Variant::Int => Instruction::ArraySetI64,
            Variant::Float => Instruction::ArraySetF32,
            Variant::Double => Instruction::ArraySetF64,
            Variant::Array(_) => todo!("multi dimensional array is not supported yet"),
        };

        [&instructions[..], &[set_op]].concat()
    }

    fn compile_array_index(&self, ctx: &FnContext, index: &Index) -> Vec<Instruction> {
        if index.index.len() > 1 {
            todo!("multi dimensional array is not supported yet");
        }

        let array_expr = self.compile_expr(ctx, index.array.as_ref());
        let index_expr = self.compile_expr(ctx, &index.index[0]);

        [&array_expr[..], &index_expr[..]].concat()
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
            ExprKind::Invalid => unreachable!(),
            ExprKind::Ident(name) => self.compile_ident(ctx, name),
            ExprKind::I8(val) => vec![Instruction::ConstI64((*val) as u64)],
            ExprKind::I16(val) => vec![Instruction::ConstI64((*val) as u64)],
            ExprKind::I32(val) => vec![Instruction::ConstI64((*val) as u64)],
            ExprKind::I64(val) => vec![Instruction::ConstI64((*val) as u64)],
            ExprKind::U8(val) => vec![Instruction::ConstI64((*val) as u64)],
            ExprKind::U16(val) => vec![Instruction::ConstI64((*val) as u64)],
            ExprKind::U32(val) => vec![Instruction::ConstI64((*val) as u64)],
            ExprKind::U64(val) => vec![Instruction::ConstI64((*val) as u64)],
            ExprKind::F32(val) => vec![Instruction::ConstF32((*val) as f32)],
            ExprKind::F64(val) => vec![Instruction::ConstF64((*val) as f64)],
            ExprKind::Bool(val) => vec![Instruction::ConstI64((*val) as u64)],
            ExprKind::Binary(binary) => self.compile_binary(ctx, binary),
            ExprKind::Unary(unary) => self.compile_unary(ctx, unary),
            ExprKind::FunctionCall(fn_call) => self.compile_func_call(ctx, fn_call),
            ExprKind::Index(index) => self.compile_index(ctx, index),
            ExprKind::Array(array) => self.compile_array(ctx, array),
            ExprKind::Cast(cast) => self.compile_cast(ctx, cast),
        }
    }

    fn compile_ident(&self, ctx: &FnContext, name: &String) -> Vec<Instruction> {
        if let Some(index_type) = ctx.find_symbol(name) {
            vec![Instruction::GetLocal(index_type.index)]
        } else if let Some(index) = self.func_to_index.get(name) {
            vec![Instruction::ConstI64(*index as u64)]
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
                    Instruction::ConstI64(0),
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
                    Instruction::ConstI64(1),
                    Instruction::Jump(b.len() as isize + 1),
                ],
                &b[..],
            ]
            .concat();
        }

        let variant = Variant::from(binary.a.typ.as_ref());
        let ins = match (&binary.op, variant) {
            (BinOp::Plus, Variant::Uint | Variant::Int) => Instruction::AddI64,
            (BinOp::Plus, Variant::Float) => Instruction::AddF32,
            (BinOp::Plus, Variant::Double) => Instruction::AddF64,

            (BinOp::Minus, Variant::Uint | Variant::Int) => Instruction::SubI64,
            (BinOp::Minus, Variant::Float) => Instruction::SubF32,
            (BinOp::Minus, Variant::Double) => Instruction::SubF64,

            (BinOp::Mod, Variant::Uint) => Instruction::ModI64,
            (BinOp::Mod, Variant::Int) => Instruction::SModI64,

            (BinOp::Mul, Variant::Uint) => Instruction::MulI64,
            (BinOp::Mul, Variant::Int) => Instruction::SMulI64,
            (BinOp::Mul, Variant::Float) => Instruction::MulF32,
            (BinOp::Mul, Variant::Double) => Instruction::MulF64,

            (BinOp::Div, Variant::Uint) => Instruction::DivI64,
            (BinOp::Div, Variant::Int) => Instruction::SDivI64,
            (BinOp::Div, Variant::Float) => Instruction::DivF32,
            (BinOp::Div, Variant::Double) => Instruction::DivF64,

            (BinOp::BitAnd, Variant::Uint | Variant::Int) => Instruction::AndI64,
            (BinOp::BitOr, Variant::Uint | Variant::Int) => Instruction::OrI64,
            (BinOp::BitXor, Variant::Uint | Variant::Int) => Instruction::XorI64,

            (BinOp::Shl, Variant::Uint | Variant::Int) => Instruction::ShlI64,
            (BinOp::Shr, Variant::Uint) => Instruction::ShrI64,
            (BinOp::Shr, Variant::Int) => Instruction::SShrI64,

            (BinOp::Eq, Variant::Uint | Variant::Int) => Instruction::EqI64,
            (BinOp::Eq, Variant::Float) => Instruction::EqF32,
            (BinOp::Eq, Variant::Double) => Instruction::EqF64,

            (BinOp::NotEq, Variant::Uint | Variant::Int) => Instruction::NEqI64,
            (BinOp::NotEq, Variant::Float) => Instruction::NEqF32,
            (BinOp::NotEq, Variant::Double) => Instruction::NEqF64,

            (BinOp::GT, Variant::Uint | Variant::Int) => Instruction::GTI64,
            (BinOp::GT, Variant::Float) => Instruction::GTF32,
            (BinOp::GT, Variant::Double) => Instruction::GTF64,

            (BinOp::GTEq, Variant::Uint | Variant::Int) => Instruction::GTEqI64,
            (BinOp::GTEq, Variant::Float) => Instruction::GTEqF32,
            (BinOp::GTEq, Variant::Double) => Instruction::GTEqF64,

            (BinOp::LT, Variant::Uint | Variant::Int) => Instruction::LTI64,
            (BinOp::LT, Variant::Float) => Instruction::LTF32,
            (BinOp::LT, Variant::Double) => Instruction::LTF64,

            (BinOp::LTEq, Variant::Uint | Variant::Int) => Instruction::LTEqI64,
            (BinOp::LTEq, Variant::Float) => Instruction::LTEqF32,
            (BinOp::LTEq, Variant::Double) => Instruction::LTEqF64,
            _ => unreachable!(),
        };
        [&a[..], &b[..], &[ins]].concat()
    }

    fn compile_unary(&self, ctx: &FnContext, unary: &Unary) -> Vec<Instruction> {
        let instructions = self.compile_expr(ctx, unary.val.as_ref());

        let variant = Variant::from(unary.val.typ.as_ref());

        match unary.op {
            UnaryOp::Plus => instructions,
            UnaryOp::Minus => [
                self.empty_value(unary.val.typ.as_ref()).as_slice(),
                &[match variant {
                    Variant::Int | Variant::Uint => Instruction::SubI64,
                    Variant::Float => Instruction::SubF32,
                    Variant::Double => Instruction::SubF64,
                    _ => unreachable!(),
                }],
                &instructions[..],
            ]
            .concat(),
            UnaryOp::BitNot => [&instructions[..], &[Instruction::NotI64]].concat(),
            UnaryOp::Not => {
                vec![
                    Instruction::JumpIfTrue(3),
                    Instruction::ConstI64(1), // true
                    Instruction::Jump(2),
                    Instruction::ConstI64(0), // false
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
            instructions.push(Instruction::ConstI64(0)); // void
        }

        for arg in fn_call.args.iter().rev() {
            instructions.extend(self.compile_expr(ctx, arg));
        }

        instructions.extend(self.compile_expr(ctx, &fn_call.func));
        instructions.push(Instruction::Call);
        instructions.push(Instruction::Pop(fn_call.args.len()));
        instructions
    }

    fn compile_index(&self, ctx: &FnContext, index: &Index) -> Vec<Instruction> {
        let instructions = self.compile_array_index(ctx, index);

        let array_type = if let Type::Array(typ) = index.array.typ.as_ref() {
            typ
        } else {
            unreachable!();
        };

        let variant = Variant::from(array_type.elem_type.as_ref());
        let get_op = match variant {
            Variant::Uint | Variant::Int => Instruction::ArrayGetI64,
            Variant::Float => Instruction::ArrayGetF32,
            Variant::Double => Instruction::ArrayGetF64,
            Variant::Array(_) => todo!("multi dimensional array is not supported yet"),
        };

        [&instructions[..], &[get_op]].concat()
    }

    fn compile_array(&self, ctx: &FnContext, array: &Array) -> Vec<Instruction> {
        if array.size.len() != 1 {
            todo!("multi dimensional array is not supported yet");
        }

        let mut instructions = self.compile_expr(ctx, &array.size[0]);

        let variant = Variant::from(array.elem_type.as_ref());
        instructions.push(match variant {
            Variant::Int | Variant::Uint => Instruction::AllocArrayI64,
            Variant::Float => Instruction::AllocArrayF32,
            Variant::Double => Instruction::AllocArrayF64,
            Variant::Array(_) => todo!("multi dimensional array is not supported yet"),
        });

        instructions
    }

    fn compile_cast(&self, ctx: &FnContext, cast: &Cast) -> Vec<Instruction> {
        let mut instructions = self.compile_expr(ctx, cast.val.as_ref());

        let source_variant = Variant::from(cast.val.typ.as_ref());
        let target_variant = Variant::from(cast.target.as_ref());

        let instruction = match (source_variant, target_variant) {
            (Variant::Float, Variant::Uint) => Instruction::ConvertF32ToI64,
            (Variant::Float, Variant::Int) => Instruction::SConvertF32ToI64,
            (Variant::Float, Variant::Double) => Instruction::ConvertF32ToF64,

            (Variant::Double, Variant::Uint) => Instruction::ConvertF64ToI64,
            (Variant::Double, Variant::Int) => Instruction::SConvertF64ToI64,
            (Variant::Double, Variant::Float) => Instruction::ConvertF64ToF32,

            (Variant::Uint, Variant::Float) => Instruction::ConvertI64ToF32,
            (Variant::Uint, Variant::Double) => Instruction::ConvertI64ToF64,
            (Variant::Int, Variant::Float) => Instruction::SConvertI64ToF32,
            (Variant::Int, Variant::Double) => Instruction::SConvertI64ToF64,
            _ => Instruction::Nop,
        };
        instructions.push(instruction);

        instructions
    }

    fn empty_value(&self, typ: &Type) -> Vec<Instruction> {
        match typ {
            Type::Invalid => unreachable!(),
            Type::Int(_) | Type::Bool | Type::Void => vec![Instruction::ConstI64(0)],
            Type::Float(float_type) => match float_type.size {
                32 => vec![Instruction::ConstF32(0f32)],
                64 => vec![Instruction::ConstF64(0f64)],
                _ => unreachable!(),
            },
            Type::Array(_) => todo!(), // vec![Instruction::Constant(Value::Ptr(0))],
            Type::Fn(_) => unreachable!("cannot create empty function on the runtime."),
        }
    }
}

struct FnContext {
    symbol_tables: Vec<HashMap<Rc<String>, Rc<Symbol>>>,
    counter_table: Vec<isize>,
    argument_size: isize,
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
            counter_table: vec![0],
            argument_size: fn_type.arguments.len() as isize,
        }
    }

    fn add_block(&mut self) {
        let last_counter = *self.counter_table.last().unwrap();
        self.counter_table.push(last_counter);
        self.symbol_tables.push(HashMap::new());
    }

    fn pop_block(&mut self) -> usize {
        self.counter_table.pop();
        self.symbol_tables.pop().unwrap().len()
    }

    fn add_symbol(&mut self, name: Rc<String>) {
        let counter = self.counter_table.last_mut().unwrap();
        self.symbol_tables
            .last_mut()
            .unwrap()
            .insert(name, Rc::new(Symbol { index: *counter }));
        *counter += 1;
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

#[derive(Debug)]
enum Variant {
    Uint,
    Int,
    Float,
    Double,
    Array(Box<Variant>),
}

impl From<&Type> for Variant {
    fn from(typ: &Type) -> Self {
        match &typ {
            Type::Int(IntType { signed: true, size: _ }) => Variant::Int,
            Type::Int(IntType { signed: false, size: _ }) => Variant::Uint,
            Type::Float(FloatType { size: 32 }) => Variant::Float,
            Type::Float(FloatType { size: 64 }) => Variant::Double,
            Type::Array(array_type) => Variant::Array(Box::new(Variant::from(array_type.elem_type.as_ref()))),
            _ => unreachable!(),
        }
    }
}
