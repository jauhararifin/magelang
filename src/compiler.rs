use std::collections::HashMap;

use crate::{
    bytecode::{Instruction, Program, Value},
    errors::Error,
    mangler::mangle_function,
    parser,
    semantic::{
        Assign, AssignOp, BinOp, Binary, BlockStatement, Expr, ExprKind, FnDecl, FunctionCall, If, Name, Return,
        Selector, Statement, Type, Unit, Var, While,
    },
};

pub trait Compiler {
    fn compile(&mut self) -> Result<Program, Error>;
}

// #[derive(Debug)]
// pub enum Error {
//     ParseError(parser::Error),
//     RedeclaredSymbol(Token),
//     UndeclaredSymbol(Token),
//     CannotAssignToFunction(Token),
//     ReturnOnVoidFunc(Pos),
//     MissingReturnValue(Pos),
//     MissingMain,
// }

// impl From<parser::Error> for Error {
//     fn from(err: parser::Error) -> Self {
//         Error::ParseError(err)
//     }
// }

// TODO (jauhararifin): refactor this into non-recursive implementation.
pub struct SimpleCompiler {
    unit: Unit,

    values: Vec<Value>,
    name_to_value_index: HashMap<String, usize>,
    // value_counter: usize,
    functions: Vec<Value>,
    name_to_func_index: HashMap<String, usize>,
    function_counter: usize,
}

impl SimpleCompiler {
    pub fn new(unit: Unit) -> Self {
        Self {
            unit,

            values: Vec::new(),
            name_to_value_index: HashMap::new(),
            // value_counter: 0,

            functions: Vec::new(),
            name_to_func_index: HashMap::new(),
            function_counter: 0,
        }
    }

    fn compile_func(&self, fn_decl: &FnDecl) -> Result<Value, Error> {
        let mut instructions = Vec::new();
        let mut ctx = FnContext::new(&fn_decl)?;

        if fn_decl.header.native {
            instructions.push(Instruction::CallNative(mangle_function(&fn_decl.header.name)));
        } else {
            instructions.extend(self.compile_statement(&mut ctx, fn_decl.body.as_ref().unwrap())?);
        }

        Ok(Value::Fn(instructions))
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
            Statement::Continue => todo!(),
            Statement::Break => todo!(),
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
        let name = var.header.clone();
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
            ExprKind::Selector(selector) => self.compile_assign_selector_expr(ctx, &selector),
            _ => unreachable!(),
        }
    }

    fn compile_assign_ident_expr(&self, ctx: &mut FnContext, name: &String) -> Result<Vec<Instruction>, Error> {
        if let Some(index_type) = ctx.find_symbol(name) {
            Ok(vec![Instruction::SetLocal(index_type.index)])
        } else if let Some(index) = self.name_to_value_index.get(name) {
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
            vec![]
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
            ExprKind::I8(val) => Ok(vec![Instruction::Constant(Value::I8(val.clone()))]),
            ExprKind::I16(val) => Ok(vec![Instruction::Constant(Value::I16(val.clone()))]),
            ExprKind::I32(val) => Ok(vec![Instruction::Constant(Value::I32(val.clone()))]),
            ExprKind::I64(val) => Ok(vec![Instruction::Constant(Value::I64(val.clone()))]),
            ExprKind::U8(val) => Ok(vec![Instruction::Constant(Value::U8(val.clone()))]),
            ExprKind::U16(val) => Ok(vec![Instruction::Constant(Value::U16(val.clone()))]),
            ExprKind::U32(val) => Ok(vec![Instruction::Constant(Value::U32(val.clone()))]),
            ExprKind::U64(val) => Ok(vec![Instruction::Constant(Value::U64(val.clone()))]),
            ExprKind::F32(val) => Ok(vec![Instruction::Constant(Value::F32(val.clone()))]),
            ExprKind::F64(val) => Ok(vec![Instruction::Constant(Value::F64(val.clone()))]),
            ExprKind::Bool(val) => Ok(vec![Instruction::Constant(Value::Bool(val.clone()))]),
            ExprKind::String(_) => todo!(),
            ExprKind::Struct(_) => todo!(),
            ExprKind::Binary(binary) => self.compile_binary(ctx, binary),
            ExprKind::Unary(_) => todo!(),
            ExprKind::FunctionCall(fn_call) => self.compile_func_call(ctx, fn_call),
            ExprKind::Cast(_) => todo!(),
            ExprKind::Selector(_) => todo!(),
        }
    }

    fn compile_ident(&self, ctx: &FnContext, name: &String) -> Result<Vec<Instruction>, Error> {
        if let Some(index_type) = ctx.find_symbol(name) {
            Ok(vec![Instruction::GetLocal(index_type.index)])
        } else if let Some(index) = self.name_to_value_index.get(name) {
            Ok(vec![Instruction::GetGlobal(index.clone())])
        } else if let Some(index) = self.name_to_func_index.get(name) {
            Ok(vec![Instruction::Constant(Value::Fn(index.clone()))])
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
        instructions.push(Instruction::Call);
        instructions.push(Instruction::Pop(fn_call.args.len()));

        Ok(instructions)
    }

    fn empty_value(&self, typ: &Type) -> Result<Value, Error> {
        Ok(match &*typ.borrow() {
            ConcreteType::Int(int_type) => match (int_type.signed, int_type.size) {
                (true, 8) => Value::I8(0),
                (true, 16) => Value::I16(0),
                (true, 32) => Value::I32(0),
                (true, 64) => Value::I64(0),
                (false, 8) => Value::U8(0),
                (false, 16) => Value::U16(0),
                (false, 32) => Value::U32(0),
                (false, 64) => Value::U64(0),
                _ => unreachable!(),
            },
            ConcreteType::Float(float_type) => match float_type.size {
                32 => Value::F32(0.0),
                64 => Value::F64(0.0),
                _ => unreachable!(),
            },
            ConcreteType::Bool => Value::Bool(false),
            ConcreteType::Void => Value::Void,
            ConcreteType::Fn(_) => todo!(),
            ConcreteType::Struct(_) => todo!(),
            _ => unreachable!(),
        })
    }
}

impl Compiler for SimpleCompiler {
    fn compile(&mut self) -> Result<Program, Error> {
        for var_decl in self.unit.var_declarations.iter() {
            var_decl.header.name
        }

        for fn_decl in self.unit.fn_declarations.iter() {
            let name = fn_decl.header.name.clone();
            self.name_to_func_index.insert(name, self.function_counter);
            self.function_counter += 1;

            let func = self.compile_func(fn_decl)?;
            self.functions.push(func);
        }

        let main_func = self.name_to_func_index.get("main").ok_or(Error::MissingMain)?;

        Ok(Program {
            executable: true,
            values: std::mem::take(&mut self.values),
            functions: std::mem::take(&mut self.functions),
            entry_point: main_func.clone(),
        })
    }
}

struct FnContext {
    symbol_tables: Vec<HashMap<Name, Symbol>>,
    counter: isize,
}

#[derive(Debug, Clone)]
struct Symbol {
    index: isize,
}

impl FnContext {
    fn new(fn_decl: &FnDecl) -> Result<Self, Error> {
        let fn_type = fn_decl.header.typ;
        let mut table = HashMap::new();
        for (i, param) in fn_type.arguments.iter().rev().enumerate() {
            let name = param.name.clone();
            let index = -(i as isize + 1);
            table.insert(name, Symbol { index });
        }

        Ok(Self {
            symbol_tables: vec![table],
            counter: 0,
        })
    }

    fn add_block(&mut self) {
        self.symbol_tables.push(HashMap::new());
    }

    fn pop_block(&mut self) -> usize {
        self.symbol_tables.pop().unwrap().len()
    }

    fn add_symbol(&mut self, name: String) {
        let index = self.counter;
        self.symbol_tables.last_mut().unwrap().insert(name, Symbol { index });
        self.counter += 1;
    }

    fn find_symbol(&self, name: &str) -> Option<Symbol> {
        for block in self.symbol_tables.iter().rev() {
            if let Some(index) = block.get(name) {
                return Some(index.clone());
            }
        }
        None
    }
}
