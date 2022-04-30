use std::collections::HashMap;

use crate::{
    ast::{
        Assign, Binary, BlockStatement, Declaration, Expr, ExprKind, FnDecl, FunctionCall, Return, Root, Statement,
        Type, Var, While,
    },
    bytecode::{Function, Instruction, Program, Value},
    parser,
    token::{Token, TokenKind},
};

pub trait Compiler {
    fn compile(&mut self) -> Result<Program, Error>;
}

#[derive(Debug)]
pub enum Error {
    ParseError(parser::Error),
    RedeclaredSymbol(Token),
    UndeclaredSymbol(Token),
    CannotAssignToFunction(Token),
    MissingMain,
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Self {
        Error::ParseError(err)
    }
}

// TODO (jauhararifin): refactor this into non-recursive implementation.
// TODO (jauhararifin): add type checking.
pub struct SimpleCompiler {
    root: Root,

    values: Vec<Value>,
    name_to_value_index: HashMap<String, usize>,
    value_counter: usize,

    functions: Vec<Function>,
    name_to_func_index: HashMap<String, usize>,
    function_counter: usize,
}

struct FnContext {
    symbol_tables: Vec<HashMap<String, isize>>,
    counter: isize,
}

impl FnContext {
    fn new(fn_decl: &FnDecl) -> Self {
        let mut table = HashMap::new();
        for (i, param) in fn_decl.params.iter().rev().enumerate() {
            table.insert(param.name.value.as_ref().unwrap().clone(), -(i as isize + 1));
        }

        Self {
            symbol_tables: vec![table],
            counter: 0,
        }
    }

    fn add_block(&mut self) {
        self.symbol_tables.push(HashMap::new());
    }

    fn pop_block(&mut self) -> usize {
        self.symbol_tables.pop().unwrap().len()
    }

    fn add_symbol(&mut self, name: String) {
        self.symbol_tables.last_mut().unwrap().insert(name, self.counter);
        self.counter += 1;
    }

    fn find_symbol(&self, name: &str) -> Option<isize> {
        for block in self.symbol_tables.iter().rev() {
            if let Some(index) = block.get(name) {
                return Some(index.clone());
            }
        }
        None
    }
}

impl SimpleCompiler {
    pub fn new(root: Root) -> Self {
        Self {
            root,

            values: Vec::new(),
            name_to_value_index: HashMap::new(),
            value_counter: 0,

            functions: Vec::new(),
            name_to_func_index: HashMap::new(),
            function_counter: 0,
        }
    }

    fn compile_func(&self, fn_decl: &FnDecl) -> Result<Function, Error> {
        let mut instructions = Vec::new();
        let mut ctx = FnContext::new(&fn_decl);

        instructions.extend(self.compile_block_stmt(&mut ctx, &fn_decl.body)?);

        Ok(Function {
            name: fn_decl.name.value.as_ref().unwrap().clone(),
            instructions,
        })
    }

    fn compile_statement(&self, ctx: &mut FnContext, expr: &Statement) -> Result<Vec<Instruction>, Error> {
        match expr {
            Statement::Block(stmt) => self.compile_block_stmt(ctx, stmt),
            Statement::Var(var) => self.compile_var(ctx, var),
            Statement::Assign(stmt) => self.compile_assign(ctx, stmt),
            Statement::Return(stmt) => self.compile_return(ctx, stmt),
            Statement::If(_) => todo!(),
            Statement::While(stmt) => self.compile_while(ctx, stmt),
            Statement::Expr(_) => todo!(),
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
        ctx.add_symbol(var.name.value.as_ref().unwrap().clone());
        if let Some(value) = &var.value {
            self.compile_expr(ctx, value)
        } else {
            Ok(vec![Instruction::Constant(self.empty_value(&var.typ)?)])
        }
    }

    fn compile_assign(&self, ctx: &mut FnContext, stmt: &Assign) -> Result<Vec<Instruction>, Error> {
        let mut instructions = self.compile_expr(ctx, &stmt.value)?;

        if stmt.op.kind != TokenKind::Assign {
            let mut receiver = self.compile_expr(ctx, &stmt.receiver)?;
            let op = match stmt.op.kind {
                TokenKind::PlusAssign => Instruction::Add,
                TokenKind::MinusAssign => Instruction::Sub,
                TokenKind::MulAssign => Instruction::Mul,
                TokenKind::DivAssign => Instruction::Div,
                TokenKind::ModAssign => Instruction::Mod,
                TokenKind::BitAndAssign => Instruction::And,
                TokenKind::BitOrAssign => Instruction::Or,
                TokenKind::BitXorAssign => Instruction::Xor,
                TokenKind::ShlAssign => Instruction::Shl,
                TokenKind::ShrAssign => Instruction::Shr,
                _ => panic!("got invalid assign op {:?}", stmt.op),
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
            ExprKind::Ident(token) => self.compile_assign_ident_expr(ctx, &token),
            ExprKind::IntegerLit(_) => todo!(),
            ExprKind::FloatLit(_) => todo!(),
            ExprKind::StringLit(_) => todo!(),
            ExprKind::BoolLit(_) => todo!(),
            ExprKind::Binary(_) => todo!(),
            ExprKind::Unary(_) => todo!(),
            ExprKind::FunctionCall(_) => todo!(),
            ExprKind::Cast(_) => todo!(),
            ExprKind::Selector(_) => todo!(),
        }
    }

    fn compile_assign_ident_expr(&self, ctx: &mut FnContext, token: &Token) -> Result<Vec<Instruction>, Error> {
        let name = token.value.as_ref().unwrap();
        if let Some(index) = ctx.find_symbol(name) {
            Ok(vec![Instruction::SetLocal(index)])
        } else if let Some(index) = self.name_to_value_index.get(name) {
            Ok(vec![Instruction::SetGlobal(index.clone())])
        } else if self.name_to_func_index.contains_key(name) {
            Err(Error::CannotAssignToFunction(token.clone()))
        } else {
            Err(Error::UndeclaredSymbol(token.clone()))
        }
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

    fn compile_while(&self, ctx: &mut FnContext, stmt: &While) -> Result<Vec<Instruction>, Error> {
        // 1. cond
        // 2. jump_if_false (5.)
        // 3. body
        // 4. jump (1.)
        // 5. <the next instruction>

        let cond = self.compile_expr(ctx, &stmt.cond)?;
        let body = self.compile_block_stmt(ctx, &stmt.body)?;
        let exit = Instruction::JumpIfFalse(body.len() as isize + 1 + 1);
        let go_back = Instruction::Jump(-(body.len() as isize + 1 + cond.len() as isize));

        let mut result = vec![];
        result.extend(cond);
        result.push(exit);
        result.extend(body);
        result.push(go_back);

        Ok(result)
    }

    fn compile_expr(&self, ctx: &FnContext, expr: &Expr) -> Result<Vec<Instruction>, Error> {
        match &expr.kind {
            ExprKind::Ident(token) => self.compile_ident(ctx, token),
            ExprKind::IntegerLit(token) => self.compile_int_lit(token),
            ExprKind::FloatLit(_) => todo!(),
            ExprKind::StringLit(_) => todo!(),
            ExprKind::BoolLit(_) => todo!(),
            ExprKind::Binary(binary) => self.compile_binary(ctx, binary),
            ExprKind::Unary(_) => todo!(),
            ExprKind::FunctionCall(fn_call) => self.compile_func_call(ctx, fn_call),
            ExprKind::Cast(_) => todo!(),
            ExprKind::Selector(_) => todo!(),
        }
    }

    fn compile_ident(&self, ctx: &FnContext, token: &Token) -> Result<Vec<Instruction>, Error> {
        let name = token.value.as_ref().unwrap();
        if let Some(index) = ctx.find_symbol(name) {
            Ok(vec![Instruction::GetLocal(index)])
        } else if let Some(index) = self.name_to_value_index.get(name) {
            Ok(vec![Instruction::GetGlobal(index.clone())])
        } else if let Some(index) = self.name_to_func_index.get(name) {
            Ok(vec![Instruction::Constant(Value::Fn(index.clone()))])
        } else {
            Err(Error::UndeclaredSymbol(token.clone()))
        }
    }

    fn compile_int_lit(&self, token: &Token) -> Result<Vec<Instruction>, Error> {
        let int_lit: i32 = token.value.as_ref().unwrap().parse().unwrap();
        Ok(vec![Instruction::Constant(Value::I32(int_lit))])
    }

    fn compile_binary(&self, ctx: &FnContext, binary: &Binary) -> Result<Vec<Instruction>, Error> {
        let mut instructions = self.compile_expr(ctx, binary.a.as_ref())?;
        instructions.extend(self.compile_expr(ctx, binary.b.as_ref())?);

        let ins = match binary.op.kind {
            TokenKind::Plus => todo!(),
            TokenKind::Minus => todo!(),
            TokenKind::Mul => todo!(),
            TokenKind::Div => todo!(),
            TokenKind::Mod => Instruction::Mod,
            TokenKind::BitAnd => todo!(),
            TokenKind::BitOr => todo!(),
            TokenKind::BitNot => todo!(),
            TokenKind::BitXor => todo!(),
            TokenKind::Shl => todo!(),
            TokenKind::Shr => todo!(),
            TokenKind::And => todo!(),
            TokenKind::Or => todo!(),
            TokenKind::Not => todo!(),
            TokenKind::GT => todo!(),
            TokenKind::LT => Instruction::LT,
            TokenKind::GTEq => todo!(),
            TokenKind::LTEq => todo!(),
            TokenKind::Eq => todo!(),
            TokenKind::NotEq => Instruction::NEq,
            _ => panic!(
                "got invalid operator while compiling binary expression: {:?}",
                binary.op
            ),
        };
        instructions.push(ins);

        Ok(instructions)
    }

    fn compile_func_call(&self, ctx: &FnContext, fn_call: &FunctionCall) -> Result<Vec<Instruction>, Error> {
        // let index = if let ExprKind::Ident(name) = &fn_call.func.kind {
        //     if let Some(index) = self.name_to_func_index.get(name.value.as_ref().unwrap()) {
        //         index.clone()
        //     } else {
        //         return Err(Error::UndeclaredSymbol(name.clone()));
        //     }
        // } else {
        //     todo!("closure not yet supported");
        // };

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
        Ok(match typ {
            Type::Primitive(token) => match token.kind {
                TokenKind::I8 => Value::I8(0),
                TokenKind::I16 => Value::I16(0),
                TokenKind::I32 => Value::I32(0),
                TokenKind::I64 => Value::I64(0),
                TokenKind::U8 => Value::U8(0),
                TokenKind::U16 => Value::U16(0),
                TokenKind::U32 => Value::U32(0),
                TokenKind::U64 => Value::U64(0),
                TokenKind::F32 => Value::F32(0.0),
                TokenKind::F64 => Value::F64(0.0),
                TokenKind::Bool => Value::Bool(false),
                _ => panic!("got invalid primitive type {:?}", token),
            },
            Type::Ident(_ident) => todo!(),
            Type::Struct(_strct) => todo!(),
        })
    }
}

impl Compiler for SimpleCompiler {
    fn compile(&mut self) -> Result<Program, Error> {
        for decl in self.root.declarations.iter() {
            match decl {
                Declaration::Fn(fn_decl) => {
                    let name = fn_decl.name.value.as_ref().unwrap().clone();

                    if self.name_to_func_index.contains_key(&name) {
                        return Err(Error::RedeclaredSymbol(fn_decl.name.clone()));
                    }
                    self.name_to_func_index.insert(name, self.function_counter);
                    self.function_counter += 1;
                }
                Declaration::Var(_var) => todo!(),
                Declaration::Type(_type_decl) => todo!(),
            }
        }

        for decl in self.root.declarations.iter() {
            match decl {
                Declaration::Fn(fn_decl) => {
                    let func = self.compile_func(fn_decl)?;
                    self.functions.push(func);
                }
                Declaration::Var(_var) => todo!(),
                Declaration::Type(_type_decl) => todo!(),
            }
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
