use std::{
    borrow::{Borrow, Cow},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    bytecode::{
        self,
        instruction::{self, OpCode},
        Instruction, Value,
    },
    semantic::{BlockStmt, Expr, FnDef, Program, Statement, Type, TypeKind, VarStmt, IntType},
};

pub enum Error {}

pub struct Compiler {}

impl Compiler {
    fn new() -> Self {
        Self {}
    }

    pub fn compile<'a>(&self, program: &'a Program) -> Result<bytecode::Program, Error> {
        compile(program)
    }
}

struct CompileContext<'a> {
    function_to_index: HashMap<&'a String, usize>,
    symbol_table: Vec<HashMap<String, usize>>,
    last_local_offset: usize,
}

fn compile(program: &Program) -> Result<bytecode::Program, Error> {
    let mut functions: Vec<(&String, &FnDef)> = program.functions.iter().collect();
    functions.sort_by_key(|v| v.0);

    let mut ctx = CompileContext {
        function_to_index: functions
            .iter()
            .enumerate()
            .map(|(i, (name, _))| (*name, i))
            .collect(),
        symbol_table: Vec::new(),
        last_local_offset: 0,
    };

    let result = bytecode::Program {
        types: Vec::new(), // TODO: support type alias.
        functions: functions
            .iter()
            .map(|(_, fn_def)| compile_func(&mut ctx, *fn_def))
            .collect(),
    };

    Ok(result)
}

fn compile_func(ctx: &mut CompileContext, fn_def: &FnDef) -> bytecode::Function {
    ctx.symbol_table = Vec::new();
    ctx.last_local_offset = 0;
    bytecode::Function {
        name: (&fn_def.id.as_ref().name[..]).into(),
        instruction: compile_statement(ctx, &fn_def.body),
    }
}

fn compile_statement<'a>(ctx: &mut CompileContext, statement: &Statement) -> Vec<Instruction> {
    match statement {
        Statement::Block(stmt) => compile_block_stmt(ctx, stmt),
        Statement::Assign(stmt) => unimplemented!(),
        Statement::If(stmt) => unimplemented!(),
        Statement::While(stmt) => unimplemented!(),
        Statement::Return(stmt) => unimplemented!(),
        Statement::Expr(stmt) => unimplemented!(),
        Statement::Var(stmt) => compile_var_stmt(ctx, stmt),
    }
}

fn compile_block_stmt(ctx: &mut CompileContext, statement: &BlockStmt) -> Vec<Instruction> {
    ctx.symbol_table.push(HashMap::new());
    statement
        .statements
        .iter()
        .map(|stmt| compile_statement(ctx, stmt))
        .flatten()
        .collect()
}

fn compile_var_stmt<'a>(ctx: &mut CompileContext, statement: &VarStmt) -> Vec<Instruction> {
    let var_name = statement.receiver.name.clone();
    let current_symbol_table = ctx.symbol_table.last_mut().unwrap();
    current_symbol_table.insert(var_name, ctx.last_local_offset);
    ctx.last_local_offset += 1;

    if let Some(value) = &statement.value {
        compile_expr(ctx, value)
    } else {
        vec![Instruction {
            op_code: OpCode::Const.into(),
            variant: None,
            index: None,
            offset: None,
            value: Some(build_constant_type(statement.receiver.typ.as_ref())),
        }]
    }
}

fn build_constant_type(typ: &Type) -> Value {
    match &typ.kind {
        TypeKind::Void => build_void_value(),
        TypeKind::Bool => unimplemented!(),
        TypeKind::Int(t) => build_int_value(t),
        TypeKind::Float(t) => unimplemented!(),
        TypeKind::Struct(t) => unimplemented!(),
        TypeKind::Fn(t) => unimplemented!(),
    }
}

fn build_void_value() -> Value {
    Value {
        r#type: bytecode::Type {
            kind: bytecode::r#type::TypeKind::Void as i32,
            r#struct: None,
            elem: None,
        },
        int_val: None,
        uint_val: None,
        bool_val: None,
        f32_val: None,
        f64_val: None,
        struct_val: None,
        elem: None,
    }
}

fn build_int_value(t: &IntType) -> Value {
    let kind = match (t.signed, t.size) {
        (true, 8) => bytecode::r#type::TypeKind::I8,
        (true, 16) => bytecode::r#type::TypeKind::I16,
        (true, 32) => bytecode::r#type::TypeKind::I32,
        (true, 64) => bytecode::r#type::TypeKind::I64,
        (false, 8) => bytecode::r#type::TypeKind::U8,
        (false, 16) => bytecode::r#type::TypeKind::U16,
        (false, 32) => bytecode::r#type::TypeKind::U32,
        (false, 64) => bytecode::r#type::TypeKind::U64,
        _ => panic!(),
    };

    Value {
        r#type: bytecode::Type {
            kind: kind as i32,
            r#struct: None,
            elem: None,
        },
        int_val: Some(0),
        uint_val: None,
        bool_val: None,
        f32_val: None,
        f64_val: None,
        struct_val: None,
        elem: None,
    }
}

fn compile_expr<'a>(ctx: &mut CompileContext, expr: &Expr) -> Vec<Instruction> {
    unimplemented!();
}
