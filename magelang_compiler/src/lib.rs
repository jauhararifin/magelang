use magelang_common::{SymbolId, SymbolLoader};
use magelang_semantic::{BinOp, Expr, ExprKind, Package, Statement, Type, TypeLoader, UnOp};
use walrus::{ir::BinaryOp, ir::UnaryOp, FunctionBuilder, InstrSeqBuilder, LocalId, Module, ValType};

pub struct Compiler<'sym, 'typ> {
    symbol_loader: &'sym SymbolLoader,
    type_loader: &'typ TypeLoader,
}

impl<'sym, 'typ> Compiler<'sym, 'typ> {
    pub fn new(symbol_loader: &'sym SymbolLoader, type_loader: &'typ TypeLoader) -> Self {
        Self {
            symbol_loader,
            type_loader,
        }
    }

    pub fn compile(&self, packages: Vec<Package>, main_package: SymbolId, mut target: impl std::io::Write) {
        let mut module = Module::default();

        for pkg in packages {
            let pkg_name = self.symbol_loader.get_symbol(pkg.name).unwrap();

            for func in pkg.native_functions {
                let func_name = self.symbol_loader.get_symbol(func.function_name).unwrap();

                let mut return_type = vec![];
                if let Some(ret_type) = func.func_type.return_type {
                    let ret_type = self.type_loader.get_type(ret_type).unwrap();
                    let ret_type = to_wasm_type(&ret_type);
                    return_type = vec![ret_type];
                }

                let mut param_types = vec![];
                let mut variables = vec![];
                for param_ty in func.func_type.parameters {
                    let param_ty = self.type_loader.get_type(param_ty).unwrap();
                    let param_ty = to_wasm_type(&param_ty);
                    param_types.push(param_ty);
                    variables.push(module.locals.add(param_ty));
                }

                let type_id = module.types.add(&param_types, &return_type);
                let (func_id, _) = module.add_import_func(&pkg_name, &func_name, type_id);

                let mangled_func = mangle_func(&pkg_name, &func_name);
                let mut builder = FunctionBuilder::new(&mut module.types, &param_types, &return_type);
                builder.name(mangled_func);
                let mut body_builder = builder.func_body();
                for var in &variables {
                    body_builder.local_get(*var);
                }
                body_builder.call(func_id);
                builder.finish(variables.to_vec(), &mut module.funcs);
            }

            for func in pkg.functions {
                let func_name = self.symbol_loader.get_symbol(func.function_name).unwrap();

                let mut return_type = vec![];
                if let Some(ret_type) = func.func_type.return_type {
                    let ret_type = self.type_loader.get_type(ret_type).unwrap();
                    let ret_type = to_wasm_type(&ret_type);
                    return_type = vec![ret_type];
                }

                let mut param_types = vec![];
                let mut variables = vec![];
                for param_ty in &func.func_type.parameters {
                    let param_ty = self.type_loader.get_type(*param_ty).unwrap();
                    let param_ty = to_wasm_type(&param_ty);
                    param_types.push(param_ty);
                    variables.push(module.locals.add(param_ty));
                }

                let mangled_func = mangle_func(&pkg_name, &func_name);

                let mut builder = FunctionBuilder::new(&mut module.types, &param_types, &return_type);
                builder.name(mangled_func);
                let mut body_builder = builder.func_body();
                self.process_statement(&module, &mut body_builder, &variables, func.body);

                let function = builder.finish(
                    variables
                        .iter()
                        .take(func.func_type.parameters.len())
                        .cloned()
                        .collect(),
                    &mut module.funcs,
                );

                if main_package == pkg.name && func_name.as_ref() == "main" {
                    module.start = Some(function);
                    module.exports.add("main", function);
                }
            }
        }

        target.write_all(&module.emit_wasm()).expect("cannot write file");
    }

    fn process_statement(
        &self,
        module: &Module,
        builder: &mut InstrSeqBuilder,
        variables: &[LocalId],
        stmt: Statement,
    ) {
        match stmt {
            Statement::Block(block) => {
                for stmt in block.statements {
                    self.process_statement(module, builder, variables, stmt);
                }
            }
            Statement::Return(ret_stmt) => {
                if let Some(val) = ret_stmt.value {
                    self.process_expr(module, builder, variables, &val);
                }
                builder.return_();
            }
            Statement::Expr(expr) => {
                self.process_expr(module, builder, variables, &expr);
            }
            Statement::Invalid => unreachable!(),
        }
    }

    fn process_expr(&self, module: &Module, builder: &mut InstrSeqBuilder, variables: &[LocalId], expr: &Expr) {
        match &expr.kind {
            ExprKind::Invalid => unreachable!(),
            ExprKind::I64(val) => {
                builder.i64_const(*val);
            }
            ExprKind::I32(val) => {
                builder.i32_const(*val);
            }
            ExprKind::I16(val) => {
                builder.i32_const(*val as i32);
                builder.unop(UnaryOp::I32Extend16S);
            }
            ExprKind::I8(val) => {
                builder.i32_const(*val as i32);
                builder.unop(UnaryOp::I32Extend8S);
            }
            ExprKind::U64(val) => {
                builder.i64_const(*val as i64);
            }
            ExprKind::U32(val) => {
                builder.i32_const(*val as i32);
            }
            ExprKind::U16(val) => {
                builder.i32_const(*val as i32);
            }
            ExprKind::U8(val) => {
                builder.i32_const(*val as i32);
            }
            ExprKind::F64(val) => {
                builder.f64_const(*val);
            }
            ExprKind::F32(val) => {
                builder.f32_const(*val);
            }
            ExprKind::Bool(val) => {
                builder.i32_const(if *val { 1 } else { 0 });
            }
            ExprKind::Local(index) => {
                builder.local_get(variables[*index]);
            }
            ExprKind::Func(..) => unreachable!(),
            ExprKind::Call(target, arguments) => {
                for arg in arguments.iter() {
                    self.process_expr(module, builder, variables, arg);
                }

                if let ExprKind::Func(func_expr) = &target.kind {
                    let pkg_name = self.symbol_loader.get_symbol(func_expr.package_name).unwrap();
                    let func_name = self.symbol_loader.get_symbol(func_expr.function_name).unwrap();
                    let name = mangle_func(&pkg_name, &func_name);
                    let Some(func_id) = module.funcs.by_name(&name) else {
                        panic!("missing function {}.{}", pkg_name, func_name);
                    };
                    builder.call(func_id);
                } else {
                    todo!();
                }
            }
            ExprKind::Binary { a, op, b } => {
                if op == &BinOp::And {
                    self.process_expr(module, builder, variables, a);
                    builder.if_else(
                        ValType::I32,
                        |builder| {
                            self.process_expr(module, builder, variables, b);
                        },
                        |builder| {
                            builder.i32_const(0);
                        },
                    );
                    return;
                }

                if op == &BinOp::Or {
                    self.process_expr(module, builder, variables, a);
                    builder.if_else(
                        ValType::I32,
                        |builder| {
                            builder.i32_const(1);
                        },
                        |builder| {
                            self.process_expr(module, builder, variables, b);
                        },
                    );
                    return;
                }

                self.process_expr(module, builder, variables, a);
                self.process_expr(module, builder, variables, b);

                let ty = self.type_loader.get_type(a.type_id).unwrap();

                match (op, ty.as_ref()) {
                    (BinOp::Add, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Add),
                    (BinOp::Add, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Add),
                    (BinOp::Add, Type::F64) => builder.binop(BinaryOp::F64Add),
                    (BinOp::Add, Type::F32) => builder.binop(BinaryOp::F32Add),

                    (BinOp::Sub, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Sub),
                    (BinOp::Sub, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Sub),
                    (BinOp::Sub, Type::F64) => builder.binop(BinaryOp::F64Sub),
                    (BinOp::Sub, Type::F32) => builder.binop(BinaryOp::F32Sub),

                    (BinOp::Mul, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Mul),
                    (BinOp::Mul, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Mul),
                    (BinOp::Mul, Type::F64) => builder.binop(BinaryOp::F64Mul),
                    (BinOp::Mul, Type::F32) => builder.binop(BinaryOp::F32Mul),

                    (BinOp::Div, Type::I64) => builder.binop(BinaryOp::I64DivS),
                    (BinOp::Div, Type::U64) => builder.binop(BinaryOp::I64DivU),
                    (BinOp::Div, Type::I32) => builder.binop(BinaryOp::I32DivS),
                    (BinOp::Div, Type::U32) => builder.binop(BinaryOp::I32DivU),
                    (BinOp::Div, Type::F64) => builder.binop(BinaryOp::F64Div),
                    (BinOp::Div, Type::F32) => builder.binop(BinaryOp::F32Div),

                    (BinOp::Mod, Type::I64) => builder.binop(BinaryOp::I64RemS),
                    (BinOp::Mod, Type::U64) => builder.binop(BinaryOp::I64RemU),
                    (BinOp::Mod, Type::I32) => builder.binop(BinaryOp::I32RemS),
                    (BinOp::Mod, Type::U32) => builder.binop(BinaryOp::I32RemU),

                    (BinOp::BitOr, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Or),
                    (BinOp::BitOr, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Or),

                    (BinOp::BitAnd, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64And),
                    (BinOp::BitAnd, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32And),

                    (BinOp::BitXor, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Xor),
                    (BinOp::BitXor, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Xor),

                    (BinOp::ShiftLeft, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Shl),
                    (BinOp::ShiftLeft, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Shl),
                    (BinOp::ShiftRight, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64ShrU),
                    (BinOp::ShiftRight, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32ShrU),

                    (BinOp::Eq, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Eq),
                    (BinOp::Eq, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Eq),
                    (BinOp::Eq, Type::F64) => builder.binop(BinaryOp::F64Eq),
                    (BinOp::Eq, Type::F32) => builder.binop(BinaryOp::F32Eq),

                    (BinOp::NEq, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Ne),
                    (BinOp::NEq, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Ne),
                    (BinOp::NEq, Type::F64) => builder.binop(BinaryOp::F64Ne),
                    (BinOp::NEq, Type::F32) => builder.binop(BinaryOp::F32Ne),

                    (BinOp::Gt, Type::I64) => builder.binop(BinaryOp::I64GtS),
                    (BinOp::Gt, Type::I32) => builder.binop(BinaryOp::I32GtS),
                    (BinOp::Gt, Type::U64) => builder.binop(BinaryOp::I64GtU),
                    (BinOp::Gt, Type::U32) => builder.binop(BinaryOp::I32GtU),
                    (BinOp::Gt, Type::F32) => builder.binop(BinaryOp::F32Gt),
                    (BinOp::Gt, Type::F64) => builder.binop(BinaryOp::F32Gt),

                    (BinOp::GEq, Type::I64) => builder.binop(BinaryOp::I64GeS),
                    (BinOp::GEq, Type::I32) => builder.binop(BinaryOp::I32GeS),
                    (BinOp::GEq, Type::U64) => builder.binop(BinaryOp::I64GeU),
                    (BinOp::GEq, Type::U32) => builder.binop(BinaryOp::I32GeU),
                    (BinOp::GEq, Type::F32) => builder.binop(BinaryOp::F32Ge),
                    (BinOp::GEq, Type::F64) => builder.binop(BinaryOp::F32Ge),

                    (BinOp::Lt, Type::I64) => builder.binop(BinaryOp::I64LtS),
                    (BinOp::Lt, Type::I32) => builder.binop(BinaryOp::I32LtS),
                    (BinOp::Lt, Type::U64) => builder.binop(BinaryOp::I64LtU),
                    (BinOp::Lt, Type::U32) => builder.binop(BinaryOp::I32LtU),
                    (BinOp::Lt, Type::F32) => builder.binop(BinaryOp::F32Lt),
                    (BinOp::Lt, Type::F64) => builder.binop(BinaryOp::F32Lt),

                    (BinOp::LEq, Type::I64) => builder.binop(BinaryOp::I64LeS),
                    (BinOp::LEq, Type::I32) => builder.binop(BinaryOp::I32LeS),
                    (BinOp::LEq, Type::U64) => builder.binop(BinaryOp::I64LeU),
                    (BinOp::LEq, Type::U32) => builder.binop(BinaryOp::I32LeU),
                    (BinOp::LEq, Type::F32) => builder.binop(BinaryOp::F32Le),
                    (BinOp::LEq, Type::F64) => builder.binop(BinaryOp::F32Le),
                    _ => unreachable!(),
                };
            }
            ExprKind::Unary { op, val } => {
                let ty = self.type_loader.get_type(val.type_id).unwrap();

                match (op, ty.as_ref()) {
                    (UnOp::BitNot, Type::I64 | Type::U64) => {
                        builder.i64_const(-1);
                        self.process_expr(module, builder, variables, val);
                        builder.binop(BinaryOp::I64Xor);
                    }
                    (UnOp::BitNot, Type::I32 | Type::U32 | Type::I16 | Type::U16 | Type::I8 | Type::U8) => {
                        builder.i32_const(-1);
                        self.process_expr(module, builder, variables, val);
                        builder.binop(BinaryOp::I32Xor);
                    }
                    (UnOp::Add, _) => {
                        self.process_expr(module, builder, variables, val);
                    }
                    (UnOp::Sub, Type::I64 | Type::U64) => {
                        builder.i64_const(0);
                        self.process_expr(module, builder, variables, val);
                        builder.binop(BinaryOp::I64Sub);
                    }
                    (UnOp::Sub, Type::I32 | Type::U32 | Type::I16 | Type::U16 | Type::I8 | Type::U8) => {
                        builder.i32_const(0);
                        self.process_expr(module, builder, variables, val);
                        builder.binop(BinaryOp::I32Sub);
                    }
                    (UnOp::Sub, Type::F64) => {
                        self.process_expr(module, builder, variables, val);
                        builder.unop(UnaryOp::F64Neg);
                    }
                    (UnOp::Sub, Type::F32) => {
                        self.process_expr(module, builder, variables, val);
                        builder.unop(UnaryOp::F32Neg);
                    }
                    (UnOp::Not, _) => {
                        builder.i32_const(1);
                        self.process_expr(module, builder, variables, val);
                        builder.binop(BinaryOp::I32Xor);
                    }
                    _ => unreachable!(),
                };
            }
        }
    }
}

fn mangle_func(pkg_name: &str, func_name: &str) -> String {
    format!("{}.{}", pkg_name, func_name)
}

fn to_wasm_type(ty: &Type) -> ValType {
    match ty {
        Type::I64 | Type::U64 => ValType::I64,
        Type::I32 | Type::U32 | Type::I16 | Type::U16 | Type::I8 | Type::U8 | Type::Bool => ValType::I32,
        Type::F64 => ValType::F64,
        Type::F32 => ValType::F32,
        _ => todo!(),
    }
}
