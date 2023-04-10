use magelang_common::{SymbolId, SymbolLoader};
use magelang_semantic::{BinOp, Expr, ExprKind, Package, Statement, Type, TypeLoader};
use walrus::{ir::BinaryOp, FunctionBuilder, InstrSeqBuilder, LocalId, Module, ValType};

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
            ExprKind::F64(val) => {
                builder.f64_const(*val);
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
                self.process_expr(module, builder, variables, a);
                self.process_expr(module, builder, variables, b);

                let ty = self.type_loader.get_type(a.type_id).unwrap();

                enum Ty {
                    I64,
                    U64,
                    I32,
                    U32,
                    F32,
                    F64,
                }

                let ty = match ty.as_ref() {
                    Type::Int(int_ty) => match (int_ty.signed, int_ty.bitsize) {
                        (true, 32) => Ty::I32,
                        (true, 64) => Ty::I64,
                        (false, 32) => Ty::U32,
                        (false, 64) => Ty::U64,
                        _ => unreachable!(),
                    },
                    Type::Float(float_ty) => match float_ty.bitsize {
                        32 => Ty::F32,
                        64 => Ty::F64,
                        _ => unreachable!(),
                    },
                    Type::Bool => Ty::I32,
                    _ => unreachable!(),
                };

                match (op, ty) {
                    (BinOp::Add, Ty::I64 | Ty::U64) => builder.binop(BinaryOp::I64Add),
                    (BinOp::Add, Ty::I32 | Ty::U32) => builder.binop(BinaryOp::I32Add),
                    (BinOp::Add, Ty::F64) => builder.binop(BinaryOp::F64Add),
                    (BinOp::Add, Ty::F32) => builder.binop(BinaryOp::F32Add),

                    (BinOp::Sub, Ty::I64 | Ty::U64) => builder.binop(BinaryOp::I64Sub),
                    (BinOp::Sub, Ty::I32 | Ty::U32) => builder.binop(BinaryOp::I32Sub),
                    (BinOp::Sub, Ty::F64) => builder.binop(BinaryOp::F64Sub),
                    (BinOp::Sub, Ty::F32) => builder.binop(BinaryOp::F32Sub),

                    (BinOp::Mul, Ty::I64 | Ty::U64) => builder.binop(BinaryOp::I64Mul),
                    (BinOp::Mul, Ty::I32 | Ty::U32) => builder.binop(BinaryOp::I32Mul),
                    (BinOp::Mul, Ty::F64) => builder.binop(BinaryOp::F64Mul),
                    (BinOp::Mul, Ty::F32) => builder.binop(BinaryOp::F32Mul),

                    (BinOp::Div, Ty::I64) => builder.binop(BinaryOp::I64DivS),
                    (BinOp::Div, Ty::U64) => builder.binop(BinaryOp::I64DivU),
                    (BinOp::Div, Ty::I32) => builder.binop(BinaryOp::I32DivS),
                    (BinOp::Div, Ty::U32) => builder.binop(BinaryOp::I32DivU),
                    (BinOp::Div, Ty::F64) => builder.binop(BinaryOp::F64Div),
                    (BinOp::Div, Ty::F32) => builder.binop(BinaryOp::F32Div),

                    (BinOp::Mod, Ty::I64) => builder.binop(BinaryOp::I64RemS),
                    (BinOp::Mod, Ty::U64) => builder.binop(BinaryOp::I64RemU),
                    (BinOp::Mod, Ty::I32) => builder.binop(BinaryOp::I32RemS),
                    (BinOp::Mod, Ty::U32) => builder.binop(BinaryOp::I32RemU),

                    (BinOp::BitOr, Ty::I64 | Ty::U64) => builder.binop(BinaryOp::I64Or),
                    (BinOp::BitOr, Ty::I32 | Ty::U32) => builder.binop(BinaryOp::I32Or),

                    (BinOp::BitAnd, Ty::I64 | Ty::U64) => builder.binop(BinaryOp::I64And),
                    (BinOp::BitAnd, Ty::I32 | Ty::U32) => builder.binop(BinaryOp::I32And),

                    (BinOp::BitXor, Ty::I64 | Ty::U64) => builder.binop(BinaryOp::I64Xor),
                    (BinOp::BitXor, Ty::I32 | Ty::U32) => builder.binop(BinaryOp::I32Xor),

                    (BinOp::ShiftLeft, Ty::I64 | Ty::U64) => builder.binop(BinaryOp::I64Shl),
                    (BinOp::ShiftLeft, Ty::I32 | Ty::U32) => builder.binop(BinaryOp::I32Shl),
                    (BinOp::ShiftRight, Ty::I64 | Ty::U64) => builder.binop(BinaryOp::I64ShrU),
                    (BinOp::ShiftRight, Ty::I32 | Ty::U32) => builder.binop(BinaryOp::I32ShrU),

                    (BinOp::And, _) => builder.binop(BinaryOp::I32And),
                    (BinOp::Or, _) => builder.binop(BinaryOp::I32Or),

                    (BinOp::Eq, Ty::I64 | Ty::U64) => builder.binop(BinaryOp::I64Eq),
                    (BinOp::Eq, Ty::I32 | Ty::U32) => builder.binop(BinaryOp::I32Eq),
                    (BinOp::Eq, Ty::F64) => builder.binop(BinaryOp::F64Eq),
                    (BinOp::Eq, Ty::F32) => builder.binop(BinaryOp::F32Eq),

                    (BinOp::NEq, Ty::I64 | Ty::U64) => builder.binop(BinaryOp::I64Ne),
                    (BinOp::NEq, Ty::I32 | Ty::U32) => builder.binop(BinaryOp::I32Ne),
                    (BinOp::NEq, Ty::F64) => builder.binop(BinaryOp::F64Ne),
                    (BinOp::NEq, Ty::F32) => builder.binop(BinaryOp::F32Ne),

                    (BinOp::Gt, Ty::I64) => builder.binop(BinaryOp::I64GtS),
                    (BinOp::Gt, Ty::I32) => builder.binop(BinaryOp::I32GtS),
                    (BinOp::Gt, Ty::U64) => builder.binop(BinaryOp::I64GtU),
                    (BinOp::Gt, Ty::U32) => builder.binop(BinaryOp::I32GtU),
                    (BinOp::Gt, Ty::F32) => builder.binop(BinaryOp::F32Gt),
                    (BinOp::Gt, Ty::F64) => builder.binop(BinaryOp::F32Gt),

                    (BinOp::GEq, Ty::I64) => builder.binop(BinaryOp::I64GeS),
                    (BinOp::GEq, Ty::I32) => builder.binop(BinaryOp::I32GeS),
                    (BinOp::GEq, Ty::U64) => builder.binop(BinaryOp::I64GeU),
                    (BinOp::GEq, Ty::U32) => builder.binop(BinaryOp::I32GeU),
                    (BinOp::GEq, Ty::F32) => builder.binop(BinaryOp::F32Ge),
                    (BinOp::GEq, Ty::F64) => builder.binop(BinaryOp::F32Ge),

                    (BinOp::Lt, Ty::I64) => builder.binop(BinaryOp::I64LtS),
                    (BinOp::Lt, Ty::I32) => builder.binop(BinaryOp::I32LtS),
                    (BinOp::Lt, Ty::U64) => builder.binop(BinaryOp::I64LtU),
                    (BinOp::Lt, Ty::U32) => builder.binop(BinaryOp::I32LtU),
                    (BinOp::Lt, Ty::F32) => builder.binop(BinaryOp::F32Lt),
                    (BinOp::Lt, Ty::F64) => builder.binop(BinaryOp::F32Lt),

                    (BinOp::LEq, Ty::I64) => builder.binop(BinaryOp::I64LeS),
                    (BinOp::LEq, Ty::I32) => builder.binop(BinaryOp::I32LeS),
                    (BinOp::LEq, Ty::U64) => builder.binop(BinaryOp::I64LeU),
                    (BinOp::LEq, Ty::U32) => builder.binop(BinaryOp::I32LeU),
                    (BinOp::LEq, Ty::F32) => builder.binop(BinaryOp::F32Le),
                    (BinOp::LEq, Ty::F64) => builder.binop(BinaryOp::F32Le),
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
        Type::Int(int_ty) => match (int_ty.signed, int_ty.bitsize) {
            (true, 64) => ValType::I64,
            _ => todo!(),
        },
        _ => todo!(),
    }
}
