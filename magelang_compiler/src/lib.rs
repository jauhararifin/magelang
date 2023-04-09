use magelang_common::{SymbolId, SymbolLoader};
use magelang_semantic::{Expr, ExprKind, Package, Statement, Type, TypeLoader};
use walrus::{FunctionBuilder, InstrSeqBuilder, LocalId, Module, ValType};

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
                builder.finish(variables.iter().cloned().collect(), &mut module.funcs);
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
        }
    }
}

fn mangle_func(pkg_name: &str, func_name: &str) -> String {
    format!("{}.{}", pkg_name, func_name)
}

fn to_wasm_type(ty: &Type) -> ValType {
    match ty {
        Type::Invalid => todo!(),
        Type::Void => todo!(),
        Type::Int(int_ty) => match (int_ty.signed, int_ty.bitsize) {
            (true, 64) => ValType::I64,
            _ => todo!(),
        },
        Type::Func(_) => todo!(),
    }
}
