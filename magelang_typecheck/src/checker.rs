use crate::errors::*;
use crate::scope::{Object, Scope, ScopeKind, I64};
use indexmap::IndexMap;
use magelang_common::{ErrorAccumulator, FileId, FileLoader, SymbolId, SymbolLoader};
use magelang_package::PackageUtil;
use magelang_semantic::{
    BinOp, BlockStatement, Expr, ExprKind, Func, FuncExpr, FuncType, NativeFunction, Package, ReturnStatement,
    Statement, Type, TypeDisplay, TypeId, TypeLoader,
};
use magelang_syntax::{
    parse_string_lit, AstLoader, AstNode, BinaryExprNode, BlockStatementNode, CallExprNode, ExprNode, FunctionNode,
    ImportNode, ItemNode, PackageNode, ReturnStatementNode, SignatureNode, StatementNode, Token, TokenKind,
};
use std::iter::zip;
use std::rc::Rc;

pub struct TypeChecker<'err, 'sym, 'file, 'pkg, 'ast, 'typ> {
    err_channel: &'err ErrorAccumulator,
    symbol_loader: &'sym SymbolLoader,
    file_loader: &'file FileLoader<'err>,
    ast_loader: &'ast AstLoader<'err, 'file, 'sym>,
    package_util: &'pkg PackageUtil<'err, 'file, 'sym, 'ast>,
    type_loader: &'typ TypeLoader,
}

impl<'err, 'sym, 'file, 'pkg, 'ast, 'typ> TypeChecker<'err, 'sym, 'file, 'pkg, 'ast, 'typ> {
    pub fn new(
        err_channel: &'err ErrorAccumulator,
        symbol_loader: &'sym SymbolLoader,
        file_loader: &'file FileLoader<'err>,
        ast_loader: &'ast AstLoader<'err, 'file, 'sym>,
        package_util: &'pkg PackageUtil<'err, 'file, 'sym, 'ast>,
        type_loader: &'typ TypeLoader,
    ) -> Self {
        Self {
            err_channel,
            symbol_loader,
            file_loader,
            ast_loader,
            package_util,
            type_loader,
        }
    }

    pub fn check_all(&self, main_package: SymbolId) -> Vec<Package> {
        let global_scope = Scope::global(self.type_loader, self.symbol_loader);
        let package_names = self.package_util.get_all_packages(main_package);
        let mut packages = vec![];
        for pkg_name in package_names {
            let package_checker = PackageChecker::new(
                self.err_channel,
                self.symbol_loader,
                self.file_loader,
                self.ast_loader,
                self.package_util,
                self.type_loader,
                main_package,
                pkg_name,
            );
            let package = package_checker.check_package(global_scope.clone());
            packages.push(package);
        }
        packages
    }
}

struct PackageChecker<'err, 'sym, 'file, 'pkg, 'ast, 'typ> {
    err_channel: &'err ErrorAccumulator,
    symbol_loader: &'sym SymbolLoader,
    file_loader: &'file FileLoader<'err>,
    ast_loader: &'ast AstLoader<'err, 'file, 'sym>,
    package_util: &'pkg PackageUtil<'err, 'file, 'sym, 'ast>,
    type_loader: &'typ TypeLoader,
    helper: TypeCheckHelper<'err, 'sym, 'typ>,
    main_package: SymbolId,
    package_name: SymbolId,

    functions: Vec<Func>,
    native_functions: Vec<NativeFunction>,
}

impl<'err, 'sym, 'file, 'pkg, 'ast, 'typ> PackageChecker<'err, 'sym, 'file, 'pkg, 'ast, 'typ> {
    fn new(
        err_channel: &'err ErrorAccumulator,
        symbol_loader: &'sym SymbolLoader,
        file_loader: &'file FileLoader<'err>,
        ast_loader: &'ast AstLoader<'err, 'file, 'sym>,
        package_util: &'pkg PackageUtil<'err, 'file, 'sym, 'ast>,
        type_loader: &'typ TypeLoader,
        main_package: SymbolId,
        package_name: SymbolId,
    ) -> Self {
        Self {
            err_channel,
            symbol_loader,
            file_loader,
            ast_loader,
            package_util,
            type_loader,
            helper: TypeCheckHelper::new(err_channel, symbol_loader, type_loader),
            main_package,
            package_name,
            functions: vec![],
            native_functions: vec![],
        }
    }

    fn check_package(mut self, global_scope: Rc<Scope>) -> Package {
        let path = self.package_util.get_package_path(self.package_name);
        let file_id = self.file_loader.declare_file(path);
        let ast = self.ast_loader.get_ast(file_id);
        let scope = self.get_package_scope(global_scope, self.package_name, &ast);

        for (name, items) in &ast.items {
            self.check_named_items(file_id, scope.clone(), *name, items);
        }

        if self.package_name == self.main_package {
            let main = self.symbol_loader.declare_symbol("main");
            if let Some(main) = ast.items.get(&main).and_then(|v| v.first()) {
                self.check_main_func(&scope, main);
            } else {
                self.err_channel.push(missing_main());
            }
        }

        Package {
            name: self.package_name,
            functions: self.functions,
            native_functions: self.native_functions,
        }
    }

    fn check_named_items(&mut self, file_id: FileId, package_scope: Rc<Scope>, name: SymbolId, items: &[ItemNode]) {
        let mut item_iter = items.iter();
        let first_item = item_iter.next().unwrap();
        for item in item_iter {
            let declared_at = first_item.get_span();
            let declared_at = self
                .file_loader
                .get_file(file_id)
                .expect("cannot find file")
                .get_pos(&declared_at);

            let name = self.symbol_loader.get_symbol(name).unwrap();
            self.err_channel
                .push(redeclared_symbol(name.as_ref(), declared_at, item.get_span()));
        }

        for item in items.iter() {
            self.check_item(&package_scope, name, item);
        }
    }

    fn get_package_scope(
        &self,
        global_scope: Rc<Scope>,
        package_name: SymbolId,
        package_node: &PackageNode,
    ) -> Rc<Scope> {
        let mut symbols = IndexMap::<SymbolId, Object>::new();
        for (name, items) in &package_node.items {
            let Some(first_item) = items.first() else {
                continue
            };

            let object = match first_item {
                ItemNode::Import(import_node) => {
                    let package_path = parse_string_lit(&import_node.path.value);
                    let package_path = self.symbol_loader.declare_symbol(package_path);
                    Object::Package(package_path)
                }
                ItemNode::Function(func_node) => {
                    let func_ty = self.helper.get_func_type(&global_scope, &func_node.signature);
                    let type_id = self.type_loader.declare_type(Type::Func(func_ty));
                    Object::Func(type_id)
                }
                ItemNode::NativeFunction(signature) => {
                    let func_ty = self.helper.get_func_type(&global_scope, signature);
                    let type_id = self.type_loader.declare_type(Type::Func(func_ty));
                    Object::Func(type_id)
                }
            };

            symbols.insert(*name, object);
        }

        global_scope.new_child(ScopeKind::Package(package_name), symbols)
    }

    fn check_main_func(&self, scope: &Rc<Scope>, item_node: &ItemNode) {
        let ItemNode::Function(func_node) = item_node else {
            self.err_channel.push(missing_main());
            return;
        };

        let func_type = self.helper.get_func_type(scope, &func_node.signature);
        if !func_type.parameters.is_empty() || func_type.return_type.is_some() {
            self.err_channel
                .push(invalid_main_func(func_node.signature.span.clone()));
        }
    }

    fn check_item(&mut self, scope: &Rc<Scope>, name: SymbolId, item_node: &ItemNode) {
        match item_node {
            ItemNode::Import(import_node) => self.check_import(import_node),
            ItemNode::Function(func_node) => {
                let mut func_checker =
                    FunctionChecker::new(self.err_channel, self.symbol_loader, self.type_loader, name);
                let func = func_checker.check_fn(scope, func_node);
                self.functions.push(func);
            }
            ItemNode::NativeFunction(signature) => {
                let func_type = self.helper.get_func_type(scope, signature);
                let native_func = NativeFunction {
                    package_name: scope.package_name().unwrap(),
                    function_name: name,
                    func_type,
                };
                self.native_functions.push(native_func);
            }
        }
    }

    fn check_import(&self, import_node: &ImportNode) {
        let path = parse_string_lit(&import_node.path.value);
        if path.is_empty() {
            self.err_channel.push(empty_package_path(import_node.path.span.clone()));
        }
    }
}

struct FunctionChecker<'err, 'sym, 'typ> {
    err_channel: &'err ErrorAccumulator,
    symbol_loader: &'sym SymbolLoader,
    type_loader: &'typ TypeLoader,
    helper: TypeCheckHelper<'err, 'sym, 'typ>,

    function_name: SymbolId,
    local_count: usize,
}

impl<'err, 'sym, 'typ> FunctionChecker<'err, 'sym, 'typ> {
    fn new(
        err_channel: &'err ErrorAccumulator,
        symbol_loader: &'sym SymbolLoader,
        type_loader: &'typ TypeLoader,
        function_name: SymbolId,
    ) -> Self {
        Self {
            err_channel,
            symbol_loader,
            type_loader,
            helper: TypeCheckHelper::new(err_channel, symbol_loader, type_loader),
            function_name,
            local_count: 0,
        }
    }

    fn check_fn(&mut self, scope: &Rc<Scope>, func_node: &FunctionNode) -> Func {
        let mut symbols = IndexMap::<SymbolId, Object>::new();

        let func_type = self.helper.get_func_type(scope, &func_node.signature);

        let param_types = func_type.parameters.iter();
        let param_nodes = func_node.signature.parameters.iter();
        for (param_ty, param_node) in zip(param_types, param_nodes) {
            symbols.insert(
                self.symbol_loader.declare_symbol(&param_node.name.value),
                Object::Local(*param_ty, self.local_count),
            );
            self.local_count += 1;
        }

        let func_scope = scope.new_child(ScopeKind::Function(func_type.return_type), symbols);
        let body = self.check_block_statement(&func_scope, &func_node.body);

        Func {
            package_name: scope.package_name().unwrap(),
            function_name: self.function_name,
            func_type,
            body,
        }
    }

    fn check_block_statement(&self, scope: &Rc<Scope>, node: &BlockStatementNode) -> Statement {
        let scope = scope.new_child(ScopeKind::Basic, IndexMap::new());
        let mut statements = vec![];
        for stmt in &node.statements {
            statements.push(self.check_statement(&scope, stmt));
        }
        Statement::Block(BlockStatement { statements })
    }

    fn check_statement(&self, scope: &Rc<Scope>, node: &StatementNode) -> Statement {
        match node {
            StatementNode::Block(node) => self.check_block_statement(scope, node),
            StatementNode::Return(node) => self.check_return_statement(scope, node),
            StatementNode::Expr(node) => Statement::Expr(self.helper.get_expr(scope, node)),
        }
    }

    fn check_return_statement(&self, scope: &Rc<Scope>, node: &ReturnStatementNode) -> Statement {
        if let Some(ref return_type) = scope.return_type() {
            let Some(ref expr_node) = node.value else {
                self.err_channel.push(missing_return_value(node.get_span()));
                return Statement::Invalid;
            };

            let expr = self.helper.get_expr(scope, expr_node);

            let return_type = self.type_loader.get_type(*return_type).unwrap();
            let ty = self.type_loader.get_type(expr.type_id).unwrap();
            if !return_type.is_assignable_with(&ty) {
                self.err_channel.push(type_mismatch(
                    expr_node.get_span(),
                    return_type.display(self.type_loader),
                    ty.display(self.type_loader),
                ));
                Statement::Invalid
            } else {
                Statement::Return(ReturnStatement { value: Some(expr) })
            }
        } else if let Some(val) = &node.value {
            self.err_channel.push(function_is_void(val.get_span()));
            Statement::Invalid
        } else {
            Statement::Return(ReturnStatement { value: None })
        }
    }
}

struct TypeCheckHelper<'err, 'sym, 'typ> {
    err_channel: &'err ErrorAccumulator,
    symbol_loader: &'sym SymbolLoader,
    type_loader: &'typ TypeLoader,
}

impl<'err, 'sym, 'typ> TypeCheckHelper<'err, 'sym, 'typ> {
    fn new(
        err_channel: &'err ErrorAccumulator,
        symbol_loader: &'sym SymbolLoader,
        type_loader: &'typ TypeLoader,
    ) -> Self {
        Self {
            err_channel,
            symbol_loader,
            type_loader,
        }
    }

    fn get_func_type(&self, scope: &Rc<Scope>, signature: &SignatureNode) -> FuncType {
        let mut parameters = vec![];

        for param_node in &signature.parameters {
            let param_type = self.get_expr_type(scope, &param_node.type_expr);
            parameters.push(param_type);
        }

        let return_type = signature
            .return_type
            .as_ref()
            .map(|expr| self.get_expr_type(scope, expr));

        FuncType {
            parameters,
            return_type,
        }
    }

    fn get_expr_type(&self, scope: &Rc<Scope>, expr_node: &ExprNode) -> TypeId {
        match expr_node {
            ExprNode::Ident(tok) => {
                let symbol_id = self.symbol_loader.declare_symbol(&tok.value);
                let Some(object) = scope.get(symbol_id) else {
                    self.err_channel.push(undeclared_symbol(tok.clone()));
                    return self.type_loader.declare_type(Type::Invalid);
                };
                let Some(type_id) = object.as_type() else {
                    self.err_channel.push(not_a_type(tok.span.clone()));
                    return self.type_loader.declare_type(Type::Invalid);
                };
                type_id
            }
            _ => {
                self.err_channel.push(not_a_type(expr_node.get_span()));
                self.type_loader.declare_type(Type::Invalid)
            }
        }
    }

    fn get_expr(&self, scope: &Rc<Scope>, expr_node: &ExprNode) -> Expr {
        match expr_node {
            ExprNode::Ident(tok) => self.get_ident_expr(scope, tok),
            ExprNode::IntegerLiteral(tok) => self.get_int_lit_expr(scope, tok),
            ExprNode::RealLiteral(tok) => self.get_real_lit_expr(scope, tok),
            ExprNode::Binary(binary_expr) => self.get_binary_expr(scope, binary_expr),
            ExprNode::Call(call_expr) => self.get_call_expr(scope, call_expr),
            _ => todo!(),
        }
    }

    fn get_ident_expr(&self, scope: &Rc<Scope>, tok: &Token) -> Expr {
        let symbol_id = self.symbol_loader.declare_symbol(&tok.value);
        let Some(ident_object) = scope.get(symbol_id) else {
            self.err_channel.push(undeclared_symbol(tok.clone()));
            return self.invalid_value_object();
        };

        match ident_object {
            Object::Local(type_id, index) => Expr {
                type_id,
                kind: ExprKind::Local(index),
            },
            Object::Func(type_id) => Expr {
                type_id,
                kind: ExprKind::Func(FuncExpr {
                    package_name: scope.package_name().unwrap(),
                    function_name: symbol_id,
                }),
            },
            _ => {
                self.err_channel.push(not_a_value(tok.span.clone()));
                self.invalid_value_object()
            }
        }
    }

    fn get_int_lit_expr(&self, scope: &Rc<Scope>, tok: &Token) -> Expr {
        let kind = match tok.value.parse::<i64>() {
            Ok(v) => ExprKind::I64(v),
            Err(err) => {
                self.err_channel.push(invalid_integer_literal(tok.span.clone(), err));
                ExprKind::Invalid
            }
        };

        let i64_type_id = self.symbol_loader.declare_symbol(I64);
        let i64_type_id = scope
            .get(i64_type_id)
            .and_then(|v| v.as_type())
            .expect("invalid state, i64 type should be found");

        Expr {
            type_id: i64_type_id,
            kind,
        }
    }

    fn get_real_lit_expr(&self, scope: &Rc<Scope>, tok: &Token) -> Expr {
        let kind = match tok.value.parse::<f64>() {
            Ok(v) => ExprKind::F64(v),
            Err(err) => {
                self.err_channel.push(invalid_real_literal(tok.span.clone(), err));
                ExprKind::Invalid
            }
        };

        let i64_type_id = self.symbol_loader.declare_symbol(I64);
        let i64_type_id = scope
            .get(i64_type_id)
            .and_then(|v| v.as_type())
            .expect("invalid state, f64 type should be found");

        Expr {
            type_id: i64_type_id,
            kind,
        }
    }

    fn get_binary_expr(&self, scope: &Rc<Scope>, expr: &BinaryExprNode) -> Expr {
        let a_expr = self.get_expr(scope, &expr.a);
        let b_expr = self.get_expr(scope, &expr.b);

        let op_name = match expr.op.kind {
            TokenKind::Add => "add",
            TokenKind::Sub => "sub",
            TokenKind::Mul => "mul",
            TokenKind::Div => "div",
            TokenKind::Mod => "mod",
            TokenKind::BitOr => "bitwise or",
            TokenKind::BitAnd => "bitwise and",
            TokenKind::BitXor => "bitwise xor",
            TokenKind::ShiftLeft => "shift left",
            TokenKind::ShiftRight => "shift right",
            TokenKind::And => "and",
            TokenKind::Or => "or",
            TokenKind::Eq => "eq",
            TokenKind::NEq => "neq",
            TokenKind::Gt => "gt",
            TokenKind::GEq => "geq",
            TokenKind::Lt => "lt",
            TokenKind::LEq => "leq",
            _ => unreachable!("found invalid token for binary operator"),
        };

        let a_ty = self.type_loader.get_type(a_expr.type_id).unwrap();
        let b_ty = self.type_loader.get_type(b_expr.type_id).unwrap();

        if a_expr.type_id != b_expr.type_id {
            self.err_channel.push(binop_type_mismatch(
                expr.get_span(),
                op_name,
                a_ty.display(self.type_loader),
                b_ty.display(self.type_loader),
            ));
            return Expr {
                type_id: a_expr.type_id,
                kind: ExprKind::Invalid,
            };
        }

        let result_ty = match expr.op.kind {
            TokenKind::Add
            | TokenKind::Sub
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::Eq
            | TokenKind::NEq
            | TokenKind::Gt
            | TokenKind::GEq
            | TokenKind::Lt
            | TokenKind::LEq => {
                if let Type::Int(int_ty) = a_ty.as_ref() {
                    Type::Int(*int_ty)
                } else if let Type::Float(real_ty) = a_ty.as_ref() {
                    Type::Float(*real_ty)
                } else {
                    self.err_channel.push(binop_type_unsupported(
                        expr.get_span(),
                        op_name,
                        a_ty.display(self.type_loader),
                    ));
                    Type::Invalid
                }
            }
            TokenKind::Mod
            | TokenKind::BitOr
            | TokenKind::BitAnd
            | TokenKind::BitXor
            | TokenKind::ShiftLeft
            | TokenKind::ShiftRight => {
                if let Type::Int(int_ty) = a_ty.as_ref() {
                    Type::Int(*int_ty)
                } else {
                    self.err_channel.push(binop_type_unsupported(
                        expr.get_span(),
                        op_name,
                        a_ty.display(self.type_loader),
                    ));
                    Type::Invalid
                }
            }
            TokenKind::And | TokenKind::Not | TokenKind::Or => {
                if let Type::Bool = a_ty.as_ref() {
                    Type::Bool
                } else {
                    self.err_channel.push(binop_type_unsupported(
                        expr.get_span(),
                        op_name,
                        a_ty.display(self.type_loader),
                    ));
                    Type::Invalid
                }
            }
            _ => todo!(),
        };

        let result_type_id = self.type_loader.declare_type(result_ty);
        let op = match expr.op.kind {
            TokenKind::Add => BinOp::Add,
            TokenKind::Sub => BinOp::Sub,
            TokenKind::Mul => BinOp::Mul,
            TokenKind::Div => BinOp::Div,
            TokenKind::Mod => BinOp::Mod,
            TokenKind::BitOr => BinOp::BitOr,
            TokenKind::BitAnd => BinOp::BitAnd,
            TokenKind::BitXor => BinOp::BitXor,
            TokenKind::ShiftLeft => BinOp::ShiftLeft,
            TokenKind::ShiftRight => BinOp::ShiftRight,
            TokenKind::And => BinOp::And,
            TokenKind::Or => BinOp::Or,
            TokenKind::Eq => BinOp::Eq,
            TokenKind::NEq => BinOp::NEq,
            TokenKind::Gt => BinOp::Gt,
            TokenKind::GEq => BinOp::GEq,
            TokenKind::Lt => BinOp::Lt,
            TokenKind::LEq => BinOp::LEq,
            _ => unreachable!("found invalid token for binary operator"),
        };

        Expr {
            type_id: result_type_id,
            kind: ExprKind::Binary {
                a: Box::new(a_expr),
                op,
                b: Box::new(b_expr),
            },
        }
    }

    fn get_call_expr(&self, scope: &Rc<Scope>, call_expr: &CallExprNode) -> Expr {
        let func_expr = self.get_expr(scope, &call_expr.target);
        let ty = self.type_loader.get_type(func_expr.type_id).unwrap();
        let Type::Func(func_type) = ty.as_ref() else {
            self.err_channel.push(not_a_func(call_expr.target.get_span()));
            return self.invalid_value_object();
        };

        if call_expr.arguments.len() != func_type.parameters.len() {
            self.err_channel.push(unmatch_function_arguments(
                call_expr.span.clone(),
                func_type.parameters.len(),
                call_expr.arguments.len(),
            ));
            return self.invalid_value_object();
        }

        let mut arguments = vec![];
        for (param, arg) in zip(func_type.parameters.iter(), call_expr.arguments.iter()) {
            let param_ty = self.type_loader.get_type(*param).unwrap();

            let arg_expr = self.get_expr(scope, arg);
            let arg_ty = self.type_loader.get_type(arg_expr.type_id).unwrap();

            if !param_ty.is_assignable_with(&arg_ty) {
                self.err_channel.push(type_mismatch(
                    arg.get_span(),
                    param_ty.display(self.type_loader),
                    arg_ty.display(self.type_loader),
                ));
                return self.invalid_value_object();
            }

            arguments.push(arg_expr);
        }

        let ret_type = if let Some(ret_ty) = func_type.return_type {
            ret_ty
        } else {
            self.type_loader.declare_type(Type::Void)
        };

        Expr {
            type_id: ret_type,
            kind: ExprKind::Call(Box::new(func_expr), arguments),
        }
    }

    fn invalid_value_object(&self) -> Expr {
        Expr {
            type_id: self.type_loader.declare_type(Type::Invalid),
            kind: ExprKind::Invalid,
        }
    }
}
