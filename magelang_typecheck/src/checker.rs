use crate::errors::*;
use crate::scope::{Object, Scope, ScopeKind, BOOL, F64, I16, I32, I64, I8, ISIZE, U16, U32, U64, U8, USIZE};
use crate::value::parse_string_tok;
use indexmap::IndexMap;
use magelang_common::{ErrorAccumulator, FileId, FileLoader, SymbolId, SymbolLoader};
use magelang_package::PackageUtil;
use magelang_semantic::{
    BinOp, BlockStatement, Expr, ExprKind, Func, FuncExpr, FuncType, IfStatement, NativeFunction, Package, PointerType,
    ReturnStatement, SliceType, Statement, StringLitExpr, Tag, Type, TypeDisplay, TypeId, TypeLoader, UnOp,
    WhileStatement,
};
use magelang_syntax::{
    AssignStatementNode, AstLoader, AstNode, BinaryExprNode, BlockStatementNode, CallExprNode, CastExprNode,
    DerefExprNode, ElseIfStatementNode, ExprNode, FunctionNode, IfStatementNode, ImportNode, IndexExprNode, ItemNode,
    LetKind, LetStatementNode, ReturnStatementNode, SelectionExprNode, SignatureNode, StatementNode, Token, TokenKind,
    UnaryExprNode, WhileStatementNode,
};
use std::cell::RefCell;
use std::iter::zip;
use std::rc::Rc;

pub struct TypeChecker<'err, 'sym, 'file, 'pkg, 'ast, 'typ> {
    err_channel: &'err ErrorAccumulator,
    symbol_loader: &'sym SymbolLoader,
    file_loader: &'file FileLoader<'err>,
    ast_loader: &'ast AstLoader<'err, 'file, 'sym>,
    package_util: &'pkg PackageUtil<'err, 'file, 'sym, 'ast>,
    type_loader: &'typ TypeLoader,

    global_scope: RefCell<Option<Rc<Scope>>>,
}

#[derive(Default)]
struct PackageCheckState {
    functions: Vec<Func>,
    native_functions: Vec<NativeFunction>,
}

#[derive(Default)]
struct FunctionCheckState {
    locals: Vec<TypeId>,
}

impl FunctionCheckState {
    fn use_local(&mut self, type_id: TypeId) -> usize {
        self.locals.push(type_id);
        self.locals.len() - 1
    }
}

struct StatementInfo {
    statement: Statement,
    is_returning: bool,
    new_scope: Option<Rc<Scope>>,
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
            global_scope: RefCell::new(None),
        }
    }

    pub fn check_all(&self, main_package: SymbolId) -> Vec<Rc<Package>> {
        let package_names = self.package_util.get_all_packages(main_package);
        let mut packages = vec![];
        for pkg_name in package_names {
            let package = self.check_package(pkg_name);

            if pkg_name == main_package {
                let path = self.package_util.get_package_path(pkg_name);
                let file_id = self.file_loader.declare_file(path);
                let ast = self.ast_loader.get_ast(file_id);
                let scope = self.get_package_scope(pkg_name);

                let main = self.symbol_loader.declare_symbol("main");
                if let Some(main) = ast.items.get(&main) {
                    self.check_main_func(&scope, main.first().unwrap());
                } else {
                    self.err_channel.push(missing_main());
                }
            }

            packages.push(package);
        }
        packages
    }

    fn get_global_scope(&self) -> Rc<Scope> {
        let mut global_scope = self.global_scope.borrow_mut();
        if global_scope.is_none() {
            let scope = Scope::global(self.type_loader, self.symbol_loader);
            *global_scope = Some(scope);
        }
        global_scope.as_ref().unwrap().clone()
    }

    fn check_package(&self, package_name: SymbolId) -> Rc<Package> {
        let path = self.package_util.get_package_path(package_name);
        let file_id = self.file_loader.declare_file(path);
        let ast = self.ast_loader.get_ast(file_id);
        let mut state = PackageCheckState::default();
        let scope = self.get_package_scope(package_name);
        let mut str_helper = ConstStrHelper::default();

        for (name, items) in &ast.items {
            self.check_named_items(&mut state, file_id, scope.clone(), &mut str_helper, *name, items);
        }

        Rc::new(Package {
            name: package_name,
            functions: state.functions,
            native_functions: state.native_functions,
            strings: str_helper.take(),
        })
    }

    fn check_named_items(
        &self,
        state: &mut PackageCheckState,
        file_id: FileId,
        package_scope: Rc<Scope>,
        str_helper: &mut ConstStrHelper,
        name: SymbolId,
        items: &[ItemNode],
    ) {
        let mut item_iter = items.iter();
        let first_item = item_iter.next().unwrap();
        for item in item_iter {
            let declared_at = first_item.get_pos();
            let declared_at = self
                .file_loader
                .get_file(file_id)
                .expect("cannot find file")
                .get_pos(&declared_at);

            let name = self.symbol_loader.get_symbol(name).unwrap();
            self.err_channel
                .push(redeclared_symbol(name.as_ref(), declared_at, item.get_pos()));
        }

        for item in items.iter() {
            self.check_item(state, &package_scope, str_helper, name, item);
        }
    }

    fn get_package_scope(&self, package_name: SymbolId) -> Rc<Scope> {
        let path = self.package_util.get_package_path(package_name);
        let file_id = self.file_loader.declare_file(path);
        let package_node = self.ast_loader.get_ast(file_id);
        let global_scope = self.get_global_scope();

        let mut symbols = IndexMap::<SymbolId, Object>::new();
        for (name, items) in &package_node.items {
            let Some(first_item) = items.first() else {
                continue
            };

            let object = match first_item {
                ItemNode::Import(import_node) => {
                    let Some(package_path) = parse_string_tok(&import_node.path) else {
                        continue;
                    };
                    let package_path = package_path.to_vec();
                    let Ok(package_path) = String::from_utf8(package_path) else {
                        self.err_channel.push(not_a_valid_utf8_package(import_node.path.pos));
                        continue;
                    };
                    let package_path = self.symbol_loader.declare_symbol(package_path);
                    Object::Package(package_path)
                }
                ItemNode::Function(func_node) => {
                    let func_ty = self.get_func_type(&global_scope, &func_node.signature);
                    let type_id = self.type_loader.declare_type(Type::Func(func_ty));
                    Object::Func(type_id)
                }
                ItemNode::NativeFunction(signature) => {
                    let func_ty = self.get_func_type(&global_scope, signature);
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

        let func_type = self.get_func_type(scope, &func_node.signature);
        if !func_type.parameters.is_empty() || func_type.return_type.is_some() {
            self.err_channel.push(invalid_main_func(func_node.signature.pos));
        }
    }

    fn check_item(
        &self,
        state: &mut PackageCheckState,
        scope: &Rc<Scope>,
        str_helper: &mut ConstStrHelper,
        name: SymbolId,
        item_node: &ItemNode,
    ) {
        match item_node {
            ItemNode::Import(import_node) => self.check_import(import_node),
            ItemNode::Function(func_node) => {
                let func = self.check_fn(scope, str_helper, func_node);
                state.functions.push(func);
            }
            ItemNode::NativeFunction(signature) => {
                let func_type = self.get_func_type(scope, signature);

                let mut tags = vec![];
                'outer: for tag in &signature.tags {
                    let name = self.symbol_loader.declare_symbol(&tag.name.value);
                    let mut arguments = vec![];
                    for arg in &tag.arguments {
                        let Some(arg) = parse_string_tok(arg) else {
                            continue 'outer;
                        };
                        arguments.push(arg);
                    }
                    tags.push(Tag { name, arguments });
                }

                let native_func = NativeFunction {
                    package_name: scope.package_name().unwrap(),
                    function_name: name,
                    func_type,
                    tags,
                };
                state.native_functions.push(native_func);
            }
        }
    }

    fn check_import(&self, import_node: &ImportNode) {
        let Some(path) = parse_string_tok(&import_node.path) else {
            return;
        };
        if path.is_empty() {
            self.err_channel.push(empty_package_path(import_node.path.pos));
        }
    }

    fn check_fn(&self, scope: &Rc<Scope>, str_helper: &mut ConstStrHelper, func_node: &FunctionNode) -> Func {
        let mut symbols = IndexMap::<SymbolId, Object>::new();
        let mut func_state = FunctionCheckState::default();

        let func_type = self.get_func_type(scope, &func_node.signature);

        let param_types = func_type.parameters.iter();
        let param_nodes = func_node.signature.parameters.iter();
        for (param_ty, param_node) in zip(param_types, param_nodes) {
            symbols.insert(
                self.symbol_loader.declare_symbol(&param_node.name.value),
                Object::Local(*param_ty, func_state.use_local(*param_ty)),
            );
        }

        let func_scope = scope.new_child(ScopeKind::Function(func_type.return_type), symbols);
        let statement_info = self.check_block_statement(
            &mut func_state,
            &func_scope,
            str_helper,
            &func_node.body,
            ScopeKind::Basic,
        );
        let body = statement_info.statement;

        if func_type.return_type.is_some() && !statement_info.is_returning {
            self.err_channel
                .push(missing_return_statement(func_node.signature.get_pos()));
        }

        let function_name = self.symbol_loader.declare_symbol(&func_node.signature.name.value);
        Func {
            package_name: scope.package_name().unwrap(),
            function_name,
            func_type,
            locals: func_state.locals,
            body,
        }
    }

    fn check_block_statement(
        &self,
        state: &mut FunctionCheckState,
        scope: &Rc<Scope>,
        str_helper: &mut ConstStrHelper,
        node: &BlockStatementNode,
        kind: ScopeKind,
    ) -> StatementInfo {
        let mut scope = scope.new_child(kind, IndexMap::new());
        let mut statements = vec![];
        let mut is_returning = false;
        let mut unreachable_reported = false;
        for stmt in &node.statements {
            let statement_info = self.check_statement(state, &scope, str_helper, stmt);
            statements.push(statement_info.statement);

            if let Some(sc) = statement_info.new_scope {
                scope = sc;
            }

            if is_returning && !unreachable_reported {
                self.err_channel.push(unreachable_statement(stmt.get_pos()));
                unreachable_reported = true;
            }
            if statement_info.is_returning {
                is_returning = true;
            }
        }
        StatementInfo {
            statement: Statement::Block(BlockStatement { statements }),
            is_returning,
            new_scope: None,
        }
    }

    fn check_statement(
        &self,
        state: &mut FunctionCheckState,
        scope: &Rc<Scope>,
        str_helper: &mut ConstStrHelper,
        node: &StatementNode,
    ) -> StatementInfo {
        match node {
            StatementNode::Let(node) => self.check_let_statement(state, scope, str_helper, node),
            StatementNode::Assign(node) => self.check_assign_statement(scope, str_helper, node),
            StatementNode::Block(node) => self.check_block_statement(state, scope, str_helper, node, ScopeKind::Basic),
            StatementNode::If(node) => self.check_if_statement(state, scope, str_helper, node),
            StatementNode::While(node) => self.check_while_statement(state, scope, str_helper, node),
            StatementNode::Continue(token) => self.check_continue_statement(scope, token),
            StatementNode::Break(token) => self.check_break_statement(scope, token),
            StatementNode::Return(node) => self.check_return_statement(scope, str_helper, node),
            StatementNode::Expr(node) => StatementInfo {
                statement: Statement::Expr(self.get_expr(scope, str_helper, node, None)),
                is_returning: false,
                new_scope: None,
            },
        }
    }

    fn check_let_statement(
        &self,
        state: &mut FunctionCheckState,
        scope: &Rc<Scope>,
        str_helper: &mut ConstStrHelper,
        node: &LetStatementNode,
    ) -> StatementInfo {
        let name_sym = self.symbol_loader.declare_symbol(&node.name.value);
        match &node.kind {
            LetKind::TypeOnly { ty } => {
                let target_ty_id = self.get_expr_type(scope, ty);
                let target_ty = self.type_loader.get_type(target_ty_id).unwrap();

                let expr_kind = match target_ty.as_ref() {
                    Type::I64 => ExprKind::I64(0),
                    Type::I32 => ExprKind::I32(0),
                    Type::I16 => ExprKind::I16(0),
                    Type::I8 => ExprKind::I8(0),
                    Type::U64 => ExprKind::U64(0),
                    Type::U32 => ExprKind::U32(0),
                    Type::U16 => ExprKind::U16(0),
                    Type::U8 => ExprKind::U8(0),
                    Type::F32 => ExprKind::F32(0.0),
                    Type::F64 => ExprKind::F64(0.0),
                    Type::Bool => ExprKind::Bool(false),
                    Type::Slice(_) => ExprKind::Usize(0),
                    _ => todo!(),
                };
                let value_expr = Expr {
                    type_id: target_ty_id,
                    assignable: false,
                    kind: expr_kind,
                };
                let local_id = state.use_local(target_ty_id);

                StatementInfo {
                    statement: Statement::SetLocal(local_id, value_expr),
                    is_returning: false,
                    new_scope: Some(scope.new_child(
                        ScopeKind::Basic,
                        IndexMap::from([(name_sym, Object::Local(target_ty_id, local_id))]),
                    )),
                }
            }
            LetKind::TypeValue { ty, value } => {
                let target_ty_id = self.get_expr_type(scope, ty);
                let target_ty = self.type_loader.get_type(target_ty_id).unwrap();

                let mut value_expr = self.get_expr(scope, str_helper, value, Some(&target_ty));
                let type_id = value_expr.type_id;
                let value_ty = self.type_loader.get_type(type_id).unwrap();

                if !target_ty.is_assignable_with(&value_ty) {
                    self.err_channel.push(type_mismatch(
                        value.get_pos(),
                        target_ty.display(self.type_loader),
                        value_ty.display(self.type_loader),
                    ));
                    value_expr = Expr {
                        type_id: target_ty_id,
                        assignable: false,
                        kind: ExprKind::Invalid,
                    }
                }
                let local_id = state.use_local(target_ty_id);

                StatementInfo {
                    statement: Statement::SetLocal(local_id, value_expr),
                    is_returning: false,
                    new_scope: Some(scope.new_child(
                        ScopeKind::Basic,
                        IndexMap::from([(name_sym, Object::Local(target_ty_id, local_id))]),
                    )),
                }
            }
            LetKind::ValueOnly { value } => {
                let value = self.get_expr(scope, str_helper, value, None);
                let type_id = value.type_id;
                let local_id = state.use_local(type_id);
                StatementInfo {
                    statement: Statement::SetLocal(local_id, value),
                    is_returning: false,
                    new_scope: Some(scope.new_child(
                        ScopeKind::Basic,
                        IndexMap::from([(name_sym, Object::Local(type_id, local_id))]),
                    )),
                }
            }
        }
    }

    fn check_assign_statement(
        &self,
        scope: &Rc<Scope>,
        str_helper: &mut ConstStrHelper,
        node: &AssignStatementNode,
    ) -> StatementInfo {
        let receiver = self.get_expr(scope, str_helper, &node.receiver, None);
        if !receiver.assignable {
            self.err_channel.push(expr_unassignable(node.receiver.get_pos()));
        }

        let receiver_type = self.type_loader.get_type(receiver.type_id).unwrap();

        let mut value_expr = self.get_expr(scope, str_helper, &node.value, Some(&receiver_type));
        let type_id = value_expr.type_id;
        let value_ty = self.type_loader.get_type(type_id).unwrap();

        if !receiver_type.is_assignable_with(&value_ty) {
            self.err_channel.push(type_mismatch(
                node.value.get_pos(),
                receiver_type.display(self.type_loader),
                value_ty.display(self.type_loader),
            ));
            value_expr = Expr {
                type_id: receiver.type_id,
                assignable: false,
                kind: ExprKind::Invalid,
            }
        }

        let statement = match receiver.kind {
            ExprKind::Local(id) => Statement::SetLocal(id, value_expr),
            ExprKind::Index(target, index) => Statement::SetIndex {
                target: *target,
                index: *index,
                value: value_expr,
            },
            ExprKind::Deref(pointer) => Statement::SetAddr {
                addr: *pointer,
                value: value_expr,
            },
            ExprKind::Invalid
            | ExprKind::I64(..)
            | ExprKind::I32(..)
            | ExprKind::I16(..)
            | ExprKind::I8(..)
            | ExprKind::U64(..)
            | ExprKind::U32(..)
            | ExprKind::U16(..)
            | ExprKind::U8(..)
            | ExprKind::F64(..)
            | ExprKind::F32(..)
            | ExprKind::Bool(..)
            | ExprKind::Isize(..)
            | ExprKind::Usize(..)
            | ExprKind::Func(..)
            | ExprKind::StringLit(..)
            | ExprKind::Binary { .. }
            | ExprKind::Unary { .. }
            | ExprKind::Call { .. }
            | ExprKind::Cast { .. }
            | ExprKind::Pointer(..) => Statement::Invalid,
        };

        StatementInfo {
            statement,
            is_returning: false,
            new_scope: None,
        }
    }

    fn check_if_statement(
        &self,
        state: &mut FunctionCheckState,
        scope: &Rc<Scope>,
        str_helper: &mut ConstStrHelper,
        node: &IfStatementNode,
    ) -> StatementInfo {
        self.check_if_like_statement(
            state,
            scope,
            str_helper,
            &node.condition,
            &node.body,
            node.else_ifs.as_slice(),
            node.else_body.as_ref(),
        )
    }

    fn check_if_like_statement(
        &self,
        state: &mut FunctionCheckState,
        scope: &Rc<Scope>,
        str_helper: &mut ConstStrHelper,
        cond_expr: &ExprNode,
        body: &BlockStatementNode,
        else_ifs: &[ElseIfStatementNode],
        else_body: Option<&BlockStatementNode>,
    ) -> StatementInfo {
        let condition = self.get_expr(scope, str_helper, cond_expr, Some(&Type::Bool));

        let bool_type_id = self.type_loader.declare_type(Type::Bool);
        if condition.type_id != bool_type_id {
            let ty = self.type_loader.get_type(condition.type_id).unwrap();
            self.err_channel
                .push(type_mismatch(cond_expr.get_pos(), "bool", ty.display(self.type_loader)));
        }

        let body_stmt_info = self.check_block_statement(state, scope, str_helper, body, ScopeKind::Basic);

        let mut is_returning = body_stmt_info.is_returning && else_body.is_some();
        let else_body = if let Some(else_if) = else_ifs.first() {
            let stmt_info = self.check_if_like_statement(
                state,
                scope,
                str_helper,
                &else_if.condition,
                &else_if.body,
                &else_ifs[1..],
                else_body,
            );
            is_returning = is_returning && stmt_info.is_returning;
            Some(Box::new(stmt_info.statement))
        } else if let Some(else_body) = else_body {
            let stmt_info = self.check_block_statement(state, scope, str_helper, else_body, ScopeKind::Basic);
            is_returning = is_returning && stmt_info.is_returning;
            Some(Box::new(stmt_info.statement))
        } else {
            None
        };

        StatementInfo {
            statement: Statement::If(IfStatement {
                condition,
                body: Box::new(body_stmt_info.statement),
                else_body,
            }),
            is_returning,
            new_scope: None,
        }
    }

    fn check_while_statement(
        &self,
        state: &mut FunctionCheckState,
        scope: &Rc<Scope>,
        str_helper: &mut ConstStrHelper,
        node: &WhileStatementNode,
    ) -> StatementInfo {
        let condition = self.get_expr(scope, str_helper, &node.condition, Some(&Type::Bool));

        let bool_type_id = self.type_loader.declare_type(Type::Bool);
        if condition.type_id != bool_type_id {
            let ty = self.type_loader.get_type(condition.type_id).unwrap();
            self.err_channel.push(type_mismatch(
                node.condition.get_pos(),
                "bool",
                ty.display(self.type_loader),
            ));
        }

        let body_stmt_info = self.check_block_statement(state, scope, str_helper, &node.body, ScopeKind::Loop);

        StatementInfo {
            statement: Statement::While(WhileStatement {
                condition,
                body: Box::new(body_stmt_info.statement),
            }),
            is_returning: false,
            new_scope: None,
        }
    }

    fn check_continue_statement(&self, scope: &Rc<Scope>, token: &Token) -> StatementInfo {
        if !scope.is_inside_loop() {
            self.err_channel.push(not_in_a_loop(token.pos, "continue"));
            StatementInfo {
                statement: Statement::Invalid,
                is_returning: false,
                new_scope: None,
            }
        } else {
            StatementInfo {
                statement: Statement::Continue,
                is_returning: false,
                new_scope: None,
            }
        }
    }

    fn check_break_statement(&self, scope: &Rc<Scope>, token: &Token) -> StatementInfo {
        if !scope.is_inside_loop() {
            self.err_channel.push(not_in_a_loop(token.pos, "break"));
            StatementInfo {
                statement: Statement::Invalid,
                is_returning: false,
                new_scope: None,
            }
        } else {
            StatementInfo {
                statement: Statement::Break,
                is_returning: false,
                new_scope: None,
            }
        }
    }

    fn check_return_statement(
        &self,
        scope: &Rc<Scope>,
        str_helper: &mut ConstStrHelper,
        node: &ReturnStatementNode,
    ) -> StatementInfo {
        if let Some(ref return_type) = scope.return_type() {
            let Some(ref expr_node) = node.value else {
                self.err_channel.push(missing_return_value(node.get_pos()));
                return StatementInfo{
                    statement: Statement::Invalid,
                    is_returning: true,
                    new_scope: None,
                };
            };

            let return_type = self.type_loader.get_type(*return_type).unwrap();
            let expr = self.get_expr(scope, str_helper, expr_node, Some(&return_type));

            let ty = self.type_loader.get_type(expr.type_id).unwrap();
            if !return_type.is_assignable_with(&ty) {
                self.err_channel.push(type_mismatch(
                    expr_node.get_pos(),
                    return_type.display(self.type_loader),
                    ty.display(self.type_loader),
                ));
                StatementInfo {
                    statement: Statement::Invalid,
                    is_returning: true,
                    new_scope: None,
                }
            } else {
                StatementInfo {
                    statement: Statement::Return(ReturnStatement { value: Some(expr) }),
                    is_returning: true,
                    new_scope: None,
                }
            }
        } else if let Some(val) = &node.value {
            self.err_channel.push(function_is_void(val.get_pos()));
            return StatementInfo {
                statement: Statement::Invalid,
                is_returning: true,
                new_scope: None,
            };
        } else {
            return StatementInfo {
                statement: Statement::Return(ReturnStatement { value: None }),
                is_returning: true,
                new_scope: None,
            };
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
                    self.err_channel.push(not_a_type(tok.pos));
                    return self.type_loader.declare_type(Type::Invalid);
                };
                type_id
            }
            ExprNode::Slice(slice_node) => {
                let element_type_id = self.get_expr_type(scope, &slice_node.element);
                self.type_loader.declare_type(Type::Slice(SliceType {
                    element_type: element_type_id,
                }))
            }
            ExprNode::Deref(deref_node) => {
                let element_type_id = self.get_expr_type(scope, &deref_node.value);
                self.type_loader.declare_type(Type::Pointer(PointerType {
                    element_type: element_type_id,
                }))
            }
            _ => {
                self.err_channel.push(not_a_type(expr_node.get_pos()));
                self.type_loader.declare_type(Type::Invalid)
            }
        }
    }

    fn get_expr(
        &self,
        scope: &Rc<Scope>,
        str_helper: &mut ConstStrHelper,
        expr_node: &ExprNode,
        expected_type: Option<&Type>,
    ) -> Expr {
        match expr_node {
            ExprNode::Ident(tok) => self.get_ident_expr(scope, tok),
            ExprNode::IntegerLiteral(tok) => self.get_int_lit_expr(scope, tok, expected_type),
            ExprNode::RealLiteral(tok) => self.get_real_lit_expr(scope, tok),
            ExprNode::BooleanLit(tok) => self.get_bool_lit_expr(scope, tok),
            ExprNode::StringLit(tok) => self.get_str_lit_expr(scope, str_helper, tok),
            ExprNode::Binary(binary_expr) => self.get_binary_expr(scope, str_helper, binary_expr),
            ExprNode::Deref(deref_expr) => self.get_deref_expr(scope, str_helper, deref_expr),
            ExprNode::Unary(unary_expr) => self.get_unary_expr(scope, str_helper, unary_expr),
            ExprNode::Call(call_expr) => self.get_call_expr(scope, str_helper, call_expr),
            ExprNode::Cast(cast_expr) => self.get_cast_expr(scope, str_helper, cast_expr),
            ExprNode::Selection(selection_expr) => self.get_selection_expr(scope, str_helper, selection_expr),
            ExprNode::Index(index_expr) => self.get_index_expr(scope, str_helper, index_expr),
            ExprNode::Grouped(expr) => self.get_expr(scope, str_helper, &expr.value, expected_type),
            ExprNode::Slice(..) => todo!("slice is not a value expression"),
        }
    }

    fn get_ident_expr(&self, scope: &Rc<Scope>, tok: &Token) -> Expr {
        self.get_expr_from_scope_symbol(scope, tok)
    }

    fn get_expr_from_scope_symbol(&self, scope: &Rc<Scope>, tok: &Token) -> Expr {
        let symbol_id = self.symbol_loader.declare_symbol(&tok.value);
        let Some(ident_object) = scope.get(symbol_id) else {
            self.err_channel.push(undeclared_symbol(tok.clone()));
            return self.invalid_value_expr();
        };

        match ident_object {
            Object::Local(type_id, index) => Expr {
                type_id,
                assignable: true,
                kind: ExprKind::Local(index),
            },
            Object::Func(type_id) => Expr {
                type_id,
                assignable: false,
                kind: ExprKind::Func(FuncExpr {
                    package_name: scope.package_name().unwrap(),
                    function_name: symbol_id,
                }),
            },
            _ => {
                self.err_channel.push(not_a_value(tok.pos));
                self.invalid_value_expr()
            }
        }
    }

    fn get_int_lit_expr(&self, scope: &Rc<Scope>, tok: &Token, expected_type: Option<&Type>) -> Expr {
        let kind = match expected_type.unwrap_or(&Type::Bool) {
            Type::Isize => tok.value.parse::<i64>().map(ExprKind::Isize),
            Type::I32 => tok.value.parse::<i32>().map(ExprKind::I32),
            Type::I16 => tok.value.parse::<i16>().map(ExprKind::I16),
            Type::I8 => tok.value.parse::<i8>().map(ExprKind::I8),
            Type::Usize => tok.value.parse::<u64>().map(ExprKind::Usize),
            Type::U64 => tok.value.parse::<u64>().map(ExprKind::U64),
            Type::U32 => tok.value.parse::<u32>().map(ExprKind::U32),
            Type::U16 => tok.value.parse::<u16>().map(ExprKind::U16),
            Type::U8 => tok.value.parse::<u8>().map(ExprKind::U8),
            Type::Pointer(pointer_ty) => tok
                .value
                .parse::<usize>()
                .map(|val| ExprKind::Pointer(val, pointer_ty.element_type)),
            _ => tok.value.parse::<i64>().map(ExprKind::I64),
        };

        let kind = match kind {
            Ok(v) => v,
            Err(err) => {
                self.err_channel.push(invalid_integer_literal(tok.pos, err));
                ExprKind::Invalid
            }
        };

        let type_id = if let Some(Type::Pointer(pointer_ty)) = expected_type {
            self.type_loader.declare_type(Type::Pointer(pointer_ty.clone()))
        } else {
            let type_name_id = match expected_type.unwrap_or(&Type::Bool) {
                Type::Isize => self.symbol_loader.declare_symbol(ISIZE),
                Type::I32 => self.symbol_loader.declare_symbol(I32),
                Type::I16 => self.symbol_loader.declare_symbol(I16),
                Type::I8 => self.symbol_loader.declare_symbol(I8),
                Type::Usize => self.symbol_loader.declare_symbol(USIZE),
                Type::U64 => self.symbol_loader.declare_symbol(U64),
                Type::U32 => self.symbol_loader.declare_symbol(U32),
                Type::U16 => self.symbol_loader.declare_symbol(U16),
                Type::U8 => self.symbol_loader.declare_symbol(U8),
                _ => self.symbol_loader.declare_symbol(I64),
            };
            scope
                .get(type_name_id)
                .and_then(|v| v.as_type())
                .expect("invalid state, type not found")
        };

        Expr {
            type_id,
            assignable: false,
            kind,
        }
    }

    fn get_real_lit_expr(&self, scope: &Rc<Scope>, tok: &Token) -> Expr {
        let kind = match tok.value.parse::<f64>() {
            Ok(v) => ExprKind::F64(v),
            Err(err) => {
                self.err_channel.push(invalid_real_literal(tok.pos, err));
                ExprKind::Invalid
            }
        };

        let f64_type_id = self.symbol_loader.declare_symbol(F64);
        let f64_type_id = scope
            .get(f64_type_id)
            .and_then(|v| v.as_type())
            .expect("invalid state, f64 type should be found");

        Expr {
            type_id: f64_type_id,
            assignable: false,
            kind,
        }
    }

    fn get_bool_lit_expr(&self, scope: &Rc<Scope>, tok: &Token) -> Expr {
        let kind = if tok.kind == TokenKind::True {
            ExprKind::Bool(true)
        } else if tok.kind == TokenKind::False {
            ExprKind::Bool(false)
        } else {
            ExprKind::Invalid
        };

        let bool_type_id = self.symbol_loader.declare_symbol(BOOL);
        let bool_type_id = scope
            .get(bool_type_id)
            .and_then(|v| v.as_type())
            .expect("invalid state, bool type should be found");

        Expr {
            type_id: bool_type_id,
            assignable: false,
            kind,
        }
    }

    fn get_str_lit_expr(&self, scope: &Rc<Scope>, str_helper: &mut ConstStrHelper, tok: &Token) -> Expr {
        let u8_type_id = self.symbol_loader.declare_symbol(U8);
        let u8_type_id = scope
            .get(u8_type_id)
            .and_then(|v| v.as_type())
            .expect("invalid state, u8 type should be found");
        let type_id = self.type_loader.declare_type(Type::Slice(SliceType {
            element_type: u8_type_id,
        }));

        let Some(string_lit) = parse_string_tok(tok) else {
            return Expr {
                type_id,
                assignable: false,
                kind: ExprKind::Invalid,
            };
        };

        let index = str_helper.declare_str(&string_lit);
        Expr {
            type_id,
            assignable: false,
            kind: ExprKind::StringLit(StringLitExpr {
                package_name: scope.package_name().unwrap(),
                index,
            }),
        }
    }

    fn get_binary_expr(&self, scope: &Rc<Scope>, str_helper: &mut ConstStrHelper, expr: &BinaryExprNode) -> Expr {
        let a_expr = self.get_expr(scope, str_helper, &expr.a, None);
        let a_ty = self.type_loader.get_type(a_expr.type_id).unwrap();

        let b_expr = self.get_expr(scope, str_helper, &expr.b, Some(&a_ty));
        let b_ty = self.type_loader.get_type(b_expr.type_id).unwrap();

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

        if a_expr.type_id != b_expr.type_id {
            self.err_channel.push(binop_type_mismatch(
                expr.get_pos(),
                op_name,
                a_ty.display(self.type_loader),
                b_ty.display(self.type_loader),
            ));
            return Expr {
                type_id: a_expr.type_id,
                assignable: false,
                kind: ExprKind::Invalid,
            };
        }

        let result_ty = match expr.op.kind {
            TokenKind::Add | TokenKind::Sub | TokenKind::Mul | TokenKind::Div => {
                if a_ty.is_numeric() {
                    a_ty.as_ref().clone()
                } else {
                    self.err_channel.push(binop_type_unsupported(
                        expr.get_pos(),
                        op_name,
                        a_ty.display(self.type_loader),
                    ));
                    Type::Invalid
                }
            }
            TokenKind::Eq | TokenKind::NEq | TokenKind::Gt | TokenKind::GEq | TokenKind::Lt | TokenKind::LEq => {
                if a_ty.is_numeric() {
                    Type::Bool
                } else {
                    self.err_channel.push(binop_type_unsupported(
                        expr.get_pos(),
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
                if a_ty.is_int() {
                    a_ty.as_ref().clone()
                } else {
                    self.err_channel.push(binop_type_unsupported(
                        expr.get_pos(),
                        op_name,
                        a_ty.display(self.type_loader),
                    ));
                    Type::Invalid
                }
            }
            TokenKind::And | TokenKind::Not | TokenKind::Or => {
                if a_ty.is_bool() {
                    Type::Bool
                } else {
                    self.err_channel.push(binop_type_unsupported(
                        expr.get_pos(),
                        op_name,
                        a_ty.display(self.type_loader),
                    ));
                    Type::Invalid
                }
            }
            op => unreachable!("token {op} is not a binary operator"),
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
            op => unreachable!("token {op} is not a binary operator"),
        };

        Expr {
            type_id: result_type_id,
            assignable: false,
            kind: ExprKind::Binary {
                a: Box::new(a_expr),
                op,
                b: Box::new(b_expr),
            },
        }
    }

    fn get_deref_expr(&self, scope: &Rc<Scope>, str_helper: &mut ConstStrHelper, expr: &DerefExprNode) -> Expr {
        let addr_expr = self.get_expr(scope, str_helper, &expr.value, None);

        let value_ty = self.type_loader.get_type(addr_expr.type_id).unwrap();
        let Type::Pointer(pointer_type) = value_ty.as_ref() else {
            self.err_channel.push(cannot_deref_a_non_pointer(expr.pos));
            return self.invalid_value_expr();
        };

        let element_ty = pointer_type.element_type;
        Expr {
            type_id: element_ty,
            assignable: true,
            kind: ExprKind::Deref(Box::new(addr_expr)),
        }
    }

    fn get_unary_expr(&self, scope: &Rc<Scope>, str_helper: &mut ConstStrHelper, expr: &UnaryExprNode) -> Expr {
        let val_expr = self.get_expr(scope, str_helper, &expr.value, None);
        let val_ty = self.type_loader.get_type(val_expr.type_id).unwrap();

        let op_name = match expr.op.kind {
            TokenKind::BitNot => "bit not",
            TokenKind::Sub => "sub",
            TokenKind::Add => "add",
            TokenKind::Not => "not",
            op => unreachable!("token {op} is not a unary operator"),
        };

        let is_bool = val_ty.is_bool();
        let is_number = val_ty.is_numeric();
        let is_int = val_ty.is_int();

        let (op, is_valid) = match expr.op.kind {
            TokenKind::BitNot => (UnOp::BitNot, is_int),
            TokenKind::Sub => (UnOp::Sub, is_number),
            TokenKind::Add => (UnOp::Add, is_number),
            TokenKind::Not => (UnOp::Not, is_bool),
            op => unreachable!("token {op} is not a unary operator"),
        };

        if !is_valid {
            self.err_channel.push(unop_type_unsupported(
                expr.get_pos(),
                op_name,
                val_ty.display(self.type_loader),
            ));
        }

        Expr {
            type_id: val_expr.type_id,
            assignable: false,
            kind: ExprKind::Unary {
                op,
                val: Box::new(val_expr),
            },
        }
    }

    fn get_call_expr(&self, scope: &Rc<Scope>, str_helper: &mut ConstStrHelper, call_expr: &CallExprNode) -> Expr {
        let func_expr = self.get_expr(scope, str_helper, &call_expr.target, None);
        let ty = self.type_loader.get_type(func_expr.type_id).unwrap();
        let Type::Func(func_type) = ty.as_ref() else {
            self.err_channel.push(not_a_func(call_expr.target.get_pos()));
            return self.invalid_value_expr();
        };

        if call_expr.arguments.len() != func_type.parameters.len() {
            self.err_channel.push(unmatch_function_arguments(
                call_expr.pos,
                func_type.parameters.len(),
                call_expr.arguments.len(),
            ));
            return self.invalid_value_expr();
        }

        let mut arguments = vec![];
        for (param, arg) in zip(func_type.parameters.iter(), call_expr.arguments.iter()) {
            let param_ty = self.type_loader.get_type(*param).unwrap();

            let arg_expr = self.get_expr(scope, str_helper, arg, Some(&param_ty));
            let arg_ty = self.type_loader.get_type(arg_expr.type_id).unwrap();

            if !param_ty.is_assignable_with(&arg_ty) {
                self.err_channel.push(type_mismatch(
                    arg.get_pos(),
                    param_ty.display(self.type_loader),
                    arg_ty.display(self.type_loader),
                ));
                return self.invalid_value_expr();
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
            assignable: false,
            kind: ExprKind::Call(Box::new(func_expr), arguments),
        }
    }

    fn get_cast_expr(&self, scope: &Rc<Scope>, str_helper: &mut ConstStrHelper, cast_expr: &CastExprNode) -> Expr {
        let target_type_id = self.get_expr_type(scope, &cast_expr.target);
        let target_ty = self.type_loader.get_type(target_type_id).unwrap();
        let target_is_number = target_ty.is_numeric();

        if let Type::Invalid = target_ty.as_ref() {
            return self.invalid_value_expr();
        }

        let value = self.get_expr(scope, str_helper, &cast_expr.value, None);
        if let ExprKind::Invalid = value.kind {
            return value;
        }

        let initial_ty = self.type_loader.get_type(value.type_id).unwrap();
        let initial_is_number = initial_ty.is_numeric();

        let kind = if initial_is_number && target_is_number {
            ExprKind::Cast(Box::new(value), target_type_id)
        } else {
            self.err_channel.push(casting_unsupported(
                cast_expr.get_pos(),
                initial_ty.display(self.type_loader),
                target_ty.display(self.type_loader),
            ));
            ExprKind::Invalid
        };

        Expr {
            type_id: target_type_id,
            assignable: false,
            kind,
        }
    }

    fn get_selection_expr(
        &self,
        scope: &Rc<Scope>,
        str_helper: &mut ConstStrHelper,
        sel_expr: &SelectionExprNode,
    ) -> Expr {
        if let Some(expr) = self.get_package_selection_expr(scope, sel_expr) {
            return expr;
        }

        let selection = self.get_expr(scope, str_helper, &sel_expr.value, None);
        if selection.kind == ExprKind::Invalid {
            return selection;
        }

        todo!("non-package selection is not supported yet");
    }

    fn get_package_selection_expr(&self, scope: &Rc<Scope>, sel_expr: &SelectionExprNode) -> Option<Expr> {
        let ExprNode::Ident(tok) = sel_expr.value.as_ref() else {
            return None;
        };

        let symbol_id = self.symbol_loader.declare_symbol(&tok.value);
        let object = scope.get(symbol_id)?;

        let Object::Package(package_id) = object else {
            return None;
        };

        let package_scope = self.get_package_scope(package_id);
        Some(self.get_expr_from_scope_symbol(&package_scope, &sel_expr.selection))
    }

    fn get_index_expr(&self, scope: &Rc<Scope>, str_helper: &mut ConstStrHelper, index_expr: &IndexExprNode) -> Expr {
        let target = self.get_expr(scope, str_helper, &index_expr.value, None);

        let target_ty = self.type_loader.get_type(target.type_id).unwrap();
        let Type::Slice(slice_ty) = target_ty.as_ref() else {
            self.err_channel.push(not_indexable(index_expr.value.get_pos()));
            return self.invalid_value_expr();
        };

        let index = self.get_expr(scope, str_helper, &index_expr.index, None);
        let index_ty = self.type_loader.get_type(index.type_id).unwrap();
        if !index_ty.is_int() {
            self.err_channel.push(cannot_used_as_index(index_expr.index.get_pos()));
            return Expr {
                type_id: slice_ty.element_type,
                assignable: true,
                kind: ExprKind::Invalid,
            };
        }

        Expr {
            type_id: slice_ty.element_type,
            assignable: true,
            kind: ExprKind::Index(Box::new(target), Box::new(index)),
        }
    }

    fn invalid_value_expr(&self) -> Expr {
        Expr {
            type_id: self.type_loader.declare_type(Type::Invalid),
            assignable: false,
            kind: ExprKind::Invalid,
        }
    }
}

#[derive(Default)]
struct ConstStrHelper {
    strs: Vec<Box<[u8]>>,
}

impl ConstStrHelper {
    fn declare_str(&mut self, value: &[u8]) -> usize {
        self.strs.push(value.into());
        self.strs.len() - 1
    }

    fn take(&mut self) -> Vec<Box<[u8]>> {
        std::mem::take(&mut self.strs)
    }
}
