// TODO: find better name.

use std::collections::{HashMap, HashSet};

use crate::{
    ast::{self, Root},
    semantic::{
        BinOp, Binary, ConcreteType, Expr, ExprKind, FieldValue, FloatType, FnHeader, FunctionCall, Header, IntType,
        Selector, StructLit, Type, TypeDecl, Unary, UnaryOp, VarHeader,
    },
    token::TokenKind,
    type_helper::{ITypeHelper, TypeHelper},
};

#[derive(Debug)]
pub enum Error {
    EmptyPackage,
    MissingPackage,
    ImportCycle,
    RedeclaredSymbol,
    UndeclaredSymbol,
    UndeclaredField,
    UnresolvedType,
    MismatchType,
    CyclicType,
    NotAStruct,
    NotAFn,
    UnsupportedOperationInConstant,
    FnCallArgNumMismatch,
}

pub trait HeaderProcessor {
    fn build_headers<'a>(&self, roots: &'a [Root]) -> Result<Vec<Header>, Error>;
}

pub struct SimpleHeaderProcessor {}

impl SimpleHeaderProcessor {
    pub fn new() -> Self {
        Self {}
    }

    fn build<'a>(&self, roots: &'a [Root]) -> Result<Vec<Header>, Error> {
        let dependency_graph = self.build_package_dependency(roots);
        let plan = self.build_processing_plan(&dependency_graph)?;

        let mut pack_to_ast: HashMap<&str, Vec<&Root>> = HashMap::new();
        for root in roots.iter() {
            pack_to_ast
                .entry(&root.package_name.unwrap_value().as_str())
                .or_default()
                .push(root);
        }

        let mut headers = Vec::new();
        for pack in plan.iter() {
            let roots = pack_to_ast.get(*pack).unwrap().as_slice();
            let header = self.build_header(roots, &headers)?;
            headers.push(header);
        }

        Ok(headers)
    }

    fn build_package_dependency<'a>(&self, roots: &'a [Root]) -> HashMap<&'a str, HashSet<&'a str>> {
        let mut dependency_graph: HashMap<&str, HashSet<&str>> = HashMap::new();
        for root in roots.iter() {
            let deps = dependency_graph
                .entry(root.package_name.unwrap_value().as_str())
                .or_default();
            for import in root.imports.iter() {
                deps.insert(import.package_name.unwrap_value().as_str());
            }
        }
        dependency_graph
    }

    fn build_processing_plan<'a>(
        &self,
        dependency_graph: &HashMap<&'a str, HashSet<&'a str>>,
    ) -> Result<Vec<&'a str>, Error> {
        if dependency_graph.is_empty() {
            return Err(Error::EmptyPackage);
        }

        let mut plan = Vec::new();
        let mut plan_set = HashSet::new();

        // TODO: make it more deterministic. maybe use ordered hash map for the iteration.
        for pack in dependency_graph.keys() {
            if plan_set.contains(pack) {
                continue;
            }

            let mut stack = vec![*pack];
            let mut in_stack = HashSet::new();
            in_stack.insert(*pack);

            while let Some(p) = stack.pop() {
                stack.push(p);
                let deps = dependency_graph.get(p).ok_or(Error::MissingPackage)?;

                let mut has_unresolved = false;
                for dep in deps.iter() {
                    if plan_set.contains(dep) {
                        continue;
                    }
                    if in_stack.contains(dep) {
                        return Err(Error::ImportCycle); // TODO: put more information here.
                    }

                    stack.push(*dep);
                    in_stack.insert(*dep);
                    has_unresolved = true;
                }

                if !has_unresolved {
                    plan.push(p);
                    plan_set.insert(p);
                    stack.pop();
                }
            }
        }

        Ok(plan)
    }

    fn build_header(&self, roots: &[&Root], dependencies: &[Header]) -> Result<Header, Error> {
        let mut processor = SingleHeaderProcessor::new(dependencies);
        for root in roots.into_iter() {
            processor.process(root)?;
        }
        Ok(processor.build())
    }
}

impl HeaderProcessor for SimpleHeaderProcessor {
    fn build_headers<'a>(&self, roots: &'a [Root]) -> Result<Vec<Header>, Error> {
        self.build(roots)
    }
}

struct SingleHeaderProcessor<'a> {
    package_name: Option<String>,
    dependencies: &'a [Header],

    types: Vec<TypeDecl>,
    vars: Vec<VarHeader>,
    functions: Vec<FnHeader>,
}

impl<'a> SingleHeaderProcessor<'a> {
    fn new(dependencies: &'a [Header]) -> Self {
        Self {
            package_name: None,
            dependencies,
            types: Vec::new(),
            vars: Vec::new(),
            functions: Vec::new(),
        }
    }

    fn process(&mut self, root: &'a Root) -> Result<(), Error> {
        self.package_name = Some(root.package_name.unwrap_value().clone());

        let type_helper = TypeHelper::from_headers(self.dependencies);

        let mut type_processor = TypeProcessor::new(root, &type_helper);
        let type_aliases = type_processor.setup()?;
        for (name, typ) in type_aliases.iter() {
            self.types.push(TypeDecl {
                name: String::from(*name),
                typ: typ.clone(),
            });
        }

        let mut value_processor = ValueProcessor::new(root, &type_helper);
        value_processor.setup()?;
        let (fn_headers, var_headers) = value_processor.build_headers();
        for var_header in var_headers.into_iter() {
            self.vars.push(var_header);
        }
        for func_header in fn_headers.into_iter() {
            self.functions.push(func_header);
        }

        Ok(())
    }

    fn build(self) -> Header {
        Header {
            package_name: self.package_name.unwrap(),
            types: self.types,
            vars: self.vars,
            functions: self.functions,
        }
    }
}

struct TypeProcessor<'a> {
    root: &'a ast::Root,
    type_helper: &'a dyn ITypeHelper<'a>,
}

impl<'a> TypeProcessor<'a> {
    fn new(root: &'a ast::Root, type_helper: &'a dyn ITypeHelper<'a>) -> Self {
        Self { root, type_helper }
    }

    fn setup(&mut self) -> Result<HashMap<&str, Type>, Error> {
        let plan = self.build_setup_plan()?;
        Ok(self.setup_types(&plan[..]))
    }

    fn build_setup_plan(&mut self) -> Result<Vec<&'a str>, Error> {
        let type_decls: Vec<&'a ast::TypeDecl> = self
            .root
            .declarations
            .iter()
            .filter_map(|decl| decl.try_unwrap_type())
            .collect();

        let mut name_to_type = HashMap::new();
        for type_decl in type_decls.iter() {
            let name = type_decl.name.unwrap_value();
            if name_to_type.contains_key(name.as_str()) {
                return Err(Error::RedeclaredSymbol);
            }
            name_to_type.insert(name.as_str(), &type_decl.typ);
        }

        let mut plan: Vec<&'a str> = vec![];
        let mut visited = HashSet::new();

        for type_decl in type_decls.iter() {
            if visited.contains(type_decl.name.unwrap_value().as_str()) {
                continue;
            }

            let chain = self.traverse_type_alias(&name_to_type, *type_decl)?;

            for item in chain.into_iter().rev() {
                if !visited.contains(&item) {
                    plan.push(item);
                    visited.insert(item);
                }
            }
        }

        Ok(plan)
    }

    fn traverse_type_alias(
        &self,
        name_to_type: &HashMap<&'a str, &'a ast::Type>,
        type_decl: &'a ast::TypeDecl,
    ) -> Result<Vec<&'a str>, Error> {
        let mut chain: Vec<&'a str> = vec![];
        let mut chain_set = HashSet::new();

        let mut current_type = &type_decl.name;
        loop {
            let name = current_type.unwrap_value().as_str();

            if chain_set.contains(name) {
                return Err(Error::CyclicType);
            }

            chain.push(name);
            chain_set.insert(name);

            let current_type_decl = name_to_type.get(name).ok_or(Error::UndeclaredSymbol)?;

            match &current_type_decl {
                ast::Type::Primitive(_) | ast::Type::Struct(_) => break,
                ast::Type::Ident(token) => current_type = token,
                ast::Type::Selector(_) => break,
            }
        }

        Ok(chain)
    }

    fn setup_types(&mut self, plan: &[&str]) -> HashMap<&str, Type> {
        let mut type_decls: HashMap<&str, &ast::TypeDecl> = self
            .root
            .declarations
            .iter()
            .filter_map(|decl| decl.try_unwrap_type())
            .map(|decl| (decl.name.unwrap_value().as_str(), decl))
            .collect();
        let type_decls: Vec<&ast::TypeDecl> = plan.iter().map(|v| type_decls.remove(*v).unwrap()).collect();

        let mut type_alias = HashMap::new();
        for type_decl in type_decls.iter() {
            let name = type_decl.name.value.as_ref().unwrap();
            type_alias.insert(name.as_str(), Type::from_concrete(ConcreteType::Invalid));
        }

        self.populate_types(&mut type_alias, type_decls.as_slice());
        self.populate_types(&mut type_alias, type_decls.as_slice());

        type_alias
    }

    fn populate_types(&mut self, type_alias: &mut HashMap<&'a str, Type>, type_decls: &[&'a ast::TypeDecl]) {
        for type_decl in type_decls.iter() {
            let name = type_decl.name.unwrap_value();
            let typ = self.type_helper.get(&type_decl.typ).unwrap();
            type_alias.insert(name.as_str(), typ);
        }
    }
}

struct ValueProcessor<'a, 'b> {
    root_ast: &'a ast::Root,
    type_helper: &'b dyn ITypeHelper<'a>,

    symbol_table: Vec<HashMap<String, Symbol>>,
}

#[derive(Clone, Debug)]
struct Symbol {
    typ: Type,
}

impl<'a, 'b> ValueProcessor<'a, 'b> {
    fn new(root_ast: &'a ast::Root, type_helper: &'b dyn ITypeHelper<'a>) -> Self {
        Self {
            root_ast,
            type_helper,
            symbol_table: Vec::new(),
        }
    }

    fn build_headers(&self) -> (Vec<FnHeader>, Vec<VarHeader>) {
        let mut fn_result = Vec::new();
        let mut var_result = Vec::new();
        for table in self.symbol_table.iter() {
            for (name, symbol) in table.iter() {
                if let ConcreteType::Fn(fn_type) = &*symbol.typ.borrow() {
                    fn_result.push(FnHeader {
                        name: name.clone(),
                        native: fn_type.native,
                        typ: symbol.typ.clone(),
                    })
                } else {
                    var_result.push(VarHeader {
                        name: name.clone(),
                        typ: symbol.typ.clone(),
                    })
                }
            }
        }
        (fn_result, var_result)
    }

    fn setup(&mut self) -> Result<(), Error> {
        self.setup_values()?;
        self.setup_functions()?;
        Ok(())
    }

    fn setup_values(&mut self) -> Result<(), Error> {
        let type_decls: Vec<&ast::Var> = self
            .root_ast
            .declarations
            .iter()
            .filter_map(|decl| {
                if let ast::Declaration::Var(t) = decl {
                    Some(t)
                } else {
                    None
                }
            })
            .collect();

        self.add_symbol_table_block();
        for type_decl in type_decls.iter() {
            let name = type_decl.name.value.as_ref().unwrap();
            let typ = self.type_helper.get(&type_decl.typ).ok_or(Error::UnresolvedType)?;

            if self.find_symbol(name).is_some() {
                return Err(Error::RedeclaredSymbol);
            }

            if let Some(val) = &type_decl.value {
                let value = self.analyze_expr(val, Some(&typ), true)?;
                let value_type = value.typ.clone();
                if &value_type != &typ {
                    return Err(Error::MismatchType);
                }
            }

            self.add_symbol(name, typ);
        }

        Ok(())
    }

    fn analyze_expr(&self, expr: &ast::Expr, expected: Option<&Type>, is_global_var: bool) -> Result<Expr, Error> {
        match &expr.kind {
            ast::ExprKind::Ident(token) => {
                let name = token.value.as_ref().unwrap();
                let symbol = self.find_symbol(name);

                if let Some(sym) = symbol {
                    Ok(Expr {
                        kind: ExprKind::Ident(name.clone()),
                        assignable: true,
                        typ: sym.typ.clone(),
                    })
                } else {
                    Err(Error::UndeclaredSymbol)
                }
            }
            ast::ExprKind::StructLit(struct_lit) => {
                let name = struct_lit.name.unwrap_value();
                let typ = self
                    .type_helper
                    .get_by_name(name.as_str())
                    .ok_or(Error::UndeclaredSymbol)?;

                let struct_fields = if let ConcreteType::Struct(strct) = &*typ.borrow() {
                    strct.fields.clone()
                } else {
                    return Err(Error::NotAStruct);
                };

                let mut fields = Vec::new();
                for field in struct_lit.fields.iter() {
                    let type_field = struct_fields.get(field.name.value.as_ref().unwrap());
                    if type_field.is_none() {
                        return Err(Error::UndeclaredField);
                    }
                    let type_field = type_field.unwrap();

                    let val = self.analyze_expr(&field.value, expected, is_global_var)?;
                    if val.typ != type_field.typ.upgrade().unwrap() {
                        return Err(Error::MismatchType);
                    }

                    fields.push(FieldValue {
                        index: type_field.index,
                        value: val,
                    });
                }

                Ok(Expr {
                    kind: ExprKind::Struct(StructLit { fields }),
                    assignable: false,
                    typ,
                })
            }
            ast::ExprKind::IntegerLit(val) => {
                let i32_type = self.type_helper.get_i32();
                let expected = expected.unwrap_or(&i32_type);
                let concrete_type = expected.borrow();
                if let ConcreteType::Int(IntType { signed, size }) = &*concrete_type {
                    let kind = match (signed, size) {
                        (true, 8) => ExprKind::I8(val.value.as_ref().unwrap().parse().unwrap()),
                        (true, 16) => ExprKind::I16(val.value.as_ref().unwrap().parse().unwrap()),
                        (true, 32) => ExprKind::I32(val.value.as_ref().unwrap().parse().unwrap()),
                        (true, 64) => ExprKind::I64(val.value.as_ref().unwrap().parse().unwrap()),
                        (false, 8) => ExprKind::U8(val.value.as_ref().unwrap().parse().unwrap()),
                        (false, 16) => ExprKind::U16(val.value.as_ref().unwrap().parse().unwrap()),
                        (false, 32) => ExprKind::U32(val.value.as_ref().unwrap().parse().unwrap()),
                        (false, 64) => ExprKind::U64(val.value.as_ref().unwrap().parse().unwrap()),
                        _ => unreachable!(),
                    };
                    Ok(Expr {
                        kind,
                        assignable: false,
                        typ: expected.clone(),
                    })
                } else {
                    Ok(Expr {
                        kind: ExprKind::I32(0),
                        assignable: false,
                        typ: self.type_helper.get_i32(),
                    })
                }
            }
            ast::ExprKind::FloatLit(val) => {
                let f64_type = self.type_helper.get_f64();
                let expected = expected.unwrap_or(&f64_type);
                let concrete_type = expected.borrow();
                if let ConcreteType::Float(FloatType { size }) = &*concrete_type {
                    let kind = match size {
                        32 => ExprKind::F32(val.value.as_ref().unwrap().parse().unwrap()),
                        64 => ExprKind::F64(val.value.as_ref().unwrap().parse().unwrap()),
                        _ => unreachable!(),
                    };
                    Ok(Expr {
                        kind,
                        assignable: false,
                        typ: expected.clone(),
                    })
                } else {
                    Ok(Expr {
                        kind: ExprKind::F64(0.0),
                        assignable: false,
                        typ: self.type_helper.get_f64(),
                    })
                }
            }
            ast::ExprKind::StringLit(_) => todo!(),
            ast::ExprKind::BoolLit(val) => Ok(Expr {
                kind: ExprKind::Bool(val.kind == TokenKind::True),
                assignable: false,
                typ: self.type_helper.get_bool(),
            }),
            ast::ExprKind::Binary(binary) => {
                let a = self.analyze_expr(binary.a.as_ref(), expected, is_global_var)?;
                let a_typ = a.typ.clone();

                let b = self.analyze_expr(binary.b.as_ref(), Some(&a_typ), is_global_var)?;
                let b_typ = b.typ.clone();

                let matched = match binary.op.kind {
                    TokenKind::Eq | TokenKind::NotEq => a_typ == b_typ,
                    TokenKind::Plus
                    | TokenKind::Minus
                    | TokenKind::Mul
                    | TokenKind::Div
                    | TokenKind::GT
                    | TokenKind::LT
                    | TokenKind::GTEq
                    | TokenKind::LTEq => a_typ == b_typ && a_typ.borrow().is_number(),
                    TokenKind::Mod | TokenKind::BitAnd | TokenKind::BitOr | TokenKind::BitXor => {
                        a_typ == b_typ && a_typ.borrow().is_int()
                    }
                    TokenKind::Shl | TokenKind::Shr => a_typ.borrow().is_int() && b_typ.borrow().is_int(),
                    TokenKind::And | TokenKind::Or => {
                        if !a_typ.borrow().is_bool() || !b_typ.borrow().is_bool() {
                            return Err(Error::MismatchType);
                        }
                        true
                    }
                    _ => unreachable!(),
                };

                if !matched {
                    return Err(Error::MismatchType);
                }

                let bin_op = match binary.op.kind {
                    TokenKind::Eq => BinOp::Eq,
                    TokenKind::NotEq => BinOp::NotEq,
                    TokenKind::GT => BinOp::GT,
                    TokenKind::LT => BinOp::LT,
                    TokenKind::GTEq => BinOp::GTEq,
                    TokenKind::LTEq => BinOp::LTEq,
                    TokenKind::And => BinOp::And,
                    TokenKind::Or => BinOp::Or,
                    TokenKind::Plus => BinOp::Plus,
                    TokenKind::Minus => BinOp::Minus,
                    TokenKind::Mul => BinOp::Mul,
                    TokenKind::Div => BinOp::Div,
                    TokenKind::Mod => BinOp::Mod,
                    TokenKind::BitAnd => BinOp::BitAnd,
                    TokenKind::BitOr => BinOp::BitOr,
                    TokenKind::BitXor => BinOp::BitXor,
                    TokenKind::Shl => BinOp::Shl,
                    TokenKind::Shr => BinOp::Shr,
                    _ => unreachable!(),
                };

                let kind = ExprKind::Binary(Binary {
                    op: bin_op,
                    a: Box::new(a),
                    b: Box::new(b),
                });

                let t = match binary.op.kind {
                    TokenKind::Eq
                    | TokenKind::NotEq
                    | TokenKind::GT
                    | TokenKind::LT
                    | TokenKind::GTEq
                    | TokenKind::LTEq
                    | TokenKind::And
                    | TokenKind::Or => self.type_helper.get_bool(),
                    TokenKind::Plus
                    | TokenKind::Minus
                    | TokenKind::Mul
                    | TokenKind::Div
                    | TokenKind::Mod
                    | TokenKind::BitAnd
                    | TokenKind::BitOr
                    | TokenKind::BitXor
                    | TokenKind::Shl
                    | TokenKind::Shr => a_typ,
                    _ => unreachable!(),
                };

                Ok(Expr {
                    kind,
                    assignable: false,
                    typ: t.clone(),
                })
            }
            ast::ExprKind::Unary(unary) => {
                let val = self.analyze_expr(unary.val.as_ref(), expected, is_global_var)?;
                let val_type = val.typ.clone();

                let matched = match unary.op.kind {
                    TokenKind::Not => val_type.borrow().is_bool(),
                    TokenKind::BitNot => val_type.borrow().is_int(),
                    TokenKind::Plus | TokenKind::Minus => val_type.borrow().is_number(),
                    _ => unreachable!(),
                };

                if !matched {
                    return Err(Error::MismatchType);
                }

                let op = match unary.op.kind {
                    TokenKind::Not => UnaryOp::Not,
                    TokenKind::BitNot => UnaryOp::BitNot,
                    TokenKind::Plus => UnaryOp::Plus,
                    TokenKind::Minus => UnaryOp::Minus,
                    _ => unreachable!(),
                };

                let t = match unary.op.kind {
                    TokenKind::Not => self.type_helper.get_bool(),
                    TokenKind::BitNot | TokenKind::Plus | TokenKind::Minus => val_type.clone(),
                    _ => unreachable!(),
                };

                Ok(Expr {
                    kind: ExprKind::Unary(Unary { op, val: Box::new(val) }),
                    assignable: false,
                    typ: t,
                })
            }
            ast::ExprKind::FunctionCall(func_call) => {
                if is_global_var {
                    return Err(Error::UnsupportedOperationInConstant);
                }

                let func = self.analyze_expr(func_call.func.as_ref(), expected, is_global_var)?;

                let fn_type = func.typ.borrow();
                let fn_type = if let ConcreteType::Fn(fn_type) = &*fn_type {
                    fn_type
                } else {
                    return Err(Error::NotAFn);
                };

                if fn_type.arguments.len() != func_call.args.len() {
                    return Err(Error::FnCallArgNumMismatch);
                }

                let mut args = Vec::new();
                for (i, arg) in func_call.args.iter().enumerate() {
                    let val = self.analyze_expr(arg, expected, is_global_var)?;
                    let val_type = val.typ.clone();
                    let func_type = fn_type.arguments.get(i).unwrap().typ.upgrade().unwrap();
                    if val_type != func_type {
                        return Err(Error::MismatchType);
                    }

                    args.push(val);
                }

                let return_type = if let Some(t) = &fn_type.return_type {
                    t.upgrade().unwrap()
                } else {
                    self.type_helper.get_void()
                };

                Ok(Expr {
                    kind: ExprKind::FunctionCall(FunctionCall {
                        func: Box::new(func.clone()),
                        args,
                    }),
                    assignable: false,
                    typ: return_type,
                })
            }
            ast::ExprKind::Cast(cast) => {
                let typ = self.type_helper.get(&cast.target);
                if let Some(typ) = typ {
                    self.analyze_expr(cast.val.as_ref(), Some(&typ), is_global_var)
                } else {
                    Err(Error::UndeclaredSymbol)
                }
            }
            ast::ExprKind::Selector(selector) => {
                let val = self.analyze_expr(selector.source.as_ref(), expected, is_global_var)?;
                let typ = val.typ.clone();
                let typ = typ.borrow();
                if let ConcreteType::Struct(strct) = &*typ {
                    if let Some(field) = strct.fields.get(selector.selection.value.as_ref().unwrap()) {
                        Ok(Expr {
                            kind: ExprKind::Selector(Selector {
                                source: Box::new(val),
                                selection: selector.selection.value.as_ref().unwrap().clone(),
                                selection_index: field.index,
                            }),
                            assignable: true,
                            typ: field.typ.upgrade().unwrap(),
                        })
                    } else {
                        Err(Error::NotAStruct)
                    }
                } else {
                    Err(Error::NotAStruct)
                }
            }
        }
    }

    fn setup_functions(&mut self) -> Result<(), Error> {
        let type_decls: Vec<&ast::FnDecl> = self
            .root_ast
            .declarations
            .iter()
            .filter_map(|decl| {
                if let ast::Declaration::Fn(t) = decl {
                    Some(t)
                } else {
                    None
                }
            })
            .collect();

        self.add_symbol_table_block();
        for type_decl in type_decls.iter() {
            let name = type_decl.name.value.as_ref().unwrap();
            if self.find_symbol(name).is_some() {
                return Err(Error::RedeclaredSymbol);
            }

            let typ = self
                .type_helper
                .get_fn(&type_decl.header)
                .ok_or(Error::UndeclaredSymbol)?;

            self.add_symbol(name, typ);
        }

        Ok(())
    }

    fn add_symbol_table_block(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    fn add_symbol(&mut self, name: &String, typ: Type) {
        self.symbol_table
            .last_mut()
            .unwrap()
            .insert(name.clone(), Symbol { typ: typ.clone() });
    }

    fn find_symbol(&self, name: &String) -> Option<Symbol> {
        for table in self.symbol_table.iter().rev() {
            if let Some(v) = table.get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    fn pop_symbol_table_block(&mut self) {
        self.symbol_table.pop();
    }
}
