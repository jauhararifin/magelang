// TODO: find better name.

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::{Rc, Weak},
};

use crate::{
    ast::{self, Root},
    semantic::{
        Argument, BinOp, Binary, Expr, ExprKind, Field, FieldValue, FnHeader, FnType, FunctionCall, Header, Selector,
        StructLit, Type, TypeDecl, TypePtr, Unary, UnaryOp, VarHeader, BOOL, F32, F64, I16, I32, I64, I8, U16, U32,
        U64, U8, VOID,
    },
    token::{Token, TokenKind},
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
    fn build_headers<'a>(&self, roots: &'a[Root]) -> Result<HashMap<&'a str, Header>, Error>;
}

pub struct SimpleHeaderProcessor {}

impl SimpleHeaderProcessor {
    pub fn new() -> Self {
        Self {}
    }

    fn build<'a>(&self, roots: &'a[Root]) -> Result<HashMap<&'a str, Header>, Error> {
        let dependency_graph = self.build_package_dependency(roots);
        let plan = self.build_processing_plan(&dependency_graph)?;

        let mut pack_to_ast: HashMap<&str, Vec<&Root>> = HashMap::new();
        for root in roots.iter() {
            pack_to_ast
                .entry(&root.package_name.unwrap_value().as_str())
                .or_default()
                .push(root);
        }

        let mut headers = HashMap::new();
        for pack in plan.iter() {
            let roots = pack_to_ast.get(*pack).unwrap().as_slice();
            let header = self.build_header(roots, &headers)?;
            headers.insert(pack, header);
        }

        Ok(headers)
    }

    fn build_package_dependency<'a>(&self, roots: &'a[Root]) -> HashMap<&'a str, HashSet<&'a str>> {
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

    fn build_header(&self, roots: &[&Root], dependencies: &HashMap<&str, Header>) -> Result<Header, Error> {
        let mut processor = SingleHeaderProcessor::new(dependencies);
        for root in roots.into_iter() {
            processor.process(root)?;
        }
        Ok(processor.build())
    }
}

impl HeaderProcessor for SimpleHeaderProcessor {
    fn build_headers<'a>(&self, roots: &'a[Root]) -> Result<HashMap<&'a str, Header>, Error> {
        self.build(roots)
    }
}

struct SingleHeaderProcessor<'a> {
    package_name: Option<String>,
    dependencies: &'a HashMap<&'a str, Header>,

    types: Vec<TypeDecl>,
    vars: Vec<VarHeader>,
    functions: Vec<FnHeader>,
}

impl<'a> SingleHeaderProcessor<'a> {
    fn new(dependencies: &'a HashMap<&'a str, Header>) -> Self {
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

        let mut type_processor = TypeProcessor::new(root, self.dependencies);
        type_processor.setup()?;
        for (name, typ) in type_processor.type_alias.iter() {
            self.types.push(TypeDecl {
                name: (*name).clone(),
                typ: Rc::clone(typ),
            });
        }

        let mut value_processor = ValueProcessor::new(root, &type_processor);
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

// TODO: this TypeProcessor is so ugly, refactor it!!!
struct TypeProcessor<'a> {
    root_ast: &'a Root,
    dependencies: HashMap<&'a str, HashMap<&'a str, &'a Rc<Type>>>,

    type_i8: Rc<Type>,
    type_i16: Rc<Type>,
    type_i32: Rc<Type>,
    type_i64: Rc<Type>,
    type_u8: Rc<Type>,
    type_u16: Rc<Type>,
    type_u32: Rc<Type>,
    type_u64: Rc<Type>,
    type_f32: Rc<Type>,
    type_f64: Rc<Type>,
    type_bool: Rc<Type>,
    type_void: Rc<Type>,

    type_alias: HashMap<&'a String, Rc<Type>>,
}

impl<'a> TypeProcessor<'a> {
    pub fn new(root_ast: &'a Root, dependencies: &'a HashMap<&'a str, Header>) -> Self {
        let mut deps: HashMap<&'a str, HashMap<&'a str, &'a Rc<Type>>> = HashMap::new();
        for (pkg_name, header) in dependencies.iter() {
            let entry = deps.entry(pkg_name).or_default();
            for type_decl in header.types.iter() {
                entry.insert(type_decl.name.as_str(), &type_decl.typ);
            }
        }

        Self {
            root_ast,
            dependencies: deps,

            type_i8: Rc::new(I8),
            type_i16: Rc::new(I16),
            type_i32: Rc::new(I32),
            type_i64: Rc::new(I64),
            type_u8: Rc::new(U8),
            type_u16: Rc::new(U16),
            type_u32: Rc::new(U32),
            type_u64: Rc::new(U64),
            type_f32: Rc::new(F32),
            type_f64: Rc::new(F64),
            type_bool: Rc::new(BOOL),
            type_void: Rc::new(VOID),

            type_alias: HashMap::new(),
        }
    }

    fn setup(&mut self) -> Result<(), Error> {
        let plan = self.build_setup_plan()?;
        self.setup_types(&plan);
        Ok(())
    }

    fn build_setup_plan(&mut self) -> Result<Vec<String>, Error> {
        let type_decls: Vec<&ast::TypeDecl> = self
            .root_ast
            .declarations
            .iter()
            .filter_map(|decl| {
                if let ast::Declaration::Type(t) = decl {
                    Some(t)
                } else {
                    None
                }
            })
            .collect();

        let mut name_to_type = HashMap::new();
        for type_decl in type_decls.iter() {
            let name = type_decl.name.value.as_ref().unwrap().clone();
            if name_to_type.contains_key(&name) {
                return Err(Error::RedeclaredSymbol);
            }
            name_to_type.insert(name, type_decl.typ.clone());
        }

        let mut plan: Vec<String> = vec![];
        let mut visited = HashSet::new();

        for type_decl in type_decls.iter() {
            if visited.contains(type_decl.name.value.as_ref().unwrap()) {
                continue;
            }

            let chain = self.traverse_type_alias(&name_to_type, *type_decl)?;

            for item in chain.into_iter().rev() {
                if !visited.contains(&item) {
                    plan.push(item.clone());
                    visited.insert(item);
                }
            }
        }

        Ok(plan)
    }

    fn traverse_type_alias(
        &self,
        name_to_type: &HashMap<String, ast::Type>,
        type_decl: &ast::TypeDecl,
    ) -> Result<Vec<String>, Error> {
        let mut chain: Vec<String> = vec![];
        let mut chain_set = HashSet::new();

        let mut current_type = &type_decl.name;
        loop {
            let name = current_type.value.as_ref().unwrap();

            if chain_set.contains(name) {
                return Err(Error::CyclicType);
            }

            chain.push(name.clone());
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

    fn setup_types(&mut self, plan: &Vec<String>) {
        let mut type_decls: HashMap<&String, &ast::TypeDecl> = self
            .root_ast
            .declarations
            .iter()
            .filter_map(|decl| {
                if let ast::Declaration::Type(t) = decl {
                    let name = t.name.value.as_ref().unwrap();
                    Some((name, t))
                } else {
                    None
                }
            })
            .collect();
        let type_decls: Vec<&ast::TypeDecl> = plan.iter().map(|v| type_decls.remove(v).unwrap()).collect();

        self.populate_types(&type_decls);
        self.repopulate_struct_fields(&type_decls);
    }

    fn populate_types(&mut self, type_decls: &Vec<&'a ast::TypeDecl>) {
        for type_decl in type_decls.iter() {
            let name = type_decl.name.value.as_ref().unwrap();
            let typ = self.get_type(&type_decl.typ).unwrap();
            self.type_alias.insert(name, typ);
        }
    }

    fn repopulate_struct_fields(&self, type_decls: &Vec<&ast::TypeDecl>) {
        for type_decl in type_decls.iter() {
            let name = type_decl.name.value.as_ref().unwrap().clone();
            let ast_type = &type_decl.typ;
            let typ = self.type_alias.get(&name).unwrap().clone();

            let ast_fields = if let ast::Type::Struct(t) = ast_type {
                &t.fields
            } else {
                continue;
            };

            let fields = if let Type::Struct { fields } = typ.as_ref() {
                fields
            } else {
                unreachable!();
            };

            for ast_field in ast_fields.iter() {
                let name = ast_field.name.value.as_ref().unwrap();
                let mut typ = fields.get(name).unwrap().typ.0.borrow_mut();
                let ast_type = &ast_field.typ;

                if typ.upgrade().is_none() {
                    let t = self.get_type(ast_type).unwrap();
                    let t = Rc::downgrade(&t);
                    *typ = t;
                }
            }
        }
    }

    fn get_type(&self, typ: &ast::Type) -> Option<Rc<Type>> {
        match &typ {
            ast::Type::Primitive(token) => Some(self.get_type_from_primitive(token)),
            ast::Type::Ident(token) => self.get_type_from_ident(token),
            ast::Type::Struct(strct) => Some(self.get_struct(strct)),
            ast::Type::Selector(selector) => {
                let pkg_name = selector.package.unwrap_value();
                let name = selector.name.unwrap_value();

                let dep_header = self.dependencies.get(pkg_name.as_str());
                if dep_header.is_none() {
                    return None;
                }
                let dep_header = dep_header.unwrap();

                dep_header.get(name.as_str()).map(|t| Rc::clone(*t))
            }
        }
    }

    fn get_type_from_ident(&self, token: &Token) -> Option<Rc<Type>> {
        let name = token.value.as_ref().unwrap();
        self.type_alias.get(name).map(Rc::clone)
    }

    fn get_type_from_primitive(&self, token: &Token) -> Rc<Type> {
        match &token.kind {
            TokenKind::Bool => Rc::clone(&self.type_bool),
            TokenKind::I8 => Rc::clone(&self.type_i8),
            TokenKind::I16 => Rc::clone(&self.type_i16),
            TokenKind::I32 => Rc::clone(&self.type_i32),
            TokenKind::I64 => Rc::clone(&self.type_i64),
            TokenKind::U8 => Rc::clone(&self.type_u8),
            TokenKind::U16 => Rc::clone(&self.type_u16),
            TokenKind::U32 => Rc::clone(&self.type_u32),
            TokenKind::U64 => Rc::clone(&self.type_u64),
            TokenKind::F32 => Rc::clone(&self.type_f32),
            TokenKind::F64 => Rc::clone(&self.type_f64),
            _ => unreachable!(),
        }
    }

    fn get_struct(&self, strct: &ast::Struct) -> Rc<Type> {
        let mut fields: HashMap<String, Field> = HashMap::new();

        for (index, field) in strct.fields.iter().enumerate() {
            let name = field.name.value.as_ref().unwrap().clone();
            let typ = self.get_type(&field.typ);
            let typ = if let Some(typ) = typ {
                Rc::downgrade(&typ)
            } else {
                Weak::new()
            };
            let typ = TypePtr(RefCell::new(typ));
            fields.insert(name, Field { index, typ });
        }

        Rc::new(Type::Struct { fields })
    }

    fn get_type_by_name(&self, name: &String) -> Option<Rc<Type>> {
        self.type_alias.get(name).cloned()
    }

    fn get_fn_type(&self, header: &ast::FnHeader) -> Option<Rc<Type>> {
        let mut arguments = Vec::new();

        for (index, arg) in header.params.iter().enumerate() {
            let typ = self.get_type(&arg.typ);
            if typ.is_none() {
                return None;
            }
            let typ = typ.unwrap();

            arguments.push(Argument {
                index,
                name: arg.name.value.as_ref().unwrap().clone(),
                typ: RefCell::new(Rc::downgrade(&typ)),
            });
        }

        let return_type = if let Some(t) = &header.ret_type {
            if let Some(typ) = self.get_type(t) {
                Some(TypePtr(RefCell::new(Rc::downgrade(&typ))))
            } else {
                None
            }
        } else {
            None
        };

        let native = header.native;

        Some(Rc::new(Type::Fn(FnType {
            native,
            arguments,
            return_type,
        })))
    }
}

struct ValueProcessor<'a, 'b> {
    root_ast: &'a ast::Root,
    type_processor: &'b TypeProcessor<'a>,

    symbol_table: Vec<HashMap<String, Symbol>>,
}

#[derive(Clone, Debug)]
struct Symbol {
    typ: Rc<Type>,
}

impl<'a, 'b> ValueProcessor<'a, 'b> {
    fn new(root_ast: &'a ast::Root, type_alias_processor: &'b TypeProcessor<'a>) -> Self {
        Self {
            root_ast,
            type_processor: type_alias_processor,
            symbol_table: Vec::new(),
        }
    }

    fn build_headers(&self) -> (Vec<FnHeader>, Vec<VarHeader>) {
        let mut fn_result = Vec::new();
        let mut var_result = Vec::new();
        for table in self.symbol_table.iter() {
            for (name, symbol) in table.iter() {
                if let Type::Fn(fn_type) = symbol.typ.as_ref() {
                    fn_result.push(FnHeader {
                        name: name.clone(),
                        native: fn_type.native,
                        typ: Rc::clone(&symbol.typ),
                    })
                } else {
                    var_result.push(VarHeader {
                        name: name.clone(),
                        typ: Rc::clone(&symbol.typ),
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
            let typ = self
                .type_processor
                .get_type(&type_decl.typ)
                .ok_or(Error::UnresolvedType)?;

            if self.find_symbol(name).is_some() {
                return Err(Error::RedeclaredSymbol);
            }

            if let Some(val) = &type_decl.value {
                let value = self.analyze_expr(val, Some(&typ), true)?;
                let value_type = Rc::clone(&value.typ);
                if &value_type != &typ {
                    return Err(Error::MismatchType);
                }
            }

            self.add_symbol(name, typ);
        }

        Ok(())
    }

    fn analyze_expr(&self, expr: &ast::Expr, expected: Option<&Rc<Type>>, is_global_var: bool) -> Result<Expr, Error> {
        match &expr.kind {
            ast::ExprKind::Ident(token) => {
                let name = token.value.as_ref().unwrap();
                let symbol = self.find_symbol(name);

                if let Some(sym) = symbol {
                    Ok(Expr {
                        kind: ExprKind::Ident(name.clone()),
                        assignable: true,
                        typ: Rc::clone(&sym.typ),
                    })
                } else {
                    Err(Error::UndeclaredSymbol)
                }
            }
            ast::ExprKind::StructLit(struct_lit) => {
                let name = struct_lit.name.value.as_ref().unwrap();
                let typ = self
                    .type_processor
                    .get_type_by_name(name)
                    .ok_or(Error::UndeclaredSymbol)?;

                let struct_fields = if let Type::Struct { fields } = typ.as_ref() {
                    fields
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
                    if val.typ != type_field.typ.0.borrow().upgrade().unwrap() {
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
                let expected = expected.unwrap_or(&self.type_processor.type_i32);
                if let Type::Int { signed, size } = expected.as_ref() {
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
                        typ: Rc::clone(expected),
                    })
                } else {
                    Ok(Expr {
                        kind: ExprKind::I32(0),
                        assignable: false,
                        typ: Rc::clone(&self.type_processor.type_i32),
                    })
                }
            }
            ast::ExprKind::FloatLit(val) => {
                let expected = expected.unwrap_or(&self.type_processor.type_f64);
                if let Type::Float { size } = expected.as_ref() {
                    let kind = match size {
                        32 => ExprKind::F32(val.value.as_ref().unwrap().parse().unwrap()),
                        64 => ExprKind::F64(val.value.as_ref().unwrap().parse().unwrap()),
                        _ => unreachable!(),
                    };
                    Ok(Expr {
                        kind,
                        assignable: false,
                        typ: Rc::clone(expected),
                    })
                } else {
                    Ok(Expr {
                        kind: ExprKind::F64(0.0),
                        assignable: false,
                        typ: Rc::clone(&self.type_processor.type_f64),
                    })
                }
            }
            ast::ExprKind::StringLit(_) => todo!(),
            ast::ExprKind::BoolLit(val) => Ok(Expr {
                kind: ExprKind::Bool(val.kind == TokenKind::True),
                assignable: false,
                typ: Rc::clone(&self.type_processor.type_bool),
            }),
            ast::ExprKind::Binary(binary) => {
                let a = self.analyze_expr(binary.a.as_ref(), expected, is_global_var)?;
                let a_typ = Rc::clone(&a.typ);

                let b = self.analyze_expr(binary.b.as_ref(), Some(&a_typ), is_global_var)?;
                let b_typ = Rc::clone(&b.typ);

                let matched = match binary.op.kind {
                    TokenKind::Eq | TokenKind::NotEq => a_typ == b_typ,
                    TokenKind::Plus
                    | TokenKind::Minus
                    | TokenKind::Mul
                    | TokenKind::Div
                    | TokenKind::GT
                    | TokenKind::LT
                    | TokenKind::GTEq
                    | TokenKind::LTEq => a_typ == b_typ && a_typ.is_number(),
                    TokenKind::Mod | TokenKind::BitAnd | TokenKind::BitOr | TokenKind::BitXor => {
                        a_typ == b_typ && a_typ.is_int()
                    }
                    TokenKind::Shl | TokenKind::Shr => a_typ.is_int() && b_typ.is_int(),
                    TokenKind::And | TokenKind::Or => {
                        if !a_typ.is_bool() || !b_typ.is_bool() {
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
                    | TokenKind::Or => Rc::clone(&self.type_processor.type_bool),
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
                    typ: Rc::clone(&t),
                })
            }
            ast::ExprKind::Unary(unary) => {
                let val = self.analyze_expr(unary.val.as_ref(), expected, is_global_var)?;
                let val_type = Rc::clone(&val.typ);

                let matched = match unary.op.kind {
                    TokenKind::Not => val_type.is_bool(),
                    TokenKind::BitNot => val_type.is_int(),
                    TokenKind::Plus | TokenKind::Minus => val_type.is_number(),
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
                    TokenKind::Not => Rc::clone(&self.type_processor.type_bool),
                    TokenKind::BitNot | TokenKind::Plus | TokenKind::Minus => Rc::clone(&val_type),
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

                let fn_type = if let Type::Fn(fn_type) = func.typ.as_ref() {
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
                    let val_type = Rc::clone(&val.typ);
                    let func_type = fn_type.arguments.get(i).unwrap().typ.borrow().upgrade().unwrap();
                    if val_type != func_type {
                        return Err(Error::MismatchType);
                    }

                    args.push(val);
                }

                let return_type = if let Some(t) = &fn_type.return_type {
                    Rc::clone(&t.0.borrow().upgrade().unwrap())
                } else {
                    Rc::clone(&self.type_processor.type_void)
                };

                Ok(Expr {
                    kind: ExprKind::FunctionCall(FunctionCall {
                        func: Box::new(func),
                        args,
                    }),
                    assignable: false,
                    typ: return_type,
                })
            }
            ast::ExprKind::Cast(cast) => {
                let typ = self.type_processor.get_type(&cast.target);
                if let Some(typ) = typ {
                    self.analyze_expr(cast.val.as_ref(), Some(&typ), is_global_var)
                } else {
                    Err(Error::UndeclaredSymbol)
                }
            }
            ast::ExprKind::Selector(selector) => {
                let val = self.analyze_expr(selector.source.as_ref(), expected, is_global_var)?;
                let typ = Rc::clone(&val.typ);
                if let Type::Struct { fields } = typ.as_ref() {
                    if let Some(field) = fields.get(selector.selection.value.as_ref().unwrap()) {
                        Ok(Expr {
                            kind: ExprKind::Selector(Selector {
                                source: Box::new(val),
                                selection: selector.selection.value.as_ref().unwrap().clone(),
                                selection_index: field.index,
                            }),
                            assignable: true,
                            typ: Rc::clone(&field.typ.0.borrow().upgrade().unwrap()),
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
                .type_processor
                .get_fn_type(&type_decl.header)
                .ok_or(Error::UndeclaredSymbol)?;

            self.add_symbol(name, typ);
        }

        Ok(())
    }

    fn add_symbol_table_block(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    fn add_symbol(&mut self, name: &String, typ: Rc<Type>) {
        self.symbol_table
            .last_mut()
            .unwrap()
            .insert(name.clone(), Symbol { typ: Rc::clone(&typ) });
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
