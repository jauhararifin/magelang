// TODO: find better name.

use std::collections::{HashMap, HashSet};

use crate::{
    ast::{self, Root},
    expr_helper::{self, ExprHelper, Symbol},
    semantic::{ConcreteType, FnHeader, Header, Name, Type, TypeDecl, VarHeader},
    type_helper::{ITypeHelper, TypeHelper},
    util,
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

impl From<expr_helper::Error> for Error {
    fn from(err: expr_helper::Error) -> Self {
        match err {
            expr_helper::Error::UndeclaredField => Error::UndeclaredField,
            expr_helper::Error::UndeclaredSymbol => Error::UndeclaredSymbol,
            expr_helper::Error::MismatchType => Error::MismatchType,
            expr_helper::Error::NotAStruct => Error::NotAStruct,
            expr_helper::Error::NotAFn => Error::NotAFn,
            expr_helper::Error::UnsupportedOperationInConstant => Error::UnsupportedOperationInConstant,
            expr_helper::Error::FnCallArgNumMismatch => Error::FnCallArgNumMismatch,
        }
    }
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

        let plan = util::build_processing_plan(&dependency_graph).map_err(|err| match err {
            util::PlanError::ImportCycle => Error::ImportCycle,
            util::PlanError::MissingDependency => Error::MissingPackage,
        })?;

        let mut pack_to_ast: HashMap<&str, Vec<&Root>> = HashMap::new();
        for root in roots.iter() {
            pack_to_ast
                .entry(&root.package_name.unwrap_str())
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
            let deps = dependency_graph.entry(root.package_name.unwrap_str()).or_default();
            for import in root.imports.iter() {
                deps.insert(import.package_name.unwrap_str());
            }
        }
        dependency_graph
    }

    fn build_header(&self, roots: &[&Root], dependencies: &[Header]) -> Result<Header, Error> {
        let mut processor = SingleHeaderProcessor::new(roots, dependencies);
        processor.process()?;
        Ok(processor.build())
    }
}

impl HeaderProcessor for SimpleHeaderProcessor {
    fn build_headers<'a>(&self, roots: &'a [Root]) -> Result<Vec<Header>, Error> {
        self.build(roots)
    }
}

struct SingleHeaderProcessor<'a> {
    roots: &'a [&'a Root],
    package_name: &'a str,
    dependencies: &'a [Header],

    types: Vec<TypeDecl>,
    vars: Vec<VarHeader>,
    functions: Vec<FnHeader>,
}

impl<'a> SingleHeaderProcessor<'a> {
    fn new(roots: &'a [&Root], dependencies: &'a [Header]) -> Self {
        let package_name = roots[0].package_name.unwrap_str();
        Self {
            roots,
            package_name,
            dependencies,

            types: Vec::new(),
            vars: Vec::new(),
            functions: Vec::new(),
        }
    }

    fn process(&mut self) -> Result<(), Error> {
        let type_helper = TypeHelper::from_headers(self.package_name, self.dependencies);

        let mut type_processor = TypeProcessor::new(self.roots, &type_helper);
        let type_aliases = type_processor.setup()?;
        for (name, typ) in type_aliases.iter() {
            self.types.push(TypeDecl {
                name: Name {
                    package: String::from(self.package_name),
                    name: String::from(*name),
                },
                typ: typ.clone(),
            });
        }

        let mut value_processor = ValueProcessor::new(self.roots, &type_helper);
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
            package_name: String::from(self.package_name),
            types: self.types,
            vars: self.vars,
            functions: self.functions,
        }
    }
}

struct TypeProcessor<'a> {
    roots: &'a [&'a ast::Root],
    type_helper: &'a dyn ITypeHelper<'a>,
}

impl<'a> TypeProcessor<'a> {
    fn new(roots: &'a [&ast::Root], type_helper: &'a dyn ITypeHelper<'a>) -> Self {
        Self { roots, type_helper }
    }

    fn setup(&mut self) -> Result<HashMap<&str, Type>, Error> {
        let dependency_graph = self.build_package_dependency()?;
        let plan = util::build_processing_plan(&dependency_graph).map_err(|err| match err {
            util::PlanError::ImportCycle => Error::ImportCycle,
            util::PlanError::MissingDependency => Error::MissingPackage,
        })?;

        Ok(self.setup_types(&plan[..]))
    }

    fn build_package_dependency(&self) -> Result<HashMap<&'a str, HashSet<&'a str>>, Error> {
        let mut dependency_graph: HashMap<&str, HashSet<&str>> = HashMap::new();

        let type_decls = self
            .roots
            .iter()
            .flat_map(|r| &r.declarations)
            .filter_map(|decl| decl.try_unwrap_type());

        for type_decl in type_decls {
            let name = type_decl.name.unwrap_str();

            if dependency_graph.contains_key(name) {
                return Err(Error::RedeclaredSymbol);
            }

            if let ast::Type::Ident(ident) = &type_decl.typ {
                dependency_graph.insert(name, HashSet::from([ident.unwrap_str()]));
            } else {
                dependency_graph.insert(name, HashSet::new());
            }
        }

        Ok(dependency_graph)
    }

    fn setup_types(&mut self, plan: &[&str]) -> HashMap<&str, Type> {
        let mut type_decls: HashMap<&str, &ast::TypeDecl> = self
            .roots
            .iter()
            .flat_map(|decl| &decl.declarations)
            .filter_map(|decl| decl.try_unwrap_type())
            .map(|decl| (decl.name.unwrap_str(), decl))
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
    roots: &'a [&'a ast::Root],
    package_name: &'a str,

    type_helper: &'b dyn ITypeHelper<'a>,
    expr_helper: ExprHelper<'a, 'b>,
}

impl<'a, 'b> ValueProcessor<'a, 'b> {
    fn new(roots: &'a [&'a ast::Root], type_helper: &'b dyn ITypeHelper<'a>) -> Self {
        let package_name = roots[0].package_name.unwrap_str();
        let expr_helper = ExprHelper::empty(package_name, type_helper);
        Self {
            roots,
            package_name,
            type_helper,
            expr_helper,
        }
    }

    fn build_headers(&self) -> (Vec<FnHeader>, Vec<VarHeader>) {
        let mut fn_result = Vec::new();
        let mut var_result = Vec::new();

        let symbols = self.expr_helper.get_all_symbols();
        for sym in symbols.iter() {
            // TODO: change to functional style.
            if self.package_name != sym.name.package {
                continue;
            }

            if let ConcreteType::Fn(fn_type) = &*sym.typ.borrow() {
                fn_result.push(FnHeader {
                    name: sym.name.clone(),
                    native: fn_type.native,
                    typ: sym.typ.clone(),
                })
            } else {
                var_result.push(VarHeader {
                    name: sym.name.clone(),
                    typ: sym.typ.clone(),
                })
            }
        }

        todo!();
    }

    fn setup(&mut self) -> Result<(), Error> {
        self.setup_values()?;
        self.setup_functions()?;
        Ok(())
    }

    fn setup_values(&mut self) -> Result<(), Error> {
        let var_decls: Vec<&ast::Var> = self
            .roots
            .iter()
            .flat_map(|root| &root.declarations)
            .filter_map(|decl| decl.try_unwrap_var())
            .collect();

        for var in var_decls.iter() {
            let typ = self.type_helper.get(&var.typ).ok_or(Error::UnresolvedType)?;

            let name = Name {
                package: String::from(self.package_name),
                name: String::from(var.name.unwrap_str()),
            };

            if self.expr_helper.find_symbol(&name).is_some() {
                return Err(Error::RedeclaredSymbol);
            }

            if let Some(val) = &var.value {
                let value = self.expr_helper.analyze_expr(val, &typ, true)?;
                let value_type = value.typ.clone();
                if &value_type != &typ {
                    return Err(Error::MismatchType);
                }
            }

            self.expr_helper.add_symbol(Symbol { name, typ: typ.clone() });
        }

        Ok(())
    }

    fn setup_functions(&mut self) -> Result<(), Error> {
        let fn_decls = self
            .roots
            .iter()
            .flat_map(|root| &root.declarations)
            .filter_map(|decl| decl.try_unwrap_func());

        for fn_decl in fn_decls {
            let name = Name {
                package: String::from(self.package_name),
                name: String::from(fn_decl.name.unwrap_str()),
            };

            if self.expr_helper.find_symbol(&name).is_some() {
                return Err(Error::RedeclaredSymbol);
            }

            let typ = self
                .type_helper
                .get_fn(&fn_decl.header)
                .ok_or(Error::UndeclaredSymbol)?;

            self.expr_helper.add_symbol(Symbol { name, typ: typ.clone() });
        }

        Ok(())
    }
}
