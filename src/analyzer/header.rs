use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    analyzer::types::{AstContext, TypeHelper},
    ast,
    errors::Error,
    semantic::{FnHeader, Header, Name, Type, VarHeader},
    util,
};

use super::expr::{ExprHelper, Symbol};

pub struct HeaderCompiler {}

impl HeaderCompiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, roots: &[ast::Root]) -> Result<Vec<Header>, Error> {
        let dependency_graph = self.build_package_dependency(roots);

        let plan = util::build_processing_plan(&dependency_graph)?;

        let mut pack_to_ast: HashMap<&str, Vec<&ast::Root>> = HashMap::new();
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

    fn build_package_dependency<'a>(&self, roots: &'a [ast::Root]) -> HashMap<&'a str, HashSet<&'a str>> {
        let mut dependency_graph: HashMap<&str, HashSet<&str>> = HashMap::new();
        for root in roots.iter() {
            let deps = dependency_graph.entry(root.package_name.unwrap_str()).or_default();
            for import in root.imports.iter() {
                deps.insert(import.package_name.unwrap_str());
            }
        }
        dependency_graph
    }

    fn build_header(&self, roots: &[&ast::Root], dependencies: &[Header]) -> Result<Header, Error> {
        let package_name = roots[0].package_name.unwrap_str();

        let mut declarations: Vec<(Rc<AstContext>, &ast::Declaration)> = Vec::new();
        for root in roots.iter() {
            let ctx = Rc::new(AstContext::new(root));
            for decl in root.declarations.iter() {
                declarations.push((Rc::clone(&ctx), decl));
            }
        }

        let mut type_helper = TypeHelper::from_headers(package_name, dependencies);
        let types = self.setup_types(&mut type_helper, package_name, &declarations[..])?;

        let mut expr_helper = ExprHelper::empty(package_name, &type_helper);
        let vars = self.setup_values(&type_helper, &mut expr_helper, package_name, &declarations[..])?;

        let functions = self.setup_funcs(&type_helper, &mut expr_helper, package_name, &declarations[..])?;

        Ok(Header {
            package_name: String::from(package_name),

            types,
            vars,
            functions,
        })
    }

    fn setup_types(
        &self,
        type_helper: &mut TypeHelper,
        package_name: &str,
        declarations: &[(Rc<AstContext>, &ast::Declaration)],
    ) -> Result<Vec<Type>, Error> {
        // build the plan.
        let dependency_graph = self.build_type_dependency(declarations)?;
        let plan = util::build_processing_plan(&dependency_graph)?;

        // build (AstContext, TypeDecl) based on the plan.
        let mut type_decls: HashMap<&str, (Rc<AstContext>, &ast::TypeDecl)> = HashMap::new();
        for (ctx, decl) in declarations.iter() {
            if let ast::Declaration::Type(type_decl) = decl {
                type_decls.insert(type_decl.name.unwrap_str(), (Rc::clone(ctx), type_decl));
            }
        }
        let type_decls: Vec<(Rc<AstContext>, &ast::TypeDecl)> =
            plan.iter().map(|v| type_decls.remove(*v).unwrap()).collect();

        // populate tye types
        let mut type_results = Vec::new();
        self.populate_types(type_helper, package_name, type_decls.as_slice(), &mut type_results);
        self.populate_types(type_helper, package_name, type_decls.as_slice(), &mut type_results);

        Ok(type_results)
    }

    fn build_type_dependency<'a>(
        &self,
        declarations: &[(Rc<AstContext>, &'a ast::Declaration)],
    ) -> Result<HashMap<&'a str, HashSet<&'a str>>, Error> {
        let mut dependency_graph: HashMap<&str, HashSet<&str>> = HashMap::new();

        let type_decls = declarations
            .iter()
            .map(|item| item.1)
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

    fn populate_types(
        &self,
        type_helper: &mut TypeHelper,
        package_name: &str,
        type_decls: &[(Rc<AstContext>, &ast::TypeDecl)],
        type_results: &mut Vec<Type>,
    ) {
        for (ctx, type_decl) in type_decls.iter() {
            let type_kind = type_helper.get(ctx, &type_decl.typ);
            let typ = Type {
                name: Some(Name {
                    package: String::from(package_name),
                    name: type_decl.name.unwrap_value().clone(),
                }),
                kind: type_kind.clone(),
            };
            type_helper.add(typ.clone());
            type_results.push(typ);
        }
    }

    fn setup_values(
        &self,
        type_helper: &TypeHelper,
        expr_helper: &mut ExprHelper,
        package_name: &str,
        declarations: &[(Rc<AstContext>, &ast::Declaration)],
    ) -> Result<Vec<VarHeader>, Error> {
        let var_decls: Vec<(Rc<AstContext>, &ast::Var)> = declarations
            .iter()
            .filter(|item| item.1.is_var())
            .map(|item| (Rc::clone(&item.0), item.1.try_unwrap_var().unwrap()))
            .collect();

        let mut results = Vec::new();

        for (ctx, var) in var_decls.iter() {
            let type_kind = type_helper.get(ctx, &var.typ);
            if type_kind.is_invalid() {
                return Err(Error::UnresolvedType);
            }

            let name = Name {
                package: String::from(package_name),
                name: String::from(var.name.unwrap_str()),
            };

            if expr_helper.find_symbol(&name).is_some() {
                return Err(Error::RedeclaredSymbol);
            }

            if let Some(val) = &var.value {
                let value = expr_helper.analyze(ctx, val, &type_kind, true)?;
                let value_type = value.type_kind.clone();
                if &value_type != &type_kind {
                    return Err(Error::MismatchType);
                }
            }

            results.push(VarHeader {
                name: name.clone(),
                type_kind: type_kind.clone(),
            });
            expr_helper.add_symbol(Symbol {
                name,
                type_kind: type_kind.clone(),
            });
        }

        Ok(results)
    }

    fn setup_funcs(
        &self,
        type_helper: &TypeHelper,
        expr_helper: &mut ExprHelper,
        package_name: &str,
        declarations: &[(Rc<AstContext>, &ast::Declaration)],
    ) -> Result<Vec<FnHeader>, Error> {
        let fn_decls: Vec<(Rc<AstContext>, &ast::FnDecl)> = declarations
            .iter()
            .filter(|item| item.1.is_func())
            .map(|item| (Rc::clone(&item.0), item.1.try_unwrap_func().unwrap()))
            .collect();

        let mut results = Vec::new();

        for (ctx, func) in fn_decls.iter() {
            let name = Name {
                package: String::from(package_name),
                name: String::from(func.name.unwrap_str()),
            };

            if expr_helper.find_symbol(&name).is_some() {
                return Err(Error::RedeclaredSymbol);
            }

            let type_kind = type_helper.get_fn(ctx, &func.header);
            if type_kind.is_invalid() {
                return Err(Error::UndeclaredSymbol);
            }

            let fn_type = type_kind.unwrap_func();
            results.push(FnHeader {
                name: name.clone(),
                native: fn_type.native,
                typ: fn_type.clone(),
            });

            expr_helper.add_symbol(Symbol {
                name,
                type_kind: type_kind,
            });
        }

        Ok(results)
    }
}
