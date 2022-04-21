use crate::ast;
use crate::ast::{Declaration, Root};
use crate::token::Token;
use std::collections::{HashMap, LinkedList};

use super::error::Error;

type Result<'a> = std::result::Result<(), Error<'a>>;
type DependencyList<'a> = HashMap<&'a String, LinkedList<&'a Token>>;
type TokenMap<'a> = HashMap<&'a String, &'a Token>;

// TODO: implement this without recursive function call.
pub fn analyze(root_ast: &Root) -> Result {
    let (dependency, token_map) = build_dependency_list(root_ast);
    check_undefined_ident(&dependency)?;
    check_dependency_cycle(&dependency, &token_map)?;

    Ok(())
}

fn build_dependency_list<'a>(root_ast: &'a Root) -> (DependencyList<'a>, TokenMap<'a>) {
    let mut dependency = DependencyList::new();
    let mut token_map = HashMap::new();
    let types = root_ast.declarations.iter().filter_map(|decl| {
        if let Declaration::Type(t) = decl {
            Some(t)
        } else {
            None
        }
    });

    for typ in types {
        let deps = get_type_dependency_names(&typ.typ);

        let name = typ.name.value.as_ref().unwrap();

        token_map.insert(name, &typ.name);
        dependency.insert(name, deps);
    }

    (dependency, token_map)
}

fn get_type_dependency_names<'a>(typ: &'a ast::Type) -> LinkedList<&'a Token> {
    let mut result = LinkedList::new();

    if let ast::Type::Ident(id) = typ {
        result.push_back(id);
    }

    if let ast::Type::Struct(strct) = typ {
        for param in strct.fields.iter() {
            let deps = get_type_dependency_names(&param.typ);
            for dep in deps.iter() {
                result.push_back(dep);
            }
        }
    }

    result
}

fn check_undefined_ident<'a>(dependency: &DependencyList<'a>) -> Result<'a> {
    for (_, deps) in dependency.iter() {
        for dep in deps.iter() {
            let name = dep.value.as_ref().unwrap();
            if !dependency.contains_key(name) {
                return Err(Error::UndefinedIdent { token: dep });
            }
        }
    }

    Ok(())
}

enum Status {
    Unvisited,
    Entered,
    Leaved,
}

fn check_dependency_cycle<'a>(
    dependency: &DependencyList<'a>,
    token_map: &TokenMap<'a>,
) -> Result<'a> {
    let mut ident_status = HashMap::new();
    let mut idents = Vec::new();
    let mut parents = HashMap::new();
    for name in dependency.keys() {
        ident_status.insert(name as &'a String, Status::Unvisited);
        idents.push(name);
        parents.insert(name as &'a String, None);
    }

    let mut dfs = DFS {
        dependency,
        token_map,
        ident_status,
        parents,
    };

    idents.sort();
    for ident in idents.iter() {
        dfs.dfs(ident)?;
    }

    Ok(())
}

struct DFS<'a, 'b> {
    dependency: &'b DependencyList<'a>,
    token_map: &'b TokenMap<'a>,
    ident_status: HashMap<&'a String, Status>,
    parents: HashMap<&'a String, Option<&'a String>>,
}

impl<'a, 'b> DFS<'a, 'b> {
    fn dfs(&mut self, node: &'a String) -> Result<'a> {
        self.ident_status.insert(node, Status::Entered);

        for dep in self.dependency[node].iter() {
            let dep_name = dep.value.as_ref().unwrap();
            let child_status = self.ident_status.get(dep_name).unwrap();
            match child_status {
                Status::Unvisited => {
                    self.parents.insert(dep_name, Some(node));
                    self.dfs(dep_name)?;
                }
                Status::Entered => {
                    return Err(self.build_cycle_error(node, dep_name));
                }
                Status::Leaved => {}
            }
        }

        self.ident_status.insert(node, Status::Leaved);
        Ok(())
    }

    fn build_cycle_error(&self, node_a: &'a String, node_b: &'a String) -> Error<'a> {
        let mut cycle = vec![node_b, node_a];

        let mut last_node = node_a;
        while let Some(par) = self.parents.get(last_node).unwrap() {
            cycle.push(par);
            if node_b.eq(*par) {
                break;
            }
            last_node = par;
        }

        cycle.reverse();
        let cycle = cycle
            .into_iter()
            .map(|node| *self.token_map.get(node).unwrap())
            .collect();

        Error::TypeCycle { token: cycle }
    }
}
