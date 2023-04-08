use indexmap::IndexSet;
use magelang_common::{FileLoader, SymbolId, SymbolLoader};
use magelang_syntax::{parse_string_lit, AstLoader};
use std::env;
use std::path::Path;
use std::rc::Rc;

pub struct PackageUtil<'err, 'file, 'sym, 'ast> {
    file_loader: &'file FileLoader<'err>,
    ast_loader: &'ast AstLoader<'err, 'file, 'sym>,
    symbol_loader: &'sym SymbolLoader,
}

impl<'err, 'file, 'sym, 'ast> PackageUtil<'err, 'file, 'sym, 'ast> {
    pub fn new(
        file_loader: &'file FileLoader<'err>,
        ast_loader: &'ast AstLoader<'err, 'file, 'sym>,
        symbol_loader: &'sym SymbolLoader,
    ) -> Self {
        Self {
            file_loader,
            ast_loader,
            symbol_loader,
        }
    }

    pub fn get_all_packages(&self, main_package: SymbolId) -> Vec<SymbolId> {
        let mut packages = IndexSet::<SymbolId>::new();
        packages.insert(main_package);

        let mut stack = vec![main_package];
        while let Some(package_name) = stack.pop() {
            let path = self.get_package_path(package_name);
            let file_id = self.file_loader.declare_file(path);
            let ast = self.ast_loader.get_ast(file_id);
            for import_node in ast.imports() {
                let next_pkg = parse_string_lit(&import_node.path.value);
                let next_pkg = self.symbol_loader.declare_symbol(next_pkg);
                if !packages.contains(&next_pkg) {
                    packages.insert(next_pkg.clone());
                    stack.push(next_pkg);
                }
            }
        }

        packages.into_iter().collect()
    }

    pub fn get_package_path(&self, package_name: SymbolId) -> Rc<Path> {
        let package_name = self.symbol_loader.get_symbol(package_name).unwrap();
        let mut path = env::current_dir().expect("cannot get the current directory");
        for segment in package_name.split('/') {
            path.push(segment);
        }
        path.set_extension("bst");
        path.into()
    }
}
