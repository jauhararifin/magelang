use indexmap::IndexSet;
use magelang_common::{FileLoader, SymbolId, SymbolLoader};
use magelang_syntax::{parse_string_lit, AstLoader};
use std::env;
use std::path::{Path, PathBuf};
use std::rc::Rc;

pub struct PackageUtil<'err, 'file, 'sym, 'ast> {
    file_loader: &'file FileLoader<'err>,
    ast_loader: &'ast AstLoader<'err, 'file, 'sym>,
    symbol_loader: &'sym SymbolLoader,
    stdlib_path: PathBuf,
}

const STDLIB_PATH_KEY: &str = "MAGELANG_ROOT";
const CODE_EXTENSION: &str = "mag";

impl<'err, 'file, 'sym, 'ast> PackageUtil<'err, 'file, 'sym, 'ast> {
    pub fn new(
        file_loader: &'file FileLoader<'err>,
        ast_loader: &'ast AstLoader<'err, 'file, 'sym>,
        symbol_loader: &'sym SymbolLoader,
    ) -> Self {
        let stdlib_path = std::env::var(STDLIB_PATH_KEY)
            .ok()
            .map(PathBuf::from)
            .or_else(|| {
                std::env::current_exe().ok().map(|mut v| {
                    v.pop();
                    v
                })
            })
            .unwrap_or_else(|| home::home_dir().map(|path| path.join("magelang")).unwrap());

        Self {
            file_loader,
            ast_loader,
            symbol_loader,
            stdlib_path,
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
                let next_pkg = parse_string_lit(&import_node.path.value).to_vec();
                let Ok(next_pkg) = String::from_utf8(next_pkg) else { continue; };

                let next_pkg = self.symbol_loader.declare_symbol(next_pkg);
                if !packages.contains(&next_pkg) {
                    packages.insert(next_pkg);
                    stack.push(next_pkg);
                }
            }
        }

        packages.into_iter().collect()
    }

    pub fn get_package_path(&self, package_name: SymbolId) -> Rc<Path> {
        let package_name = self.symbol_loader.get_symbol(package_name).unwrap();

        if let Some(path) = self.get_stdlib_package(&package_name) {
            return path;
        };

        let mut path = env::current_dir().expect("cannot get the current directory");
        for segment in package_name.split('/') {
            path.push(segment);
        }
        path.set_extension(CODE_EXTENSION);
        path.into()
    }

    fn get_stdlib_package(&self, package_name: &str) -> Option<Rc<Path>> {
        let package_path = PathBuf::from(package_name);
        let mut package_path = self.stdlib_path.join(package_path);
        package_path.set_extension(CODE_EXTENSION);
        if package_path.exists() {
            Some(package_path.into())
        } else {
            None
        }
    }
}
