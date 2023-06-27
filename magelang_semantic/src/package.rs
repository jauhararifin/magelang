use crate::ast::{AstDb, AstInfo};
use crate::error::ErrorAccumulator;
use crate::symbol::{SymbolDb, SymbolId};
use std::env;
use std::path::{Path, PathBuf};
use std::rc::Rc;

const STDLIB_PATH_KEY: &str = "MAGELANG_ROOT";
const CODE_EXTENSION: &str = "mg";

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct PackageId(SymbolId);

impl From<SymbolId> for PackageId {
    fn from(value: SymbolId) -> Self {
        Self(value)
    }
}

impl From<PackageId> for SymbolId {
    fn from(value: PackageId) -> Self {
        value.0
    }
}

pub trait PackageDb: AstDb + SymbolDb + ErrorAccumulator {
    fn get_stdlib_path(&self) -> Rc<Path>;
    fn get_package_path(&self, package_id: PackageId) -> Rc<Path>;
    fn get_package_ast(&self, package_id: PackageId) -> Rc<AstInfo>;
}

pub fn get_package_path(db: &impl PackageDb, package_name: PackageId) -> Rc<Path> {
    let package_name = db.get_symbol(package_name.0);

    if let Some(path) = get_stdlib_package_path(db, &package_name) {
        return path;
    };

    let mut path = env::current_dir().expect("cannot get the current directory");
    for segment in package_name.split('/') {
        path.push(segment);
    }
    path.set_extension(CODE_EXTENSION);
    path.into()
}

fn get_stdlib_package_path(db: &impl PackageDb, package_name: &str) -> Option<Rc<Path>> {
    let package_path = PathBuf::from(package_name);
    let mut package_path = db.get_stdlib_path().join(package_path);
    package_path.set_extension(CODE_EXTENSION);
    if package_path.exists() {
        Some(package_path.into())
    } else {
        None
    }
}

pub fn get_stdlib_path() -> Rc<Path> {
    get_stdlib_path_from_cargo()
        .or_else(get_stdlib_path_from_env)
        .or_else(get_stdlib_path_from_current_exe)
        .unwrap_or_else(get_stdlib_path_from_current_home)
        .into()
}

fn get_stdlib_path_from_cargo() -> Option<PathBuf> {
    let pathbuf = std::env::var("CARGO_MANIFEST_DIR").map(PathBuf::from).ok()?;
    let pathbuf = pathbuf.join("lib/");
    if !pathbuf.exists() {
        return None;
    }
    if !pathbuf.is_dir() {
        return None;
    }
    Some(pathbuf)
}

fn get_stdlib_path_from_env() -> Option<PathBuf> {
    let pathbuf = std::env::var(STDLIB_PATH_KEY).map(PathBuf::from).ok()?;
    let pathbuf = pathbuf.join("lib/");
    if !pathbuf.exists() {
        return None;
    }
    if !pathbuf.is_dir() {
        return None;
    }
    Some(pathbuf)
}

fn get_stdlib_path_from_current_exe() -> Option<PathBuf> {
    let mut pathbuf = std::env::current_exe().ok()?;
    pathbuf.pop();
    let pathbuf = pathbuf.join("lib/");
    if !pathbuf.exists() {
        return None;
    }
    if !pathbuf.is_dir() {
        return None;
    }
    Some(pathbuf)
}

fn get_stdlib_path_from_current_home() -> PathBuf {
    let pathbuf = home::home_dir()
        .map(|path| path.join("magelang"))
        .expect("cannot get home directory");
    pathbuf.join("lib/")
}

pub fn get_ast_by_package(db: &impl PackageDb, package_id: PackageId) -> Rc<AstInfo> {
    let path = db.get_package_path(package_id);
    let path_id = db.define_path(path.clone());
    db.get_ast_by_path(path_id)
}
