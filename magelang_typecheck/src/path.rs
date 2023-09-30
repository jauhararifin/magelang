use std::env;
use std::path::{Path, PathBuf};

const STDLIB_PATH_KEY: &str = "MAGELANG_ROOT";
const CODE_EXTENSION: &str = "mg";

pub(crate) fn get_stdlib_path() -> PathBuf {
    get_stdlib_path_from_cargo()
        .or_else(get_stdlib_path_from_env)
        .or_else(get_stdlib_path_from_current_exe)
        .unwrap_or_else(get_stdlib_path_from_current_home)
}

fn get_stdlib_path_from_cargo() -> Option<PathBuf> {
    let pathbuf = std::env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .ok()?;
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

pub(crate) fn get_package_path(stdlib_path: &Path, package_name: &str) -> PathBuf {
    if let Some(path) = get_stdlib_package_path(stdlib_path, package_name) {
        return path;
    };
    let mut path = env::current_dir().expect("cannot get the current directory");
    for segment in package_name.split('/') {
        path.push(segment);
    }
    // TODO: this is wrong, we should add the extension not changing it. Supposed we want to
    // compile package `a.b.c`. The correct file of this package should be `a.b.c.mg`, not `a.b.mg`.
    path.set_extension(CODE_EXTENSION);
    path
}

fn get_stdlib_package_path(stdlib_path: &Path, package_name: &str) -> Option<PathBuf> {
    let mut package_path = PathBuf::from(stdlib_path);
    let path = PathBuf::from(package_name);
    package_path.push(path);
    package_path.set_extension(CODE_EXTENSION);
    if package_path.exists() {
        Some(package_path)
    } else {
        None
    }
}
