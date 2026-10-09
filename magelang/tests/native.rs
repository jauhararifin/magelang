#![cfg(all(any(target_os = "linux", target_os = "macos"), target_pointer_width = "64"))]

const BACKEND: &str = "cranelift";
include!("common/native.rs");

#[test]
fn cranelift_remains_native_default() {
    let directory = source_directory("@main() fn main() {}");
    success(
        Command::new(env!("CARGO_BIN_EXE_magelang"))
            .current_dir(directory.path())
            .env("LLVM_CLANG", directory.path().join("missing-clang"))
            .args(["compile", "main", "--target", "native"])
            .output()
            .unwrap(),
    );
    success(Command::new(directory.path().join("a.out")).output().unwrap());
}
