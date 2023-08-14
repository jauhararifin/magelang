use magelang_syntax::{parse, ErrorManager, FileManager};
use std::fs::{read_dir, read_to_string};
use std::path::PathBuf;

#[test]
fn test_parsing() {
    let crate_dir = env!("CARGO_MANIFEST_DIR");
    let mut testdata_path = PathBuf::from(crate_dir);
    testdata_path.push("data/");

    let paths = read_dir(&testdata_path).expect("cannot read test data directory");
    for path in paths {
        let mut path = path.unwrap().path();
        let Some(ext) = path.extension() else { continue };
        if ext != "mg" {
            continue;
        }

        let mut error_manager = ErrorManager::default();
        let mut file_manager = FileManager::default();
        let file = file_manager
            .open(path.clone())
            .expect("cannot open file {path:?}");
        parse(&error_manager, &file);

        let mut error_str = String::default();
        for err in error_manager.take() {
            let location = file_manager.location(err.pos);
            let message = &err.message;
            error_str.push_str(&format!("{location}: {message}\n"));
        }

        let mut expected_error = String::default();
        path.set_extension("err");
        if path.exists() {
            expected_error =
                read_to_string(&path).expect("cannot read expected error file: {path:?}");
        }

        assert_eq!(expected_error, error_str);
    }
}
