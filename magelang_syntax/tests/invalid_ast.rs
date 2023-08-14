use magelang_syntax::{parse, ErrorManager, FileManager};

const TESTCASE_1: &str = r#"
fn f(

fn g(): i32 {
  return 0;
}

fn f(a)

fn g(): i32 {}

@annotation1()
fn g(): i32;

fn g(): i32 {
"#;

const EXPECTED_ERRORS: &[&'static str] = &[
    "testcase.mg:2:5: Missing closing ')'",
    "testcase.mg:8:7: Expected ':', but found ')'",
    "testcase.mg:8:7: Missing function body",
    "testcase.mg:15:13: Expected '{', but found EOF",
];

#[test]
fn test_parsing() {
    let mut error_manager = ErrorManager::default();
    let mut file_manager = FileManager::default();
    let file = file_manager.add_file("testcase.mg".into(), TESTCASE_1.to_string());
    parse(&error_manager, &file);

    let mut error_str = Vec::default();
    for err in error_manager.take() {
        let location = file_manager.location(err.pos);
        let message = &err.message;
        error_str.push(format!("{location}: {message}"));
    }
    assert_eq!(EXPECTED_ERRORS, error_str);
}
