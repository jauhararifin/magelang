use magelang_syntax::{parse, ErrorManager, FileManager};

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

const TESTCASE_1: &str = r#"
fn f(

fn g(): i32 {
  return 0;
}

fn f(a)

fn g(): i32 {}

@annotation()
fn g(): i32;

@()
fn g(): i32;

@annotation
fn g(): i32;

import;

let global: i32 = 10;
let global = 10;

let global: package.sometype = 10;
let global: package.sometype<int> = 10;
let global: package.sometype<int,int> = 10;
let global: sometype = 10;
let global: *sometype = 10;
let global: *package.sometype = 10;
let global: *package.sometype<i32,package.package<i32> > = 10;
let global: *package.sometype<i32,(package.package<i32>)> = 10;
let global: [*]sometype = 10;
let global: [*]package.sometype = 10;
let global: [*]package.sometype<i32,package.package<i32> > = 10;
let global: [*]package.sometype<i32,(package.package<i32>)> = 10;

let _: * = 10;
let _: *package = 10;
let _: *package. = 10;
let _: *package.sometype = 10;
let _: *package.sometype< = 10;
let _: *package.sometype<i32 = 10;
let _: *package.sometype<i32> = 10;
let _: [package = 10;
let _: [*package = 10;
let _: [*]package = 10;

fn g(): i32{}
"#;

const EXPECTED_ERRORS: &[&'static str] = &[
    "testcase.mg:2:5: Missing closing ')'",
    "testcase.mg:8:7: Expected ':', but found ')'",
    "testcase.mg:8:7: Missing function body",
    "testcase.mg:15:2: Expected annotation identifier, but found '('",
    "testcase.mg:19:1: Expected annotation arguments, but found 'fn'",
    "testcase.mg:21:7: Expected IDENT, but found ';'",
    "testcase.mg:24:12: Expected ':', but found '='",
    "testcase.mg:39:8: Missing pointee type",
    "testcase.mg:41:18: Expected IDENT, but found '='",
    "testcase.mg:43:25: Missing closing '>'",
    "testcase.mg:44:25: Missing closing '>'",
    "testcase.mg:46:9: Expected '*', but found IDENT",
    "testcase.mg:47:10: Expected ']', but found IDENT",
];
