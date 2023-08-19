use magelang_syntax::{parse, ErrorManager, FileManager};

fn test_parsing(source: String, expected_errors: &[(&str, &str)]) {
    let mut error_manager = ErrorManager::default();
    let mut file_manager = FileManager::default();
    let file = file_manager.add_file("testcase.mg".into(), source);
    parse(&error_manager, &file);

    let mut actual_errors = Vec::default();
    for err in error_manager.take() {
        let location = file_manager.location(err.pos);
        let message = &err.message;
        actual_errors.push((format!("{location}"), format!("{message}")));
    }

    let actual_errors: Vec<(&str, &str)> = actual_errors
        .iter()
        .map(|(loc, msg)| (loc.as_str(), msg.as_str()))
        .collect();

    assert_eq!(expected_errors, actual_errors);
}

macro_rules! testcase {
    ($name:ident, $source:expr, $expected_errors:expr) => {
        #[test]
        fn $name() {
            let source = $source.to_string();
            test_parsing(source, $expected_errors);
        }
    };
}

const TEST_FUNCTION_DEFINITION_SOURCE: &str = r#"
fn f(

fn g(): i32 {
  return 0;
}

fn f(a)

fn g(): i32 {}
"#;
const TEST_FUNCTION_DEFINITION_ERRORS: &[(&'static str, &'static str)] = &[
    ("testcase.mg:2:4", "Missing function parameter list"),
    ("testcase.mg:2:4", "Missing function body"),
    ("testcase.mg:2:5", "Missing closing ')'"),
    ("testcase.mg:8:7", "Expected ':', but found ')'"),
    ("testcase.mg:8:7", "Missing function body"),
];
testcase!(
    test_function_definition,
    TEST_FUNCTION_DEFINITION_SOURCE,
    TEST_FUNCTION_DEFINITION_ERRORS
);

const TEST_ANNOTATION_SOURCE: &str = r#"
@annotation()
fn g(): i32;

@()
fn g(): i32;

@annotation
fn g(): i32;

@dangling_annotation()
"#;
const TEST_ANNOTATION_ERRORS: &[(&'static str, &'static str)] = &[
    (
        "testcase.mg:5:2",
        "Expected annotation identifier, but found '('",
    ),
    (
        "testcase.mg:9:1",
        "Expected annotation arguments, but found 'fn'",
    ),
    ("testcase.mg:11:1", "There is no object to annotate"),
];
testcase!(
    test_annotation,
    TEST_ANNOTATION_SOURCE,
    TEST_ANNOTATION_ERRORS
);

const TEST_IMPORTS_SOURCE: &str = r#"
import;
import something;
import something "something";
"#;
const TEST_IMPORTS_ERRORS: &[(&'static str, &'static str)] = &[
    ("testcase.mg:2:7", "Expected IDENT, but found ';'"),
    ("testcase.mg:3:17", "Expected STRING_LIT, but found ';'"),
];
testcase!(test_imports, TEST_IMPORTS_SOURCE, TEST_IMPORTS_ERRORS);

const TEST_TYPE_EXPRS_SOURCE: &str = r#"
let _: package.sometype = 10;
let _: package.sometype<int> = 10;
let _: package.sometype<int,int> = 10;
let _: sometype = 10;
let _: *sometype = 10;
let _: *package.sometype = 10;
let _: *package.sometype<i32,package.package<i32> > = 10;
let _: *package.sometype<i32,(package.package<i32>)> = 10;
let _: [*]sometype = 10;
let _: [*]package.sometype = 10;
let _: [*]package.sometype<i32,package.package<i32> > = 10;
let _: [*]package.sometype<i32,(package.package<i32>)> = 10;
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
let _: [*] = 10;
let _: i32;
"#;
const TEST_TYPE_EXPRS_ERRORS: &[(&'static str, &'static str)] = &[
    ("testcase.mg:14:8", "Missing pointee type"),
    ("testcase.mg:16:18", "Expected IDENT, but found '='"),
    ("testcase.mg:18:25", "Missing closing '>'"),
    ("testcase.mg:19:25", "Missing closing '>'"),
    ("testcase.mg:21:9", "Expected '*', but found IDENT"),
    ("testcase.mg:22:10", "Expected ']', but found IDENT"),
    ("testcase.mg:24:10", "Missing pointee type"),
];
testcase!(
    test_type_exprs,
    TEST_TYPE_EXPRS_SOURCE,
    TEST_TYPE_EXPRS_ERRORS
);

const TEST_STRUCT_DEFINITIONS_SOURCE: &str = r#"
struct {}
struct a
struct a<i32>
struct <i32>{}
struct a<i32>{field1: type1}
"#;
const TEST_STRUCT_DEFINITIONS_ERROR: &[(&'static str, &'static str)] = &[
    ("testcase.mg:2:8", "Expected IDENT, but found '{'"),
    (
        "testcase.mg:4:1",
        "Expected struct body, but found 'struct'",
    ),
    (
        "testcase.mg:5:1",
        "Expected struct body, but found 'struct'",
    ),
    ("testcase.mg:5:8", "Expected IDENT, but found '<'"),
];
testcase!(
    test_struct_definitions,
    TEST_STRUCT_DEFINITIONS_SOURCE,
    TEST_STRUCT_DEFINITIONS_ERROR
);

const TEST_VALUE_EXPRS_SOURCE: &str = r#"
let a: i32 = 10;
let a: i32 = 10 + 20 * (30 - 1) / 2 + 3 >> 5 as i32;
let a: bool = !!(false && true);
let a: i32 = SomeStruct{a: 10};
let a: i32 = pkg.SomeStruct{a: 10};
let a: i32 = pkg.SomeStruct<a,b,c>{a: 10};
let a: i32 = pkg.some_func<i32>(a, b)[1].*;
let a: f32 = 1.0 + 2.0;
let a: [*]u8 = "some string";
"#;
const TEST_VALUE_EXPRS_ERROR: &[(&'static str, &'static str)] = &[];
testcase!(
    test_value_exprs,
    TEST_VALUE_EXPRS_SOURCE,
    TEST_VALUE_EXPRS_ERROR
);

const TEST_SIGNATURE_SOURCE: &str = r#"
fn;
fn f;
fn empty_func();
fn missing_return():;
fn returning():i32;
fn f(a: i32, b: i32): i32;
fn func_with_typeargs<T,U>();
"#;
const TEST_SIGNATURE_ERROR: &[(&'static str, &'static str)] = &[
    ("testcase.mg:2:3", "Expected IDENT, but found ';'"),
    ("testcase.mg:3:4", "Missing function parameter list"),
    ("testcase.mg:5:20", "Missing return type"),
];
testcase!(test_signatures, TEST_SIGNATURE_SOURCE, TEST_SIGNATURE_ERROR);
