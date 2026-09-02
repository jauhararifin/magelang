// =====================================================
// Function definitions
// =====================================================

//syntax_error line=+3 col=4: Missing function parameter list
//syntax_error line=+2 col=4: Missing function body
//syntax_error line=+1 col=5: Missing closing ')'
fn f(

fn g(): i32 {
  return 0;
}

//syntax_error line=+2 col=7: Expected ':', but found ')'
//syntax_error line=+1 col=7: Missing function body
fn f(a)

fn g(): i32 {}

// =====================================================
// Imports
// =====================================================

//syntax_error line=+1 col=7: Expected IDENT, but found ';'
import;
//syntax_error line=+1 col=17: Expected STRING_LIT, but found ';'
import something;
import something "something";

// =====================================================
// Type expressions
// =====================================================

let _: package::sometype = 10;
let _: package::sometype<int> = 10;
let _: package::sometype<int,int> = 10;
let _: sometype = 10;
let _: *sometype = 10;
let _: *package::sometype = 10;
let _: *package::sometype<i32,package::package<i32>> = 10;
let _: *package::sometype<i32,(package::package<i32>)> = 10;
let _: [*]sometype = 10;
let _: [*]package::sometype = 10;
let _: [*]package::sometype<i32,package::package<i32>> = 10;
let _: [*]package::sometype<i32,(package::package<i32>)> = 10;
//syntax_error line=+1 col=8: Missing pointee type
let _: * = 10;
let _: *package = 10;
//syntax_error line=+1 col=19: Expected IDENT, but found '='
let _: *package:: = 10;
let _: *package::sometype = 10;
//syntax_error line=+1 col=26: Missing closing '>'
let _: *package::sometype< = 10;
//syntax_error line=+2 col=26: Missing closing '>'
//syntax_error line=+1 col=31: Expected ',', but found '='
let _: *package::sometype<i32 = 10;
let _: *package::sometype<i32> = 10;
//syntax_error line=+2 col=9: Expected '*', but found 'package'
//syntax_error line=+1 col=9: Expected ';', but found 'package'
let _: [package = 10;
//syntax_error line=+2 col=10: Expected ']', but found 'package'
//syntax_error line=+1 col=10: Expected ';', but found 'package'
let _: [*package = 10;
let _: [*]package = 10;
//syntax_error line=+1 col=10: Missing pointee type
let _: [*] = 10;
let _: i32;
//syntax_error line=+2 col=8: Missing type expression
//syntax_error line=+1 col=8: Expected ';', but found NUMBER_LIT
let _: 123 = 10;
//syntax_error line=+1 col=7: Missing type expression
let _:;

// =====================================================
// Struct definitions
// =====================================================

//syntax_error line=+1 col=8: Expected IDENT, but found '{'
struct {}
struct a
//syntax_error line=+1 col=1: Expected struct body, but found 'struct'
struct a<i32>
//syntax_error line=+2 col=1: Expected struct body, but found 'struct'
//syntax_error line=+1 col=8: Expected IDENT, but found '<'
struct <i32>{}
struct a<i32>{field1: type1}

// =====================================================
// Value expressions
// =====================================================

let a: i32 = 10;
let a: i32 = 10 + 20 * (30 - 1) / 2 + 3 >> 5 as i32;
let a: bool = !!(false && true);
let a: i32 = SomeStruct{a: 10};
let a: i32 = pkg::SomeStruct{a: 10};
let a: i32 = pkg::SomeStruct::<a,b,c>{a: 10};
let a: i32 = pkg::some_func::<i32>(a, b)[1].*;
let a: f32 = 1.0 + 2.0;
let a: [*]u8 = "some string";
let a: i32 = a < b;
//syntax_error line=+1 col=17: Expected expression, but found ';'
let a: i32 = a +;

// =====================================================
// Signatures
// =====================================================

//syntax_error line=+1 col=3: Expected IDENT, but found ';'
fn;
//syntax_error line=+1 col=4: Missing function parameter list
fn f;
fn empty_func();
//syntax_error line=+1 col=20: Missing return type
fn missing_return():;
fn returning():i32;
fn f(a: i32, b: i32): i32;
fn func_with_typeargs<T,U>();

// =====================================================
// Statements
// =====================================================

fn f(): i32 {
    let a: i32 = 10;
    let b = 10;
    let c: i32;
    if a == 0 {
        return a;
    }
    if true {
        return a;
    } else if false && true {
        return b;
    } else {
        return c;
    }
    while a != 0 {
        a = a / 10;
        if a % 2 == 0 {
            continue;
        }
        if a == 10 {
            break;
        }
    }
    //syntax_error line=+1 col=5: Missing if body
    if (true)
        print(a);
    //syntax_error line=+1 col=5: Missing while body
    while (true)
        print(a);
}

// =====================================================
// Parser diagnostics that used to be silently accepted
// =====================================================

fn case_let_value_missing() {
    //syntax_error line=+1 col=13: Expected expression, but found ';'
    let a = ;
}

fn case_let_typed_value_missing() {
    //syntax_error line=+1 col=18: Expected expression, but found ';'
    let b: i32 = ;
}

fn case_let_type_junk() {
    //syntax_error line=+1 col=12: Missing type expression
    let c: 5;
}

fn case_let_type_keyword() {
    //syntax_error line=+1 col=12: Missing type expression
    let d: while;
}

fn case_assign_value_missing() {
    //syntax_error line=+1 col=9: Expected expression, but found ';'
    e = ;
}

fn case_unary_missing_operand() {
    //syntax_error line=+1 col=14: Expected expression, but found ';'
    let f = -;
}

fn case_else_without_block() {
    //syntax_error line=+1 col=21: Expected '{', but found 'foo'
    if true {} else foo();
}

fn case_empty_statement_is_not_an_error() {
    ;;
}

struct CaseMissingCommaField { a: i32, b: i32 }
struct CaseMissingCommaFieldBug {
    //syntax_error line=+1 col=12: Expected ',', but found 'b'
    a: i32 b: i32
}

//syntax_error line=+1 col=36: Expected ',', but found 'b'
fn case_missing_comma_param(a: i32 b: i32) {}

fn case_missing_comma_call() {
    //syntax_error line=+1 col=9: Expected ',', but found NUMBER_LIT
    f(1 2);
}

fn case_missing_comma_struct_lit() {
    //syntax_error line=+1 col=40: Expected ',', but found 'b'
    let s = CaseMissingCommaField{a: 1 b: 2};
}

fn case_missing_comma_trailing_still_ok() {
    let s = CaseMissingCommaField{a: 1, b: 2,};
}

//syntax_error line=+1 col=22: Expected ';', but found 'foo'
let global_junk: i32 foo = 10;

fn case_return_missing_value(): i32 {
    //syntax_error line=+1 col=12: Expected expression, but found ')'
    return ) ;
}

// =====================================================
// Annotations
// =====================================================

@annotation()
fn g(): i32;

//syntax_error line=+1 col=2: Expected annotation identifier, but found '('
@()
fn g(): i32;

//syntax_error line=+2 col=1: Expected annotation arguments, but found 'fn'
@annotation
fn g(): i32;

//syntax_error line=+1 col=2: Expected annotation identifier, but found '*'
@*annotation()
fn g(): i32;

//syntax_error line=+1 col=1: There is no object to annotate
@dangling_annotation()
