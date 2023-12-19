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
//syntax_error line=+1 col=26: Missing closing '>'
let _: *package::sometype<i32 = 10;
let _: *package::sometype<i32> = 10;
//syntax_error line=+1 col=9: Expected '*', but found 'package'
let _: [package = 10;
//syntax_error line=+1 col=10: Expected ']', but found 'package'
let _: [*package = 10;
let _: [*]package = 10;
//syntax_error line=+1 col=10: Missing pointee type
let _: [*] = 10;
let _: i32;

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
