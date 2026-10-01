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
// Global definitions
// =====================================================

//syntax_error line=+1 col=17: Expected ';', but found 'unexpected'
let answer: i32 unexpected tokens = 42;
let next: i32 = 7;
//syntax_error line=+1 col=18: Missing initializer value expression
let empty: i32 = ;
//syntax_error line=+1 col=19: Missing unary operand
let unary: i32 = +;
let recovered: i32 = 1;

// =====================================================
// Type expressions
// =====================================================

fn invalid_keyword_types() {
    //syntax_error line=+1 col=12: Expected type expression, but found 'if'
    let x: if;
    //syntax_error line=+1 col=12: Expected type expression, but found 'return'
    let y: return;
    let recovered: i32 = 1;
}

let _: package.sometype = 10;
let _: package.sometype<int> = 10;
let _: package.sometype<int,int> = 10;
let _: sometype = 10;
let _: *sometype = 10;
let _: *package.sometype = 10;
let _: *package.sometype<i32,package.package<i32>> = 10;
let _: *package.sometype<i32,(package.package<i32>)> = 10;
let _: [*]sometype = 10;
let _: [*]package.sometype = 10;
let _: [*]package.sometype<i32,package.package<i32>> = 10;
let _: [*]package.sometype<i32,(package.package<i32>)> = 10;
//syntax_error line=+1 col=8: Missing pointee type
let _: * = 10;
let _: *package = 10;
//syntax_error line=+1 col=18: Expected IDENT, but found '='
let _: *package. = 10;
let _: *package.sometype = 10;
//syntax_error line=+1 col=25: Missing closing '>'
let _: *package.sometype< = 10;
//syntax_error line=+1 col=25: Missing closing '>'
let _: *package.sometype<i32 = 10;
let _: *package.sometype<i32> = 10;
//syntax_error line=+1 col=9: Expected '*', but found 'package'
let _: [package = 10;
//syntax_error line=+1 col=10: Expected ']', but found 'package'
let _: [*package = 10;
let _: [*]package = 10;
//syntax_error line=+1 col=10: Missing pointee type
let _: [*] = 10;
let _: i32;
//syntax_error line=+1 col=17: Expected ',', but found 'bool'
let _: Pair<i32 bool>;
//syntax_error line=+2 col=12: Missing at least one type argument
//syntax_error line=+1 col=13: Expected list item, but found ','
let _: Pair<,>;
//syntax_error line=+1 col=12: Missing at least one type argument
let _: Pair<>;
//syntax_error line=+1 col=17: Missing at least one type argument
let _: Pair<Pair<>>;
let _: Pair<i32,>;
//syntax_error line=+1 col=9: Missing grouped type
let _: ();
//syntax_error line=+1 col=15: Expected ',', but found 'i32'
let _: fn(i32 i32);
//syntax_error line=+1 col=11: Expected list item, but found ','
let _: fn(,);
//syntax_error line=+1 col=17: Missing parameter type
let _: fn(value:): i32;
//syntax_error line=+1 col=8: Missing type expression
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
//syntax_error line=+1 col=20: Missing at least one type parameter
struct EmptyGeneric<> {}
//syntax_error line=+1 col=34: Expected ',', but found 'right'
struct MissingCommas { left: i32 right: i32 }
//syntax_error line=+1 col=33: Expected ':', but found ','
struct MissingFieldColon { value, next: i32 }
//syntax_error line=+1 col=24: Missing struct field type
struct Empty { erased: }
//syntax_error line=+1 col=34: Missing struct field type
struct Partial { good: i32, bad: }

// =====================================================
// Value expressions
// =====================================================

let a: i32 = 10;
let a: i32 = 10 + 20 * (30 - 1) / 2 + 3 >> 5 as i32;
let a: bool = !!(false && true);
let a: i32 = SomeStruct{a: 10};
let a: i32 = pkg.SomeStruct{a: 10};
let a: i32 = pkg.SomeStruct<a,b,c>{a: 10};
let a: i32 = pkg.some_func<i32>(a, b)[1].*;
let a: pkg.Pair<i32>=pkg.Pair<i32>{value: 1};
let a: bool = pkg.id<i32>==pkg.id<i32>;
let a: bool = pkg.id<pkg.Pair<i32>>==pkg.id<pkg.Pair<i32>>;
let a: i32 = pkg.id<i32>>>value;
let a: i32 = pkg.id<pkg.Pair<pkg.Pair<i32>>>>>value;
let a: bool = pkg.id<i32>=// comments are transparent to the parser
=pkg.id<i32>;
//syntax_error line=+1 col=14: Struct literal target must be a type expression
let _: i32 = 1{};
//syntax_error line=+1 col=18: Expected ',', but found NUMBER_LIT
let _: i32 = f(1 2);
let _: i32 = f(1,);
//syntax_error line=+1 col=28: Expected ',', but found 'right'
let _: Pair = Pair{left: 1 right: 2};
let _: Pair = Pair{left: 1,};
//syntax_error line=+1 col=25: Expected ':', but found ','
let _: Pair = Pair{value, next: 1};
let a: f32 = 1.0 + 2.0;
let a: [*]u8 = "some string";
let a: i32 = a < b;
let _: u8 = '\0';
let _: u8 = '\x00';
//syntax_error line=+1 col=13: Character literal cannot be empty
let _: u8 = '' + 1;
//syntax_error line=+1 col=16: Expected at least one digit in 16-base integer literal
let _: i32 = 0x;
//syntax_error line=+1 col=16: Expected at least one digit in 2-base integer literal
let _: i32 = 0b;
//syntax_error line=+1 col=16: Expected at least one digit in 8-base integer literal
let _: i32 = 0o;
//syntax_error line=+1 col=17: Expected at least one digit in 16-base integer literal
let _: i32 = 0x_;
//syntax_error line=+1 col=19: Expected at least one digit in 2-base integer literal
let _: i32 = 0b___;
//syntax_error line=+1 col=17: Expected at least one digit in 8-base integer literal
let _: i32 = 0o_;
//syntax_error line=+1 col=17: Expected at least one digit in 16-base integer literal
let _: i32 = 0_x;
//syntax_error line=+1 col=20: Expected at least one digit in 2-base integer literal
let _: i32 = 0_b___;
//syntax_error line=+1 col=19: Expected at least one digit in 8-base integer literal
let _: i32 = 0__o_;
//syntax_error line=+1 col=17: Missing second operand
let a: i32 = a +;
//syntax_error line=+1 col=15: Missing grouped expression
let _: i32 = ();
//syntax_error line=+1 col=21: Missing index expression
let _: i32 = values[];
//syntax_error line=+1 col=16: Missing function argument
let _: i32 = f(, 1);
//syntax_error line=+1 col=28: Missing struct field value
let _: i32 = SomeStruct{a: };
//syntax_error line=+1 col=20: Expected ident or '*', but found ';'
let _: i32 = value.;
//syntax_error line=+1 col=15: Missing closing ')'
let _: i32 = f(1;
//syntax_error line=+1 col=22: Expected ']', but found ';'
let _: i32 = values[1;
//syntax_error line=+1 col=14: Unexpected token 'let'
let _: i32 = let;

// =====================================================
// Signatures
// =====================================================

//syntax_error line=+1 col=3: Expected IDENT, but found ';'
fn;
//syntax_error line=+1 col=4: Missing function parameter list
fn f;
fn empty_func();
//syntax_error line=+1 col=17: Missing at least one type parameter
fn empty_generic<>();
//syntax_error line=+1 col=20: Missing return type
fn missing_return():;
fn returning():i32;
fn f(a: i32, b: i32): i32;
//syntax_error line=+1 col=14: Expected ',', but found 'U'
fn generic<T U>();
//syntax_error line=+1 col=18: Expected ',', but found 'b'
fn params(a: i32 b: i32);
//syntax_error line=+1 col=17: Expected list item, but found ','
fn comma_params(,);
//syntax_error line=+1 col=12: Expected list item, but found ','
fn leading(,a: i32);
//syntax_error line=+1 col=19: Expected list item, but found ','
fn doubled(a: i32,, b: i32);
fn trailing<T,>(value: T,);
//syntax_error line=+1 col=33: Expected ':', but found ','
fn missing_parameter_colon(value,) {}
//syntax_error line=+1 col=27: Missing parameter type
fn erased_parameter(value:) {}
//syntax_error line=+1 col=27: Missing parameter type
fn partial(good: i32, bad:) {}
fn func_with_typeargs<T,U>();

// =====================================================
// Statements
// =====================================================

fn f(): i32 {
    let a: i32 = 10;
    let b = 10;
    let c: i32;
    //syntax_error line=+1 col=11: Missing local variable type
    let d:;
    //syntax_error line=+1 col=12: Missing local variable type
    let e: = 1;
    //syntax_error line=+1 col=17: Missing initial value expression
    let empty = ;
    //syntax_error line=+1 col=22: Missing initial value expression
    let typed: i32 = ;
    //syntax_error line=+1 col=9: Missing right-hand operand
    a = ;
    //syntax_error line=+1 col=10: Missing right-hand operand
    a += ;
    //syntax_error line=+1 col=18: Missing unary operand
    let unary = +;
    //syntax_error line=+1 col=10: Missing unary operand
    a = +;
    //syntax_error line=+2 col=6: Missing grouped expression
    //syntax_error line=+1 col=10: Missing right-hand operand
    () = ;
    let recovered = 1;
    //syntax_error line=+1 col=12: Expected return value expression, but found ','
    return ,;
    let recovered_after_return = 2;
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
    for let i = 0; i < 10; i = i + 1 {
        if i == 3 {
            continue;
        }
        if i == 5 {
            break;
        }
    }
    for ; a < 10; a = a + 1 {}
    for a = 0;; a = a + 1 { break; }
    for a = 0; a < 10; { a = a + 1; }
    for ;; { break; }
    for print(a); a < 10; print(a) {}
    a += 1;
    a -= 1;
    a *= 2;
    a /= 2;
    a %= 2;
    a &= 1;
    a |= 1;
    a ^= 1;
    a <<= 1;
    a >>= 1;
    a &&= b;
    a ||= b;
    p.*.x += f(a);
    p.*=v;
    p.*==v;
    p.**=2;
    for let i = 0; i < 10; i += 1 {}
    //syntax_error line=+1 col=5: Missing if body
    if (true)
        print(a);
    //syntax_error line=+1 col=5: Missing while body
    while (true)
        print(a);
    //syntax_error line=+1 col=5: Missing for body
    for ;;
        print(a);
    //syntax_error line=+1 col=23: Expected ';', but found '{'
    for a = 0; a < 10 { }
    let dummy = 0;
    //syntax_error line=+1 col=9: Missing for init statement
    for { }
    //syntax_error line=+1 col=11: Missing for condition
    for ; { }
    //syntax_error line=+1 col=12: Missing for update statement
    for ;; , { }
    let dummy = 0;
    //syntax_error line=+1 col=9: Unexpected token 'break'
    for break;; {}
    defer f();
    defer { let x = 1; f(x); };
    defer if true { f(); }
    //syntax_error line=+1 col=10: Expected deferred statement, but found ';'
    defer;
    //syntax_error line=+1 col=16: Expected deferred statement, but found ';'
    defer defer;
    //syntax_error line=+1 col=11: Expected deferred statement, but found ','
    defer ,;
    let recovered_after_defer_comma = 1;
    //syntax_error line=+1 col=11: Expected deferred statement, but found ')'
    defer );
    let recovered_after_defer_close_brac = 1;
    //syntax_error line=+1 col=11: Expected deferred statement, but found ']'
    defer ];
    let recovered_after_defer_close_square = 1;
    //syntax_error line=+1 col=11: Expected deferred statement, but found 'else'
    defer else;
    let recovered_after_defer_else = 1;
    //syntax_error line=+1 col=11: Missing if condition
    defer if ;
    let recovered_after_invalid_deferred_if = 1;
    //syntax_error line=+1 col=9: Unexpected token 'defer'
    for defer f();; {}
    for ;; let x = 1 {}
}

fn missing_deferred_statement_before_close() {
    //syntax_error line=+2 col=1: Expected deferred statement, but found '}'
    defer
}

fn missing_jump_terminators() {
    //syntax_error line=+1 col=24: Expected ';', but found 'let'
    while true { break let x = 1; }
    //syntax_error line=+1 col=24: Expected ';', but found '}'
    while true { break }
    //syntax_error line=+1 col=27: Expected ';', but found 'f'
    while true { continue f(); }
    //syntax_error line=+1 col=27: Expected ';', but found '}'
    while true { continue }
}

fn missing_return_value_before_close(): i32 {
    //syntax_error line=+2 col=1: Expected return value expression, but found '}'
    return
}

fn dangling_else_before_close() {
    //syntax_error line=+1 col=16: Missing else body
    if true {} else
}

fn invalid_else_bodies() {
    //syntax_error line=+1 col=16: Missing else body
    if true {} else ;
    //syntax_error line=+1 col=16: Missing else body
    if true {} else return;
    let recovered = 1;
}

fn nested_dangling_else() {
    //syntax_error line=+1 col=33: Missing else body
    if true {} else if false {} else
}

// =====================================================
// Annotations
// =====================================================

@annotation()
fn g(): i32;

//syntax_error line=+1 col=17: Expected ',', but found STRING_LIT
@annotation("a" "b")
fn annotation_missing_comma(): i32;

//syntax_error line=+1 col=13: Expected list item, but found ','
@annotation(,)
fn annotation_comma_only(): i32;

@annotation("a",)
fn annotation_trailing_comma(): i32;

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
