# Magelang

Magelang is a programming language written in Rust. As for now, Magelang is only targetting web assembly.
The syntax is similar to Go and Rust. Magelang is in early development stage and doesn't have a lot of features yet.
Since it's still in early development stage, the language syntax and semantic are not stable yet and very likely
to change.

This project is in WIP state.

## Compiling

```bash
# Magelang uses MAGELANG_ROOT environment variable to find the standard library
export MAGELANG_ROOT=./magelang

cargo run -- compile <the_main_package> -o <output_path>

# Example:
cargo run -- compile examples/hello -o hello.wasm
```

## Running

Magelang produces a web assembly binary module. To run the web assembly, you can use javascript API from the browser
or any other web assembly runtime such as wasmtime. To run the web assembly module from wasmtime, you can try this:

```bash
wasmtime hello.wasm
```

Magelang also supports running package directly without compiling it first:

```
cargo run -- run examples/hello
```

## Brief Syntax

```
// You can import module using import keyword.
import fmt "std/fmt";

// Comment are wrritten using '//' prefix.

// There are 13 primitive data types:
// i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, isize, usize, bool

// Defining a function
fn gcd(a: i64, b: i64): i64 {
  // loop using while keyword
  while b != 0 {
    // create a new variable using let
    let t: i64 = b;
    b = a % b;
    a = t;
  }
  // return a value
  return a;
}

// function with `@main()` annotation is the first function executed.
@main()
fn main() {
  let a: i64 = 1260;
  let b: i64 = 165;
  // calling a function
  let c: i64 = gcd(a, b);
  fmt::print_i64(c);
  fmt::print_str("\n");
}
```

# Guide

## Installing

Download and install Go quickly with the steps described here.

| OS           | Arch    | Link                                                                                                                                  |
|--------------|---------|---------------------------------------------------------------------------------------------------------------------------------------|
| macOS        | aarch64 | [magelang-aarch64-apple-darwin.tar.xz](https://blog.jauhar.dev/magelang/release/nightly/magelang-aarch64-apple-darwin.tar.xz)         |
| macOS        | x86_64  | [magelang-x86_64-apple-darwin.tar.xz](https://blog.jauhar.dev/magelang/release/nightly/magelang-x86_64-apple-darwin.tar.xz)           |
| linux (gnu)  | x86_64  | [magelang-x86_64-unknown-linux-gnu.tar.xz](https://blog.jauhar.dev/magelang/release/nightly/magelang-x86_64-unknown-linux-gnu.tar.xz) |
| linux (musl) | x86_64  | [magelang-x86_64-unknown-linux-musl.tar.xz](https://blog.jauhar.dev/magelang/release/nightly/magelang-x86_64-unknown-linux-musl.tar.xz) |

After downloading the archive, extract it to `magelang` directory and add `$(pwd)/magelang/bin` to your path:

```
tar -xf <the_downloaded_archieve>
export PATH="$PATH:$(pwd)/magelang/bin"
```

Verify that you've installed Magelang by opening a command prompt and typing the following command:

```
magelang --version
```

## Installing From Source

```
cargo install --path magelang
```

## Hello World

```
// the main functino should be annotated with @main() annotation.
// You can call your main function anything you want, as long as it has
// @main() annotation, it will be treated as a main function.
@main()
fn the_main_function() {
  // use let to define a variable.
  let msg = "Hello, world\n";

  // we can cast a number to a pointer. In this case, we cast the number 40 to pointer to IoVec.
  // This is like allocating an IoVec struct in address 40. Note that this is not a good way
  // to allocate memory since address 40 might not be existed, or might be used to store some
  // other data. Ideally, you should write memory allocator yourself to avoid these problem.
  // But, since our case is so simple, we know that we can use address 40 to allocate IoVec.
  let iovec = 40 as *IoVec;

  // iovec.p access the address of field p in the IoVec struct. Note that it doesn't access
  // the field, but only the address. To dereference the address, you can use .* operator.
  // The line below assign msg (pointer to u8) to iovec.p field.
  iovec.p.* = msg;
  // Similarly, we also assign the len of the msg to iovec.len field.
  iovec.len.* = 13;

  // We use WASI's fd_write (https://github.com/WebAssembly/WASI/blob/main/legacy/preview1/docs.md#-iovec-record)
  // to write bytes to stdout file.
  fd_write(stdout, iovec, 1, 0 as *i32);
}

// IoVec is WASI's [Iovec](https://github.com/WebAssembly/WASI/blob/main/legacy/preview1/docs.md#-iovec-record)
// structure representation.
struct IoVec {
  p: [*]u8,
  len: i32,
}

// stdout is the file descriptor id for standard output.
let stdout: i32 = 1;

// To import web assembly function from host environment, you can use @wasm_import(...)
// annotation. The @wasm_import annotation takes two string arguments, the module and the
// name of the function you want to import.
// In this case, we imported a [WASI](https://wasi.dev/) function from host environment to 
// write bytes to file.
@wasm_import("wasi_snapshot_preview1", "fd_write")
fn fd_write(fd: i32, iovec_addr: *IoVec, count: i32, n_written_ptr: *i32): i32;
```

Save this code into a file named `hello.mg` and run `magelang compile hello -o hello.wasm`.
You should have a compiled program named `hello.wasm` in your directory now. You can run
the compiled webassembly program using webassembly runtime or load it to your host program.
To run it using wasmtime, you can run `wasmtime hello.wasm`. You should see "Hello, world"
text printed in your terminal.

As for now, Magelang doesn't have an official standard library and package manager yet. So,
we need to build everything ourself from scratch.

## Packages

Magelang has quite simple module system, a file represents a package. You can import a package
using `import <name> "path/to/package/file"` syntax. For example, let's split hello world example
above into 2 packages, "hello" package and "fmt" package. Creates two files named `hello.mg` and
`fmt.mg`:

```
// hello.mg

import fmt "fmt";

@main()
fn the_main_function() {
  let msg = "Hello again, world\n";
  fmt::print_string(msg);
}
```

```
// fmt.mg

fn print_string(msg: [*]u8) {
  let len: i32 = 0;
  while msg[len].* != 0 {
    len = len + 1;
  }

  let iovec = 40 as *IoVec;
  iovec.p.* = msg;
  iovec.len.* = len;
  fd_write(stdout, iovec, 1, 0 as *i32);
}

struct IoVec {
  p: [*]u8,
  len: i32,
}

let stdout: i32 = 1;

@wasm_import("wasi_snapshot_preview1", "fd_write")
fn fd_write(fd: i32, iovec_addr: *IoVec, count: i32, n_written_ptr: *i32): i32;
```

Now, compile it with `magelang compile hello -o hello.wasm`. You should be able to run
the `hello.wasm` file and see the hello world message.

As you can see, you can import the `fmt` package into `hello` package by writing
`import fmt "fmt"`. Here, the first `fmt` is the name of the import, we can access all
items in the `"fmt"` package using `fmt::<the_name_of_the_item>`. The second `"fmt"` is
the path to fmt package, which is just "fmt".

Magelang uses the current directory as the base path to find the package. In this case,
"fmt" package is located at `./fmt.mg`. If you put the `fmt.mg` at `./a/b/c/d/fmt.mg`,
then you need to use `import fmt "a/b/c/d/fmt` to import the fmt package.

Alternatively, you can also run the main package directly by running `magelang run hello`.

## Comments

```
// Comments are written using // prefix.
// Any character after // until the end of the line will be considered as a comment.
// Multiline comments are not supported yet.
```

## Data Types

### Primitives

As for now, there are few primitive data types supported by Magelang:

```
i8 i16 i32 i64 isize
u8 u16 u32 u64 usize
f32 f64
bool opaque void
```

### Pointers

Magelang supports two kind of pointer: a unit pointer and an array pointer. A unit pointer is
just an ordinary pointer pointing to a single value. You can dereference a unit pointer and
get the pointed value. An array pointer is a pointer pointing to zero or more items with the
same type. A unit pointer to a type `T` is `*T`, whereas an array pointer to `T` is `[*]T`.

To dereference a unit pointer `p`, you can write `p.*`. For eaxmple:
```
@main()
fn main() {
    let p: *i32 = mem::alloc::<i32>();
    p.* = 10;
    fmt::print_i32(p.*);
}
```

To deference the `i`-th element of an array pointer `p`, you can write `p[i].*`. Writing `p[i]`
will give you the address of `i`-th element, adding `.*` will dereference it. For example:

```
@main()
fn main() {
    let p: [*]i32 = mem::alloc_array::<i32>(10);
    set(p[5]);
    p[6].* = 10;
    fmt::print_i32(p[5].*);
}

fn set(value: *i32) {
    value.* = 10;
}
```

### Structs

You can define a struct using `struct` keyword like this:
```
struct Foo {
    field1: i32,
    field2: bool,
    field3: Bar,
}

struct Bar {
    field1: bool,
}
```

You can embed struct in another struct just like above example where struct `Bar` is embedded inside struct `Foo`.
However, you can't have circularly embedded struct since they will have infinite size. In order to make a circular
data structure, you need a pointer for indirection.

Accessing a field of a struct is quite strightforward, you just need to put a dot followed by the field you want to access.
For example:

```
struct Foo {
    field1: i32,
    field2: bool,
    field3: Bar,
}

struct Bar {
    field1: bool,
}

fn print_foo(foo: Foo) {
    fmt::print_i32(foo.field1);
    fmt::print_bool(foo.bar.field1);
}
```

However, accessing a field of a struct behind a pointer is a little bit different. If you have a pointer `p` to a struct.
`p.field1` won't give you the `field1` value, but it gives you the address of the `field1` field instead. If you want to
get the actual value, you need to dereference it using `.*`.

```
struct Foo {
    field1: i32,
    field2: bool,
    field3: Bar,
}

struct Bar {
    field1: bool,
}

fn print_foo(foo: *Foo) {
    fmt::print_i32(foo.field1.*);
    fmt::print_bar(foo.bar);
}

fn print_bar(bar: *Bar) {
    fmt::print_bool(bar.field1.*);
}
```

### Function

You can also have a function type defined like this:
```
let some_func: fn(i32, i64): i8 = ...;
fmt::print_u8(some_func(10, 11));
```

## Functions

```
import fmt "std/fmt";

// Here is how you create a function:
fn gcd(a: i32, b: i32): i32 {
    if b == 0 {
        return a;
    }

    // Here is how you call a function.
    let result = gcd(b, a % b);

    // Here is how you return from a function.
    return result;
}

// function annotated with @main() annotation will be executed on startup.
// there can only be one function annotated with @main() annotation.
// a main function shouldn't have any parameter or return value.
@main()
fn main() {
    // Here is how you call a function from different package.
    fmt::print_str("hello world\n");
}
```

Just like C, Magelang's function cannot be associated with any type (it's not a method) and cannot be overloaded.
But, unlike C, you can declare function in any order. You don't have too declare `foo` before `bar` in order to call it from
`bar`.

## Visibility

As for now, every top level declaration, and all struct fields are publicly visible and can be accessed from any package.

## Variables

```
// global variable are declared this way:
// note that you need to explicitly specify the variable type for global variable.
let foo: i32 = 10;

// However, you don't have to initialize them with any value. In this case, the initial
// value of a global variable is zero.
let bar: i32;

@main()
fn main() {
    // Local variable are declared this way:
    let a: i32 = 10;
    let b = 10;
    let c: i32;

    // you can assign value to a variable this way:
    a = 9;
}
```

Just like Rust, Magelang support variable shadowing for local variables:

```
@main()
fn main() {
    let a: i32 = 10;
    fmt::print_i32(a);

    // you can redeclare variable with the same name and invalidate the
    // previously declared variable with the same name.
    let a: [*]u8 = "Hello world\n";
    fmt::print_str(a);
}
```

In magelang, all variable are mutable. There is no constant variable feature (yet).

## Statements

```
if a < 10 {
    return a % 2 == 0;
} else if a == 15 {
    return false;
} else {
    return a % 3 == 1;
}
```

If statement in Magelang is quite straightforward just like other programming languages.
There is no need to put parenthesis surrounding the condition. Unlike C, the condition expression
have to be a boolean.

```
let i = 0;
let sum = 0;
while i < 100 {
    sum = sum + i;
    i = i + 1;
}

fmt::print_i32(i);
```

Use while statement to perform a loop. Magelang doesn't support for statement yet.

```
let i = 0;
let sum = 0;
while i < 100 {
    if i % 2 == 0 {
        continue;
    } else if i % 31 == 0 {
        break;
    } else {
        fmt::print_str("HAHA\n");
    }
}
```

You can use `break` and `continue` just like other programming languages.

## Expressions

Integer literals can be expressed like this:
```
100
1e2
1_000_000
0xdeadbeef
0o1234567
0b1000_1100
0777
```

Floating point literal can be expressed like this:
```
1.0
1.2e-1
3.1415
1
```

You can use `true` and `false` as bool literal.

String literal can be expressed like this:
```
"this is a string"
"new line can be represented like this: \n"
"tab can be represented by \t"
"this is a slash \\"
"you can define raw byte like this: \x00\x01"
"use \" to write quote"
```

Magelang's string is kind of similar to C, its null terminated and the type is `[*]u8`.

Similar to other programming languages, Magelang has arithmetic operator such as:
```
+ - * / % | & ^ << >> && || == > < >= <= ! ~
```

Magelang also support casting between integer and floating point types.
