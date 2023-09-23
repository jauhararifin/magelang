# Magelang

Magelang is a toy programming language I build just for fun. As for now, Magelang is only targetting web assembly.
The syntax is similar to Go and Rust. Magelang is in early development stage and doesn't have a lot of features yet.

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

// main is the first function executed.
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

```
cargo install --path magelang
```

## Hello World

```
// the main function should be annotated with @main() annotation.
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

# TODOs

- [ ] Support externref type for web assembly
- [ ] Char literal
- [ ] For loops
- [ ] Public and private modifiers
- [ ] Support incremental compilation

