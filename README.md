# Magelang

Magelang is a toy programming language I build just for fun. As for now, Magelang is only targetting web assembly.
The syntax is similar to Go and Rust.

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

# TODOs

- [ ] Char literal
- [ ] For loops
- [ ] Public and private modifiers

