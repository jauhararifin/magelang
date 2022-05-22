# Magelang

Magelang is a toy programming language I build just for fun.
The syntax is similar to Go and Rust.

## Brief Syntax

```
// Comment are wrritten using '//' prefix.

// There are 11 primitive data types:
// i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, bool

// Defining a function
fn gcd(a: i64, b: i64): i64 {
  // loop using while keyword
  while b != 0 {
    // create a new variable using var
    var t: i64 = b
    b = a % b
    a = t
  }
  // return a value
  return a
}

// Defining a native function.
// Native function are implemented by the VM directly. When native functions are called,
// it search the native function table inside the VM.
// Currently, there are only 2 native functions: print_int and print_char.
// In the future, this native function will function like system-call. It is used to interact with the machine.
fn native print_int(v: i64)

// main is the first function executed.
fn main() {
  var a: i64 = 1260
  var b: i64 = 165
  // calling a function
  var c: i64 = gcd(a, b)
  // calling native function
  print_int(c)
}
```
