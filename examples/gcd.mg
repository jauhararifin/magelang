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
