import wasm "std/wasm";

struct Pair { a: i32, b: i64 }
struct Box<T> { v: T }

let g: i32 = 10;

@main()
fn main() {
  test_arithmetic();
  test_bitwise();
  test_float();
  test_wrapping();
  test_global();
  test_struct_field();
  test_deref_and_index();
  test_untyped_literal();
  test_in_for_update();
  test_shift_right_assign_after_generic_type();
  test_logical();
  test_receiver_evaluated_once();
  test_deref_assign_without_spaces();
}

fn test_arithmetic() {
  let a: i32 = 5;
  a += 3;
  assert_equal::<i32>(8, a);
  a -= 2;
  assert_equal::<i32>(6, a);
  a *= 7;
  assert_equal::<i32>(42, a);
  a /= 4;
  assert_equal::<i32>(10, a);
  a %= 3;
  assert_equal::<i32>(1, a);
}

fn test_bitwise() {
  let x: i32 = 12;
  x &= 10;
  assert_equal::<i32>(8, x);
  x |= 1;
  assert_equal::<i32>(9, x);
  x ^= 3;
  assert_equal::<i32>(10, x);
  x <<= 2;
  assert_equal::<i32>(40, x);
  x >>= 1;
  assert_equal::<i32>(20, x);
}

fn test_float() {
  let f: f64 = 1.5;
  f += 2.0;
  f *= 2.0;
  f -= 1.0;
  f /= 4.0;
  assert_equal::<f64>(1.5, f);
}

fn test_wrapping() {
  let u: u8 = 250;
  u += 10;
  assert_equal::<u8>(4, u);
  let s: i8 = -128;
  s -= 1;
  assert_equal::<i8>(127, s);
}

fn test_global() {
  g = 10;
  g += 5;
  g <<= 1;
  assert_equal::<i32>(30, g);
}

fn test_struct_field() {
  let p = Pair{a: 1, b: 2};
  p.a += 10;
  p.b *= 3;
  assert_equal::<i32>(11, p.a);
  assert_equal::<i64>(6, p.b);
}

fn test_deref_and_index() {
  let p = 1024 as *Pair;
  p.* = Pair{a: 4, b: 5};
  p.a.* += 1;
  p.*.b -= 2;
  assert_equal::<i32>(5, p.a.*);
  assert_equal::<i64>(3, p.b.*);

  let arr = 2048 as [*]i32;
  arr[0].* = 7;
  arr[1].* = 8;
  arr[0].* *= arr[1].*;
  arr[1].* %= 3;
  assert_equal::<i32>(56, arr[0].*);
  assert_equal::<i32>(2, arr[1].*);
}

fn test_untyped_literal() {
  let u: u64 = 1;
  u <<= 40;
  u += 1;
  assert_equal::<u64>(1099511627777, u);
  let f: f32 = 2.0;
  f *= 1.5;
  assert_equal::<f32>(3.0, f);
}

fn test_in_for_update() {
  let sum: i32 = 0;
  for let i: i32 = 0; i < 5; i += 1 {
    sum += i;
  }
  assert_equal::<i32>(10, sum);
  for let i: i32 = 16; i > 1; i >>= 1 {
    sum -= 1;
  }
  assert_equal::<i32>(6, sum);
}

fn test_shift_right_assign_after_generic_type() {
  let b: Box<Box<i32>>= Box::<Box<i32>>{v: Box::<i32>{v: 3}};
  b.v.v >>= 1;
  assert_equal::<i32>(1, b.v.v);
}

let calls: i32 = 0;

fn count(result: bool): bool {
  calls += 1;
  return result;
}

fn test_logical() {
  let t: bool = true;
  t &&= count(false);
  assert_equal::<bool>(false, t);
  t ||= count(true);
  assert_equal::<bool>(true, t);
  assert_equal::<i32>(2, calls);

  let f: bool = false;
  f &&= count(true);
  assert_equal::<bool>(false, f);
  let t: bool = true;
  t ||= count(false);
  assert_equal::<bool>(true, t);
  assert_equal::<i32>(2, calls);
}

fn next_slot(): *i32 {
  calls += 1;
  return (3072 as [*]i32)[calls];
}

fn next_flag(): *bool {
  calls += 1;
  return (4096 as [*]bool)[calls];
}

fn test_receiver_evaluated_once() {
  let slots = 3072 as [*]i32;
  slots[1].* = 10;
  slots[2].* = 20;
  calls = 0;
  next_slot().* += 5;
  assert_equal::<i32>(1, calls);
  assert_equal::<i32>(15, slots[1].*);
  assert_equal::<i32>(20, slots[2].*);
  next_slot().* *= 3;
  assert_equal::<i32>(2, calls);
  assert_equal::<i32>(60, slots[2].*);

  let flags = 4096 as [*]bool;
  flags[1].* = true;
  flags[2].* = false;
  calls = 0;
  next_flag().* &&= false;
  next_flag().* ||= true;
  assert_equal::<i32>(2, calls);
  assert_equal::<bool>(false, flags[1].*);
  assert_equal::<bool>(true, flags[2].*);
}

fn test_deref_assign_without_spaces() {
  let p = 5120 as *i32;
  p.*=7;
  assert_equal::<bool>(true, p.*==7);
  p.**=3;
  assert_equal::<i32>(21, p.*);
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
