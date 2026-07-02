import wasm "std/wasm";

let global_init_a: i64 = global_init_b;
let global_init_b: i64 = 10;
let global_init_c: i64 = global_init_from_func();
let global_func_ref: fn(): i64 = global_func_ref_target;
let global_recursive_value: i64 = recursive_without_global_dependency(3);

struct EqInner {
  a: i32,
  b: bool,
}

struct EqOuter {
  inner: EqInner,
  c: i64,
}

struct EqEmpty {}

@main()
@wasm_export("_start")
fn main() {
  test_precedence();
  test_f64_arithmetic();
  test_global_initialization_order();
  test_struct_equality();
  test_generic_equality();
  test_generic_operations();
  test_opaque_null_equality();

  test_shift_left();
  test_shift_right();
}

fn test_precedence() {
  assert_equal::<i32>(0, 0);
  assert_equal::<i32>(3, 1+2);
  assert_equal::<i32>(10, 2*3+4);
  assert_equal::<i32>(14, 2+3*4);

  let a = 10;
  assert_equal::<isize>(10, a);
  let a: i64 = 1260;
  assert_equal::<i64>(1260, a);
  let a = 1.0 / 2.0;
  let eps = a - 0.5;
  assert(eps < 0.00001 && eps > -0.00001);

  let a: i8 = -128;
  assert(a < 0);
  assert(a == -128);
  assert(a < -127);
  let a: i8 = 127;
  assert(a > 0);
  assert(a == 127);
  assert(a > 126);
  let a: u8 = -1;
  assert(a > 254);
  assert(a == 255);
  let a: u8 = 0;
  assert(a == 0);
  assert(a < 1);

  let a: i16 = -32768;
  assert(a < 0);
  assert(a == -32768);
  assert(a < -32767);
  let a: i16 = 32767;
  assert(a > 0);
  assert(a == 32767);
  assert(a > 32766);
  let a: u16 = -1;
  assert(a > 65534);
  assert(a == 65535);
  let a: u16 = 0;
  assert(a == 0);
  assert(a < 1);

  let a: i32 = -2147483648;
  assert(a < 0);
  assert(a == -2147483648);
  assert(a < -2147483647);
  let a: i32 = 2147483647;
  assert(a > 0);
  assert(a == 2147483647);
  assert(a > 2147483646);
  let a: u32 = -1;
  assert(a > 4294967294);
  assert(a == 4294967295);
  let a: u32 = 0;
  assert(a == 0);
  assert(a < 1);

  let a: i64 = -9223372036854775808;
  assert(a < 0);
  assert(a == -9223372036854775808);
  assert(a < -9223372036854775807);
  let a: i64 = 9223372036854775807;
  assert(a > 0);
  assert(a == 9223372036854775807);
  assert(a > 9223372036854775806);
  let a: u64 = -1;
  assert(a > 18446744073709551614);
  assert(a == 18446744073709551615);
  let a: u64 = 0;
  assert(a == 0);
  assert(a < 1);

  assert_equal::<i32>(-4, 1-2-3); // make sure it's (1-2)-3, not 1-(2-3)
  assert_equal::<i32>(2, 1-2+3);  // make sure it's (1-2)+3, not 1-(2+3)
  
  // testing precendence between && and ||
  assert_equal::<bool>(false, false && false || false && false);
  assert_equal::<bool>(false, false && false || false && true);
  assert_equal::<bool>(false, false && false || true && false);
  assert_equal::<bool>(true, false && false || true && true);
  assert_equal::<bool>(false, false && true || false && false);
  assert_equal::<bool>(false, false && true || false && true);
  assert_equal::<bool>(false, false && true || true && false);
  assert_equal::<bool>(true, false && true || true && true);
  assert_equal::<bool>(false, true && false || false && false);
  assert_equal::<bool>(false, true && false || false && true);
  assert_equal::<bool>(false, true && false || true && false);
  assert_equal::<bool>(true, true && false || true && true);
  assert_equal::<bool>(true, true && true || false && false);
  assert_equal::<bool>(true, true && true || false && true);
  assert_equal::<bool>(true, true && true || true && false);
  assert_equal::<bool>(true, true && true || true && true);
}

fn test_f64_arithmetic() {
  let x: f64 = 1.5;
  let y: f64 = ((x + 2.0) * 3.0) / 2.0 - 1.0;
  assert_near_f64(y, 4.25);

  let z: f64 = y - x;
  assert_near_f64(z, 2.75);
}

fn assert_near_f64(actual: f64, expected: f64) {
  let diff = actual - expected;
  assert(diff < 0.0000001 && diff > -0.0000001);
}

fn test_global_initialization_order() {
  assert_equal::<i64>(10, global_init_a);
  assert_equal::<i64>(10, global_init_b);
  assert_equal::<i64>(20, global_init_c);
  assert_equal::<i64>(3, global_recursive_value);
}

fn test_struct_equality() {
  let a = EqOuter {
    inner: EqInner { a: 1, b: true },
    c: 3,
  };
  let b = EqOuter {
    inner: EqInner { a: 1, b: true },
    c: 3,
  };
  let c = EqOuter {
    inner: EqInner { a: 2, b: true },
    c: 3,
  };
  let d = EqOuter {
    inner: EqInner { a: 1, b: true },
    c: 4,
  };

  assert_equal::<bool>(true, a == b);
  assert_equal::<bool>(false, a != b);
  assert_equal::<bool>(false, a == c);
  assert_equal::<bool>(true, a != c);
  assert_equal::<bool>(false, a == d);
  assert_equal::<bool>(true, a != d);

  let empty_a = EqEmpty {};
  let empty_b = EqEmpty {};
  assert_equal::<bool>(true, empty_a == empty_b);
  assert_equal::<bool>(false, empty_a != empty_b);
}

fn test_generic_equality() {
  let a = EqOuter {
    inner: EqInner { a: 1, b: true },
    c: 3,
  };
  let b = EqOuter {
    inner: EqInner { a: 1, b: true },
    c: 3,
  };

  assert_equal::<bool>(true, generic_eq::<i32>(1, 1));
  assert_equal::<bool>(false, generic_eq::<i32>(1, 2));
  assert_equal::<bool>(true, generic_eq::<EqOuter>(a, b));
  assert_equal::<bool>(false, generic_neq::<EqOuter>(a, b));
}

fn test_generic_operations() {
  assert_equal::<i32>(3, generic_add::<i32, i32>(1, 2));
  assert_equal::<i32>(-1, generic_sub::<i32, i32>(1, 2));
  assert_equal::<i32>(6, generic_mul::<i32, i32>(2, 3));
  assert_equal::<i32>(4, generic_div::<i32, i32>(8, 2));
  assert_equal::<i32>(8, generic_shl::<i32, i32>(1, 3));
  assert_equal::<bool>(true, generic_lt::<i32, i32>(1, 2));
  assert_equal::<bool>(false, generic_and::<bool, bool>(true, false));
  assert_equal::<bool>(false, generic_not::<bool>(true));
}

fn test_opaque_null_equality() {
  let a = null;

  assert_equal::<bool>(true, null == null);
  assert_equal::<bool>(false, null != null);
  assert_equal::<bool>(true, a == null);
  assert_equal::<bool>(false, a != null);
}

fn generic_eq<T>(a: T, b: T): bool {
  return a == b;
}

fn generic_neq<T>(a: T, b: T): bool {
  return a != b;
}

fn generic_add<T, U>(a: T, b: U): U {
  return a + b;
}

fn generic_sub<T, U>(a: T, b: U): U {
  return a - b;
}

fn generic_mul<T, U>(a: T, b: U): U {
  return a * b;
}

fn generic_div<T, U>(a: T, b: U): U {
  return a / b;
}

fn generic_shl<T, U>(a: T, b: U): T {
  return a << b;
}

fn generic_lt<T, U>(a: T, b: U): bool {
  return a < b;
}

fn generic_and<T, U>(a: T, b: U): bool {
  return a && b;
}

fn generic_not<T>(a: T): bool {
  return !a;
}

fn global_init_from_func(): i64 {
  return global_init_a + global_init_b;
}

fn global_func_ref_target(): i64 {
  let f = global_func_ref;
  return 1;
}

fn recursive_without_global_dependency(n: i64): i64 {
  if n == 0 {
    return 0;
  }
  return recursive_without_global_dependency(n - 1) + 1;
}

fn test_shift_left() {
  assert_equal::<i8>(2, 1 as i8 << 1 as i8);
  assert_equal::<i8>(2, 1 as i8 << 1 as i16);
  assert_equal::<i8>(2, 1 as i8 << 1 as i32);
  assert_equal::<i8>(2, 1 as i8 << 1 as i64);
  assert_equal::<i8>(2, 1 as i8 << 1 as isize);
  assert_equal::<i8>(2, 1 as i8 << 1 as u8);
  assert_equal::<i8>(2, 1 as i8 << 1 as u16);
  assert_equal::<i8>(2, 1 as i8 << 1 as u32);
  assert_equal::<i8>(2, 1 as i8 << 1 as u64);
  assert_equal::<i8>(2, 1 as i8 << 1 as usize);
  assert_equal::<u8>(2, 1 as u8 << 1 as i8);
  assert_equal::<u8>(2, 1 as u8 << 1 as i16);
  assert_equal::<u8>(2, 1 as u8 << 1 as i32);
  assert_equal::<u8>(2, 1 as u8 << 1 as i64);
  assert_equal::<u8>(2, 1 as u8 << 1 as isize);
  assert_equal::<u8>(2, 1 as u8 << 1 as u8);
  assert_equal::<u8>(2, 1 as u8 << 1 as u16);
  assert_equal::<u8>(2, 1 as u8 << 1 as u32);
  assert_equal::<u8>(2, 1 as u8 << 1 as u64);
  assert_equal::<u8>(2, 1 as u8 << 1 as usize);
  assert_equal::<i16>(2, 1 as i16 << 1 as i8);
  assert_equal::<i16>(2, 1 as i16 << 1 as i16);
  assert_equal::<i16>(2, 1 as i16 << 1 as i32);
  assert_equal::<i16>(2, 1 as i16 << 1 as i64);
  assert_equal::<i16>(2, 1 as i16 << 1 as isize);
  assert_equal::<i16>(2, 1 as i16 << 1 as u8);
  assert_equal::<i16>(2, 1 as i16 << 1 as u16);
  assert_equal::<i16>(2, 1 as i16 << 1 as u32);
  assert_equal::<i16>(2, 1 as i16 << 1 as u64);
  assert_equal::<i16>(2, 1 as i16 << 1 as usize);
  assert_equal::<u16>(2, 1 as u16 << 1 as i8);
  assert_equal::<u16>(2, 1 as u16 << 1 as i16);
  assert_equal::<u16>(2, 1 as u16 << 1 as i32);
  assert_equal::<u16>(2, 1 as u16 << 1 as i64);
  assert_equal::<u16>(2, 1 as u16 << 1 as isize);
  assert_equal::<u16>(2, 1 as u16 << 1 as u8);
  assert_equal::<u16>(2, 1 as u16 << 1 as u16);
  assert_equal::<u16>(2, 1 as u16 << 1 as u32);
  assert_equal::<u16>(2, 1 as u16 << 1 as u64);
  assert_equal::<u16>(2, 1 as u16 << 1 as usize);
  assert_equal::<i32>(2, 1 as i32 << 1 as i8);
  assert_equal::<i32>(2, 1 as i32 << 1 as i16);
  assert_equal::<i32>(2, 1 as i32 << 1 as i32);
  assert_equal::<i32>(2, 1 as i32 << 1 as i64);
  assert_equal::<i32>(2, 1 as i32 << 1 as isize);
  assert_equal::<i32>(2, 1 as i32 << 1 as u8);
  assert_equal::<i32>(2, 1 as i32 << 1 as u16);
  assert_equal::<i32>(2, 1 as i32 << 1 as u32);
  assert_equal::<i32>(2, 1 as i32 << 1 as u64);
  assert_equal::<i32>(2, 1 as i32 << 1 as usize);
  assert_equal::<u32>(2, 1 as u32 << 1 as i8);
  assert_equal::<u32>(2, 1 as u32 << 1 as i16);
  assert_equal::<u32>(2, 1 as u32 << 1 as i32);
  assert_equal::<u32>(2, 1 as u32 << 1 as i64);
  assert_equal::<u32>(2, 1 as u32 << 1 as isize);
  assert_equal::<u32>(2, 1 as u32 << 1 as u8);
  assert_equal::<u32>(2, 1 as u32 << 1 as u16);
  assert_equal::<u32>(2, 1 as u32 << 1 as u32);
  assert_equal::<u32>(2, 1 as u32 << 1 as u64);
  assert_equal::<u32>(2, 1 as u32 << 1 as usize);
  assert_equal::<i64>(2, 1 as i64 << 1 as i8);
  assert_equal::<i64>(2, 1 as i64 << 1 as i16);
  assert_equal::<i64>(2, 1 as i64 << 1 as i32);
  assert_equal::<i64>(2, 1 as i64 << 1 as i64);
  assert_equal::<i64>(2, 1 as i64 << 1 as isize);
  assert_equal::<i64>(2, 1 as i64 << 1 as u8);
  assert_equal::<i64>(2, 1 as i64 << 1 as u16);
  assert_equal::<i64>(2, 1 as i64 << 1 as u32);
  assert_equal::<i64>(2, 1 as i64 << 1 as u64);
  assert_equal::<i64>(2, 1 as i64 << 1 as usize);
  assert_equal::<u64>(2, 1 as u64 << 1 as i8);
  assert_equal::<u64>(2, 1 as u64 << 1 as i16);
  assert_equal::<u64>(2, 1 as u64 << 1 as i32);
  assert_equal::<u64>(2, 1 as u64 << 1 as i64);
  assert_equal::<u64>(2, 1 as u64 << 1 as isize);
  assert_equal::<u64>(2, 1 as u64 << 1 as u8);
  assert_equal::<u64>(2, 1 as u64 << 1 as u16);
  assert_equal::<u64>(2, 1 as u64 << 1 as u32);
  assert_equal::<u64>(2, 1 as u64 << 1 as u64);
  assert_equal::<u64>(2, 1 as u64 << 1 as usize);
  assert_equal::<isize>(2, 1 as isize << 1 as i8);
  assert_equal::<isize>(2, 1 as isize << 1 as i16);
  assert_equal::<isize>(2, 1 as isize << 1 as i32);
  assert_equal::<isize>(2, 1 as isize << 1 as i64);
  assert_equal::<isize>(2, 1 as isize << 1 as isize);
  assert_equal::<isize>(2, 1 as isize << 1 as u8);
  assert_equal::<isize>(2, 1 as isize << 1 as u16);
  assert_equal::<isize>(2, 1 as isize << 1 as u32);
  assert_equal::<isize>(2, 1 as isize << 1 as u64);
  assert_equal::<isize>(2, 1 as isize << 1 as usize);
  assert_equal::<usize>(2, 1 as usize << 1 as i8);
  assert_equal::<usize>(2, 1 as usize << 1 as i16);
  assert_equal::<usize>(2, 1 as usize << 1 as i32);
  assert_equal::<usize>(2, 1 as usize << 1 as i64);
  assert_equal::<usize>(2, 1 as usize << 1 as isize);
  assert_equal::<usize>(2, 1 as usize << 1 as u8);
  assert_equal::<usize>(2, 1 as usize << 1 as u16);
  assert_equal::<usize>(2, 1 as usize << 1 as u32);
  assert_equal::<usize>(2, 1 as usize << 1 as u64);
  assert_equal::<usize>(2, 1 as usize << 1 as usize);
}

fn test_shift_right() {
  assert_equal::<i8>(2, 4 as i8 >> 1 as i8);
  assert_equal::<i8>(2, 4 as i8 >> 1 as i16);
  assert_equal::<i8>(2, 4 as i8 >> 1 as i32);
  assert_equal::<i8>(2, 4 as i8 >> 1 as i64);
  assert_equal::<i8>(2, 4 as i8 >> 1 as isize);
  assert_equal::<i8>(2, 4 as i8 >> 1 as u8);
  assert_equal::<i8>(2, 4 as i8 >> 1 as u16);
  assert_equal::<i8>(2, 4 as i8 >> 1 as u32);
  assert_equal::<i8>(2, 4 as i8 >> 1 as u64);
  assert_equal::<i8>(2, 4 as i8 >> 1 as usize);
  assert_equal::<u8>(2, 4 as u8 >> 1 as i8);
  assert_equal::<u8>(2, 4 as u8 >> 1 as i16);
  assert_equal::<u8>(2, 4 as u8 >> 1 as i32);
  assert_equal::<u8>(2, 4 as u8 >> 1 as i64);
  assert_equal::<u8>(2, 4 as u8 >> 1 as isize);
  assert_equal::<u8>(2, 4 as u8 >> 1 as u8);
  assert_equal::<u8>(2, 4 as u8 >> 1 as u16);
  assert_equal::<u8>(2, 4 as u8 >> 1 as u32);
  assert_equal::<u8>(2, 4 as u8 >> 1 as u64);
  assert_equal::<u8>(2, 4 as u8 >> 1 as usize);
  assert_equal::<i16>(2, 4 as i16 >> 1 as i8);
  assert_equal::<i16>(2, 4 as i16 >> 1 as i16);
  assert_equal::<i16>(2, 4 as i16 >> 1 as i32);
  assert_equal::<i16>(2, 4 as i16 >> 1 as i64);
  assert_equal::<i16>(2, 4 as i16 >> 1 as isize);
  assert_equal::<i16>(2, 4 as i16 >> 1 as u8);
  assert_equal::<i16>(2, 4 as i16 >> 1 as u16);
  assert_equal::<i16>(2, 4 as i16 >> 1 as u32);
  assert_equal::<i16>(2, 4 as i16 >> 1 as u64);
  assert_equal::<i16>(2, 4 as i16 >> 1 as usize);
  assert_equal::<u16>(2, 4 as u16 >> 1 as i8);
  assert_equal::<u16>(2, 4 as u16 >> 1 as i16);
  assert_equal::<u16>(2, 4 as u16 >> 1 as i32);
  assert_equal::<u16>(2, 4 as u16 >> 1 as i64);
  assert_equal::<u16>(2, 4 as u16 >> 1 as isize);
  assert_equal::<u16>(2, 4 as u16 >> 1 as u8);
  assert_equal::<u16>(2, 4 as u16 >> 1 as u16);
  assert_equal::<u16>(2, 4 as u16 >> 1 as u32);
  assert_equal::<u16>(2, 4 as u16 >> 1 as u64);
  assert_equal::<u16>(2, 4 as u16 >> 1 as usize);
  assert_equal::<i32>(2, 4 as i32 >> 1 as i8);
  assert_equal::<i32>(2, 4 as i32 >> 1 as i16);
  assert_equal::<i32>(2, 4 as i32 >> 1 as i32);
  assert_equal::<i32>(2, 4 as i32 >> 1 as i64);
  assert_equal::<i32>(2, 4 as i32 >> 1 as isize);
  assert_equal::<i32>(2, 4 as i32 >> 1 as u8);
  assert_equal::<i32>(2, 4 as i32 >> 1 as u16);
  assert_equal::<i32>(2, 4 as i32 >> 1 as u32);
  assert_equal::<i32>(2, 4 as i32 >> 1 as u64);
  assert_equal::<i32>(2, 4 as i32 >> 1 as usize);
  assert_equal::<u32>(2, 4 as u32 >> 1 as i8);
  assert_equal::<u32>(2, 4 as u32 >> 1 as i16);
  assert_equal::<u32>(2, 4 as u32 >> 1 as i32);
  assert_equal::<u32>(2, 4 as u32 >> 1 as i64);
  assert_equal::<u32>(2, 4 as u32 >> 1 as isize);
  assert_equal::<u32>(2, 4 as u32 >> 1 as u8);
  assert_equal::<u32>(2, 4 as u32 >> 1 as u16);
  assert_equal::<u32>(2, 4 as u32 >> 1 as u32);
  assert_equal::<u32>(2, 4 as u32 >> 1 as u64);
  assert_equal::<u32>(2, 4 as u32 >> 1 as usize);
  assert_equal::<i64>(2, 4 as i64 >> 1 as i8);
  assert_equal::<i64>(2, 4 as i64 >> 1 as i16);
  assert_equal::<i64>(2, 4 as i64 >> 1 as i32);
  assert_equal::<i64>(2, 4 as i64 >> 1 as i64);
  assert_equal::<i64>(2, 4 as i64 >> 1 as isize);
  assert_equal::<i64>(2, 4 as i64 >> 1 as u8);
  assert_equal::<i64>(2, 4 as i64 >> 1 as u16);
  assert_equal::<i64>(2, 4 as i64 >> 1 as u32);
  assert_equal::<i64>(2, 4 as i64 >> 1 as u64);
  assert_equal::<i64>(2, 4 as i64 >> 1 as usize);
  assert_equal::<u64>(2, 4 as u64 >> 1 as i8);
  assert_equal::<u64>(2, 4 as u64 >> 1 as i16);
  assert_equal::<u64>(2, 4 as u64 >> 1 as i32);
  assert_equal::<u64>(2, 4 as u64 >> 1 as i64);
  assert_equal::<u64>(2, 4 as u64 >> 1 as isize);
  assert_equal::<u64>(2, 4 as u64 >> 1 as u8);
  assert_equal::<u64>(2, 4 as u64 >> 1 as u16);
  assert_equal::<u64>(2, 4 as u64 >> 1 as u32);
  assert_equal::<u64>(2, 4 as u64 >> 1 as u64);
  assert_equal::<u64>(2, 4 as u64 >> 1 as usize);
  assert_equal::<isize>(2, 4 as isize >> 1 as i8);
  assert_equal::<isize>(2, 4 as isize >> 1 as i16);
  assert_equal::<isize>(2, 4 as isize >> 1 as i32);
  assert_equal::<isize>(2, 4 as isize >> 1 as i64);
  assert_equal::<isize>(2, 4 as isize >> 1 as isize);
  assert_equal::<isize>(2, 4 as isize >> 1 as u8);
  assert_equal::<isize>(2, 4 as isize >> 1 as u16);
  assert_equal::<isize>(2, 4 as isize >> 1 as u32);
  assert_equal::<isize>(2, 4 as isize >> 1 as u64);
  assert_equal::<isize>(2, 4 as isize >> 1 as usize);
  assert_equal::<usize>(2, 4 as usize >> 1 as i8);
  assert_equal::<usize>(2, 4 as usize >> 1 as i16);
  assert_equal::<usize>(2, 4 as usize >> 1 as i32);
  assert_equal::<usize>(2, 4 as usize >> 1 as i64);
  assert_equal::<usize>(2, 4 as usize >> 1 as isize);
  assert_equal::<usize>(2, 4 as usize >> 1 as u8);
  assert_equal::<usize>(2, 4 as usize >> 1 as u16);
  assert_equal::<usize>(2, 4 as usize >> 1 as u32);
  assert_equal::<usize>(2, 4 as usize >> 1 as u64);
  assert_equal::<usize>(2, 4 as usize >> 1 as usize);

  assert_equal::<i8>(-5, -10 as i8 >> 1 as i8);
  assert_equal::<i8>(-5, -10 as i8 >> 1 as i16);
  assert_equal::<i8>(-5, -10 as i8 >> 1 as i32);
  assert_equal::<i8>(-5, -10 as i8 >> 1 as i64);
  assert_equal::<i8>(-5, -10 as i8 >> 1 as isize);
  assert_equal::<i8>(-5, -10 as i8 >> 1 as u8);
  assert_equal::<i8>(-5, -10 as i8 >> 1 as u16);
  assert_equal::<i8>(-5, -10 as i8 >> 1 as u32);
  assert_equal::<i8>(-5, -10 as i8 >> 1 as u64);
  assert_equal::<i8>(-5, -10 as i8 >> 1 as usize);
  assert_equal::<i16>(-5, -10 as i16 >> 1 as i8);
  assert_equal::<i16>(-5, -10 as i16 >> 1 as i16);
  assert_equal::<i16>(-5, -10 as i16 >> 1 as i32);
  assert_equal::<i16>(-5, -10 as i16 >> 1 as i64);
  assert_equal::<i16>(-5, -10 as i16 >> 1 as isize);
  assert_equal::<i16>(-5, -10 as i16 >> 1 as u8);
  assert_equal::<i16>(-5, -10 as i16 >> 1 as u16);
  assert_equal::<i16>(-5, -10 as i16 >> 1 as u32);
  assert_equal::<i16>(-5, -10 as i16 >> 1 as u64);
  assert_equal::<i16>(-5, -10 as i16 >> 1 as usize);
  assert_equal::<i32>(-5, -10 as i32 >> 1 as i8);
  assert_equal::<i32>(-5, -10 as i32 >> 1 as i16);
  assert_equal::<i32>(-5, -10 as i32 >> 1 as i32);
  assert_equal::<i32>(-5, -10 as i32 >> 1 as i64);
  assert_equal::<i32>(-5, -10 as i32 >> 1 as isize);
  assert_equal::<i32>(-5, -10 as i32 >> 1 as u8);
  assert_equal::<i32>(-5, -10 as i32 >> 1 as u16);
  assert_equal::<i32>(-5, -10 as i32 >> 1 as u32);
  assert_equal::<i32>(-5, -10 as i32 >> 1 as u64);
  assert_equal::<i32>(-5, -10 as i32 >> 1 as usize);
  assert_equal::<i64>(-5, -10 as i64 >> 1 as i8);
  assert_equal::<i64>(-5, -10 as i64 >> 1 as i16);
  assert_equal::<i64>(-5, -10 as i64 >> 1 as i32);
  assert_equal::<i64>(-5, -10 as i64 >> 1 as i64);
  assert_equal::<i64>(-5, -10 as i64 >> 1 as isize);
  assert_equal::<i64>(-5, -10 as i64 >> 1 as u8);
  assert_equal::<i64>(-5, -10 as i64 >> 1 as u16);
  assert_equal::<i64>(-5, -10 as i64 >> 1 as u32);
  assert_equal::<i64>(-5, -10 as i64 >> 1 as u64);
  assert_equal::<i64>(-5, -10 as i64 >> 1 as usize);
  assert_equal::<isize>(-5, -10 as isize >> 1 as i8);
  assert_equal::<isize>(-5, -10 as isize >> 1 as i16);
  assert_equal::<isize>(-5, -10 as isize >> 1 as i32);
  assert_equal::<isize>(-5, -10 as isize >> 1 as i64);
  assert_equal::<isize>(-5, -10 as isize >> 1 as isize);
  assert_equal::<isize>(-5, -10 as isize >> 1 as u8);
  assert_equal::<isize>(-5, -10 as isize >> 1 as u16);
  assert_equal::<isize>(-5, -10 as isize >> 1 as u32);
  assert_equal::<isize>(-5, -10 as isize >> 1 as u64);
  assert_equal::<isize>(-5, -10 as isize >> 1 as usize);
}

fn assert(cond: bool) {
  if !cond {
    wasm::unreachable();
  }
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
