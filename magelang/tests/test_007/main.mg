import wasm "std/wasm";

@main()
fn main() {
  test_narrowing_signed_source();
  test_narrowing_unsigned_source();
  test_widening_and_reinterpreting_stay_correct();
  test_float_to_narrow_int();
  test_bitnot();
}

fn test_narrowing_signed_source() {
  let a: i32 = 300;
  assert_equal::<u8>(44, a as u8);

  let b: i32 = 200;
  assert_equal::<i8>(-56, b as i8);

  let c: i8 = -1;
  assert_equal::<u8>(255, c as u8);

  let d: i8 = -1;
  assert_equal::<u16>(65535, d as u16);

  let e: i16 = -1;
  assert_equal::<u8>(255, e as u8);

  let f: i64 = 511;
  assert_equal::<u8>(255, f as u8);

  let g: i64 = -1;
  assert_equal::<i32>(-1, g as i32);
}

fn test_narrowing_unsigned_source() {
  let a: u8 = 200;
  assert_equal::<i16>(200, a as i16);

  let b: u8 = 200;
  assert_equal::<i32>(200, b as i32);

  let c: u8 = 200;
  assert_equal::<i8>(-56, c as i8);

  let d: u32 = 70000;
  assert_equal::<u16>(4464, d as u16);

  let e: u16 = 300;
  assert_equal::<i8>(44, e as i8);
}

fn test_widening_and_reinterpreting_stay_correct() {
  let a: u8 = 255;
  assert_equal::<u16>(255, a as u16);

  let b: i8 = -1;
  assert_equal::<i16>(-1, b as i16);
  assert_equal::<i32>(-1, b as i32);
  assert_equal::<i64>(-1, b as i64);

  let c: i16 = -1;
  assert_equal::<i32>(-1, c as i32);
  assert_equal::<i64>(-1, c as i64);

  let d: u16 = 65535;
  assert_equal::<u32>(65535, d as u32);

  let e: u16 = 40000;
  assert_equal::<i32>(40000, e as i32);
}

fn test_float_to_narrow_int() {
  let a: f32 = 200.0;
  assert_equal::<i8>(-56, a as i8);

  let b: f64 = 300.9;
  assert_equal::<u8>(44, b as u8);

  let c: f64 = -300.0;
  assert_equal::<i8>(-44, c as i8);
}

fn test_bitnot() {
  let a: u8 = 0;
  assert_equal::<u8>(255, ~a);

  let b: u16 = 0;
  assert_equal::<u16>(65535, ~b);

  let c: u8 = 200;
  assert_equal::<u8>(55, ~c);

  let d: i8 = 5;
  assert_equal::<i8>(-6, ~d);

  let e: i16 = 0;
  assert_equal::<i16>(-1, ~e);

  let f: u32 = 1;
  assert_equal::<u32>(4294967294, ~f);

  let g: i64 = 0;
  assert_equal::<i64>(-1, ~g);
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
