import wasm "std/wasm";

@main()
@wasm_export("_start")
fn main() {
  test_precedence();
}

fn test_precedence() {
  assert_equal::<i32>(0, 0);
  assert_equal::<i32>(3, 1+2);
  assert_equal::<i32>(10, 2*3+4);
  assert_equal::<i32>(14, 2+3*4);

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
