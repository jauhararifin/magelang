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

  assert_equal::<u8>(0xff, -1 as u8);
  assert_equal::<u16>(0xffff, -1 as u16);
  assert_equal::<u32>(0xffffffff, -1 as u32);
  assert_equal::<u64>(0xffffffffffffffff, -1 as u64);
  assert_equal::<i8>(-128, -1 as i8);
  assert_equal::<i16>(-32768, -1 as i16);
  assert_equal::<i32>(-2147483648, -1 as i32);
  assert_equal::<i64>(-9223372036854775808, -1 as i64);

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

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
