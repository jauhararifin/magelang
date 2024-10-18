import wasm "std/wasm";

@main()
@wasm_export("_start")
fn main() {
  assert_equal::<i32>(0, 0);
  assert_equal::<i32>(3, 1+2);
  assert_equal::<i32>(10, 2*3+4);
  assert_equal::<i32>(14, 2+3*4);
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
