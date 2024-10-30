import wasm "std/wasm";
import mem "std/mem";

let global_1: i32 = 10;

struct Foo {
  a: i32,
  b: void,
  c: i64,
}

struct Bar<T> {
  a: T,
}

@main()
fn main() {
  let x: void;

  let a = Foo{a: 10, c: 30};
  let x = a.a;
  let y = a.c;
  a.a = x;
  a.c = y;

  assert_equal::<i32>(10, a.a);
  assert_equal::<i64>(30, a.c);

  assert_equal::<i32>(10, global_1);
  a.b = calculate();
  assert_equal::<i32>(99, global_1);

  let bar = Bar::<void>{};
  let x = bar.a;
  let size = wasm::size_of::<Bar<void>>();
  assert_equal::<usize>(0, size);
}

fn calculate(): void {
  global_1 = 99;
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
