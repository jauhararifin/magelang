import wasm "std/wasm";
import mem "std/mem";

var global_1: i32 = 10;

struct Foo {
  a: i32,
  b: void,
  c: i64,
}

struct Bar[T] {
  a: T,
}

@main()
fn main() {
  var x: void;

  var a = Foo{a: 10, c: 30};
  var x = a.a;
  var y = a.c;
  a.a = x;
  a.c = y;

  assert_equal[i32](10, a.a);
  assert_equal[i64](30, a.c);

  assert_equal[i32](10, global_1);
  a.b = calculate();
  assert_equal[i32](99, global_1);

  var bar = Bar[void]{};
  var x = bar.a;
  var size = wasm.size_of[Bar[void]]();
  assert_equal[usize](0, size);
}

fn calculate(): void {
  global_1 = 99;
}

fn assert(cond: bool) {
  if !cond {
    wasm.unreachable();
  }
}

fn assert_equal[T](expected: T, actual: T) {
  if expected != actual {
    wasm.unreachable();
  }
}
