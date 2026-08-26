import wasm "std/wasm";

struct Inner { a: i64, b: i64 }
struct Big { x: i32, i: Inner, f: f64, y: i32 }
struct Tail { p: i32, q: Inner }
struct Mixed { a: i8, b: f32, c: u16, d: i64, e: f64, f: bool }
struct WithVoid { a: i32, v: void, b: i64 }

let global_big: Big = Big{x: 1, i: Inner{a: 2, b: 3}, f: 4.5, y: 6};
let counter: i32 = 0;

@main()
fn main() {
  test_field_of_call_result();
  test_nested_field_of_call_result();
  test_struct_typed_field();
  test_mixed_component_types();
  test_zero_sized_field();
  test_operands_below_the_field();
  test_local_and_global();
  test_deref();
  test_inside_loop();
  test_evaluated_once();
}

fn make_big(): Big {
  return Big{x: 1, i: Inner{a: 2, b: 3}, f: 4.5, y: 6};
}

fn make_tail(): Tail {
  return Tail{p: 7, q: Inner{a: 8, b: 9}};
}

fn make_mixed(): Mixed {
  return Mixed{a: -1, b: 1.5, c: 300, d: 1234567890123, e: 2.25, f: true};
}

fn make_with_void(): WithVoid {
  return WithVoid{a: 1, v: nothing(), b: 2};
}

fn nothing() {}

fn test_field_of_call_result() {
  assert_equal::<i32>(1, make_big().x);   // nothing before it
  assert_equal::<f64>(4.5, make_big().f); // components before and after it
  assert_equal::<i32>(6, make_big().y);   // nothing after it
}

fn test_nested_field_of_call_result() {
  assert_equal::<i64>(2, make_big().i.a);
  assert_equal::<i64>(3, make_big().i.b);
  assert_equal::<i32>(7, make_tail().p);
  assert_equal::<i64>(8, make_tail().q.a);
  assert_equal::<i64>(9, make_tail().q.b);
}

fn test_struct_typed_field() {
  let inner = make_big().i;
  assert_equal::<i64>(2, inner.a);
  assert_equal::<i64>(3, inner.b);

  let tail = make_tail().q;
  assert_equal::<i64>(8, tail.a);
  assert_equal::<i64>(9, tail.b);
}

fn test_mixed_component_types() {
  assert_equal::<i8>(-1, make_mixed().a);
  assert_equal::<f32>(1.5, make_mixed().b);
  assert_equal::<u16>(300, make_mixed().c);
  assert_equal::<i64>(1234567890123, make_mixed().d);
  assert_equal::<f64>(2.25, make_mixed().e);
  assert_equal::<bool>(true, make_mixed().f);
}

fn test_zero_sized_field() {
  assert_equal::<i32>(1, make_with_void().a);
  assert_equal::<i64>(2, make_with_void().b);
  let v: void = make_with_void().v;
  make_with_void().v;
}

fn test_operands_below_the_field() {
  assert_equal::<i64>(3, pick_middle(100, make_big().i.b, 200));
  assert_equal::<i32>(7, make_big().x + make_big().y);
  assert_equal::<i64>(5, make_big().i.a + make_big().i.b);
  assert_equal::<i32>(13, make_big().y + make_tail().p);
  assert_equal::<f64>(6.75, make_big().f + make_mixed().e);
}

fn pick_middle(a: i32, b: i64, c: i32): i64 {
  assert_equal::<i32>(100, a);
  assert_equal::<i32>(200, c);
  return b;
}

fn test_local_and_global() {
  let b = make_big();
  assert_equal::<i32>(1, b.x);
  assert_equal::<i64>(3, b.i.b);
  assert_equal::<i32>(6, b.y);

  b.y = 60;
  b.i.b = 30;
  assert_equal::<i32>(60, b.y);
  assert_equal::<i64>(30, b.i.b);
  assert_equal::<i64>(2, b.i.a);

  assert_equal::<i32>(1, global_big.x);
  assert_equal::<i64>(3, global_big.i.b);
  assert_equal::<i32>(6, global_big.y);
}

fn test_deref() {
  let p = 16384 as *Big;
  p.*.x = 11;
  p.*.i = Inner{a: 22, b: 33};
  p.*.f = 4.5;
  p.*.y = 44;

  assert_equal::<i32>(11, p.*.x);
  assert_equal::<i64>(22, p.*.i.a);
  assert_equal::<i64>(33, p.*.i.b);
  assert_equal::<f64>(4.5, p.*.f);
  assert_equal::<i32>(44, p.*.y);
}

fn test_inside_loop() {
  let i: i32 = 0;
  let sum: i64 = 0;
  while i < make_big().y - 1 {
    i = i + 1;
    if make_big().x == 1 {
      if i == 2 { continue; }
      if i == 4 { break; }
    }
    sum = sum + make_big().i.b;
  }
  assert_equal::<i32>(4, i);
  assert_equal::<i64>(6, sum); // only i == 1 and i == 3 contribute
}

fn bump(): Big {
  counter = counter + 1;
  return Big{x: counter, i: Inner{a: 2, b: 3}, f: 4.5, y: 6};
}

fn test_evaluated_once() {
  counter = 0;
  assert_equal::<i32>(6, bump().y);
  assert_equal::<i32>(1, counter);
  assert_equal::<i64>(3, bump().i.b);
  assert_equal::<i32>(2, counter);
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
