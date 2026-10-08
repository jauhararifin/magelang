import wasm "std/wasm";

struct Pair { a: i32, b: i32 }
struct Triple { x: i32, y: i32, z: i32 }
struct Nested { p: Pair, q: Pair }
struct Wide { a: i32, b: i64, c: f64, d: i32 }
struct Gen[T] { first: T, second: T }

let seq: i32 = 0;
let gseq: i32 = 0;
let gpair: Pair = Pair{b: gtick(), a: gtick()};

fn gtick(): i32 {
  gseq += 1;
  return gseq;
}

fn tick(): i32 {
  seq += 1;
  return seq;
}

fn tick64(): i64 {
  seq += 1;
  return seq as i64;
}

fn tickf(): f64 {
  seq += 1;
  return seq as f64;
}

@main()
fn main() {
  test_source_order_is_evaluation_order();
  test_declaration_order_still_works();
  test_partial_literal();
  test_nested_literal();
  test_mixed_width_fields();
  test_generic_literal();
  test_nested_struct_field_order();
  test_global_initializer_order();
  test_literal_in_call_and_return();
}

fn test_global_initializer_order() {
  assert_equal[i32](1, gpair.b);
  assert_equal[i32](2, gpair.a);
}

fn take_pair(p: Pair, extra: i32): i32 {
  return p.a * 100 + p.b * 10 + extra;
}

fn make_triple(): Triple {
  return Triple{
    y: tick(),
    x: tick(),
    z: tick(),
  };
}

fn test_literal_in_call_and_return() {
  seq = 0;
  assert_equal[i32](213, take_pair(Pair{b: tick(), a: tick()}, tick()));

  seq = 0;
  let t = make_triple();
  assert_equal[i32](1, t.y);
  assert_equal[i32](2, t.x);
  assert_equal[i32](3, t.z);
}

fn test_source_order_is_evaluation_order() {
  seq = 0;
  let p = Pair{
    b: tick(),
    a: tick(),
  };
  assert_equal[i32](1, p.b);
  assert_equal[i32](2, p.a);

  seq = 0;
  let t = Triple{
    z: tick(),
    x: tick(),
    y: tick(),
  };
  assert_equal[i32](1, t.z);
  assert_equal[i32](2, t.x);
  assert_equal[i32](3, t.y);

  seq = 0;
  let r = Triple{
    y: tick(),
    z: tick(),
    x: tick(),
  };
  assert_equal[i32](1, r.y);
  assert_equal[i32](2, r.z);
  assert_equal[i32](3, r.x);
}

fn test_declaration_order_still_works() {
  seq = 0;
  let p = Pair{
    a: tick(),
    b: tick(),
  };
  assert_equal[i32](1, p.a);
  assert_equal[i32](2, p.b);

  seq = 0;
  let t = Triple{x: tick(), y: tick(), z: tick()};
  assert_equal[i32](1, t.x);
  assert_equal[i32](2, t.y);
  assert_equal[i32](3, t.z);
}

fn test_partial_literal() {
  seq = 0;
  let t = Triple{
    z: tick(),
    x: tick(),
  };
  assert_equal[i32](1, t.z);
  assert_equal[i32](2, t.x);
  assert_equal[i32](0, t.y);

  seq = 0;
  let u = Triple{y: tick()};
  assert_equal[i32](0, u.x);
  assert_equal[i32](1, u.y);
  assert_equal[i32](0, u.z);

  let v = Triple{};
  assert_equal[i32](0, v.x);
  assert_equal[i32](0, v.y);
  assert_equal[i32](0, v.z);
}

fn test_nested_literal() {
  seq = 0;
  let n = Nested{
    q: Pair{b: tick(), a: tick()},
    p: Pair{b: tick(), a: tick()},
  };
  assert_equal[i32](1, n.q.b);
  assert_equal[i32](2, n.q.a);
  assert_equal[i32](3, n.p.b);
  assert_equal[i32](4, n.p.a);
}

fn test_mixed_width_fields() {
  seq = 0;
  let w = Wide{
    d: tick(),
    c: tickf(),
    b: tick64(),
    a: tick(),
  };
  assert_equal[i32](1, w.d);
  assert_equal[f64](2.0, w.c);
  assert_equal[i64](3, w.b);
  assert_equal[i32](4, w.a);
}

fn test_generic_literal() {
  seq = 0;
  let g = Gen[i32]{
    second: tick(),
    first: tick(),
  };
  assert_equal[i32](1, g.second);
  assert_equal[i32](2, g.first);

  seq = 0;
  let h = Gen[Pair]{
    second: Pair{b: tick(), a: tick()},
    first: Pair{b: tick(), a: tick()},
  };
  assert_equal[i32](1, h.second.b);
  assert_equal[i32](2, h.second.a);
  assert_equal[i32](3, h.first.b);
  assert_equal[i32](4, h.first.a);
}

fn read_and_bump(p: *i32): i32 {
  p.* += 1;
  return p.*;
}

fn test_nested_struct_field_order() {
  let slot = 8192 as *i32;
  slot.* = 0;
  let n = Nested{
    q: Pair{a: read_and_bump(slot), b: read_and_bump(slot)},
    p: Pair{b: read_and_bump(slot), a: read_and_bump(slot)},
  };
  assert_equal[i32](1, n.q.a);
  assert_equal[i32](2, n.q.b);
  assert_equal[i32](3, n.p.b);
  assert_equal[i32](4, n.p.a);
}

fn assert_equal[T](expected: T, actual: T) {
  if expected != actual {
    wasm.unreachable();
  }
}
