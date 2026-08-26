import wasm "std/wasm";

struct Inner {
  a: i64,
  b: i64,
}

struct Outer {
  x: i32,
  inner: Inner,
  y: i32,
}

struct A { pad: i32, b: B }
struct B { pad: i64, c: *C }
struct C { pad: i64, d: D }
struct D { pad: i32, e: E }
struct E { f: *F, pad: i32 }
struct F { pad: i32, g: G }
struct G { pad: i64, h: H }
struct H { pad: i32, i: *i64 }

let global_outer: *Outer = 1024 as *Outer;

@main()
fn main() {
  test_assign_through_deref();
  test_assign_through_nested_deref();
  test_assign_whole_struct_field();
  test_assign_through_array_index();
  test_assign_through_global_pointer();
  test_read_through_deref();
  test_assign_local_struct_field();
  test_long_chain();
  test_long_chain_through_two_pointers();
}

fn test_assign_through_deref() {
  let p = 2048 as *Outer;
  p.x.* = 1;
  p.y.* = 2;

  p.*.x = 10;
  p.*.y = 20;

  assert_equal::<i32>(10, p.x.*);
  assert_equal::<i32>(20, p.y.*);
  assert_equal::<i32>(10, p.*.x);
  assert_equal::<i32>(20, p.*.y);
}

fn test_assign_through_nested_deref() {
  let p = 3072 as *Outer;

  p.*.inner.a = 111;
  p.*.inner.b = 222;

  assert_equal::<i64>(111, p.inner.a.*);
  assert_equal::<i64>(222, p.inner.b.*);
  assert_equal::<i64>(111, p.*.inner.a);
  assert_equal::<i64>(222, p.*.inner.b);

  assert_equal::<i32>(0, p.x.*);
  assert_equal::<i32>(0, p.y.*);
}

fn test_assign_whole_struct_field() {
  let p = 4096 as *Outer;

  p.*.x = 1;
  p.*.inner = Inner{a: 333, b: 444};
  p.*.y = 2;

  assert_equal::<i32>(1, p.x.*);
  assert_equal::<i64>(333, p.inner.a.*);
  assert_equal::<i64>(444, p.inner.b.*);
  assert_equal::<i32>(2, p.y.*);
}

fn test_assign_through_array_index() {
  let arr = 5120 as [*]Outer;

  arr[0].*.x = 7;
  arr[1].*.x = 8;
  arr[2].*.inner.a = 9;

  assert_equal::<i32>(7, arr[0].x.*);
  assert_equal::<i32>(8, arr[1].x.*);
  assert_equal::<i64>(9, arr[2].inner.a.*);

  assert_equal::<i32>(7, arr[0].*.x);
  assert_equal::<i32>(8, arr[1].*.x);
  assert_equal::<i64>(9, arr[2].*.inner.a);
}

fn test_assign_through_global_pointer() {
  global_outer.*.x = 55;
  global_outer.*.inner.b = 66;

  assert_equal::<i32>(55, global_outer.x.*);
  assert_equal::<i64>(66, global_outer.inner.b.*);
}

fn test_read_through_deref() {
  let p = 6144 as *Outer;
  p.x.* = 3;
  p.inner.a.* = 4;
  p.y.* = 5;

  assert_equal::<i32>(3, p.*.x);
  assert_equal::<i64>(4, p.*.inner.a);
  assert_equal::<i32>(5, p.*.y);

  let v = p.*;
  assert_equal::<i32>(3, v.x);
  assert_equal::<i64>(4, v.inner.a);
  assert_equal::<i32>(5, v.y);
}

fn test_assign_local_struct_field() {
  let p = 7168 as *Outer;
  p.x.* = 3;
  p.inner.a.* = 4;

  let v = p.*;
  v.x = 30;
  v.inner.a = 40;

  assert_equal::<i32>(30, v.x);
  assert_equal::<i64>(40, v.inner.a);
  assert_equal::<i32>(3, p.x.*);
  assert_equal::<i64>(4, p.inner.a.*);
}

fn test_long_chain() {
  let a: A;
  a.b.c = 8192 as *C;

  a.b.c.*.d.e.f = 9216 as *F;
  assert_equal::<usize>(9216, a.b.c.*.d.e.f as usize);

  assert_equal::<usize>(8204, a.b.c.d.e.f as usize);

  assert_equal::<usize>(9216, a.b.c.d.e.f.* as usize);

  let raw_f = 8204 as **F;
  assert_equal::<usize>(9216, raw_f.* as usize);
}

fn test_long_chain_through_two_pointers() {
  let a: A;
  a.b.c = 8192 as *C;
  a.b.c.*.d.e.f = 9216 as *F;

  a.b.c.*.d.e.f.*.g.h.i = 10240 as *i64;
  assert_equal::<usize>(10240, a.b.c.*.d.e.f.*.g.h.i as usize);
  assert_equal::<usize>(9236, a.b.c.d.e.f.*.g.h.i as usize);

  a.b.c.*.d.e.f.*.g.h.i.* = 123456789;
  assert_equal::<i64>(123456789, a.b.c.*.d.e.f.*.g.h.i.*);

  assert_equal::<i64>(123456789, a.b.c.d.e.f.*.g.h.i.*.*);

  let raw_i = 10240 as *i64;
  assert_equal::<i64>(123456789, raw_i.*);

  a.b.c.d.e.f.*.g.h.i.*.* = 987654321;
  assert_equal::<i64>(987654321, a.b.c.*.d.e.f.*.g.h.i.*);

  assert_equal::<i32>(0, a.b.c.*.pad as i32);
  assert_equal::<i32>(0, a.b.c.*.d.pad);
  assert_equal::<i32>(0, a.b.c.*.d.e.pad);
  assert_equal::<i32>(0, a.b.c.*.d.e.f.*.pad);
  assert_equal::<i32>(0, a.b.c.*.d.e.f.*.g.h.pad);
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
