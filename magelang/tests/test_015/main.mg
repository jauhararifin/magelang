import wasm "std/wasm";

struct Inner { x: i32 }
struct Outer { n: i32, inner: Inner }
struct Pair { a: i32, b: i64 }
struct Ptrs { p: *i32, q: *i32 }
struct Box<T> { v: T }

let calls: i32 = 0;
let evals: i32 = 0;
let g: i32 = 0;
let gs: Outer = Outer{n: 1, inner: Inner{x: 2}};

let init_b: i32 = compute();
let init_c: i32 = compute2();
let init_d: i32 = compute3();
let init_a: i32 = 5;
let gptr: *i32 = alloc_slot();

fn compute(): i32 { let r: i32 = 0; r += init_a; r *= 2; return r; }
fn compute2(): i32 { let r = 7000 as *i32; r.* = 0; r.* += init_a; return r.*; }
fn compute3(): i32 { gptr.* += 1; return gptr.*; }
fn alloc_slot(): *i32 { let s = 7100 as *i32; s.* = 41; return s; }

@main()
fn main() {
  test_global_initializer_dependencies();
  test_deref_targets_of_every_primitive_type();
  test_value_temporaries_do_not_clobber_target_address();
  test_short_circuit_through_deref_target();
  test_deferred_compound_assignment();
  test_mixed_shift_count_widths();
  test_struct_field_targets();
  test_deref_assign_token_splitting();
}

fn test_global_initializer_dependencies() {
  assert_equal::<i32>(10, init_b);
  assert_equal::<i32>(5, init_c);
  assert_equal::<i32>(42, init_d);
}

fn p_i8(): *i8 { calls += 1; return 1024 as *i8; }
fn p_u8(): *u8 { calls += 1; return 1040 as *u8; }
fn p_i16(): *i16 { calls += 1; return 1056 as *i16; }
fn p_u16(): *u16 { calls += 1; return 1072 as *u16; }
fn p_i32(): *i32 { calls += 1; return 1088 as *i32; }
fn p_u32(): *u32 { calls += 1; return 1104 as *u32; }
fn p_i64(): *i64 { calls += 1; return 1120 as *i64; }
fn p_u64(): *u64 { calls += 1; return 1136 as *u64; }
fn p_isize(): *isize { calls += 1; return 1152 as *isize; }
fn p_usize(): *usize { calls += 1; return 1168 as *usize; }
fn p_f32(): *f32 { calls += 1; return 1184 as *f32; }
fn p_f64(): *f64 { calls += 1; return 1200 as *f64; }
fn p_bool(): *bool { calls += 1; return 1216 as *bool; }

fn test_deref_targets_of_every_primitive_type() {
  p_i8().* = 127; p_i8().* += 1; assert_equal::<i8>(-128, p_i8().*);
  p_i8().* = -128; p_i8().* -= 1; assert_equal::<i8>(127, p_i8().*);
  p_i8().* = -16; p_i8().* >>= 2; assert_equal::<i8>(-4, p_i8().*);
  p_i8().* = -7; p_i8().* /= 2; assert_equal::<i8>(-3, p_i8().*);
  p_i8().* = -7; p_i8().* %= 3; assert_equal::<i8>(-1, p_i8().*);

  p_u8().* = 250; p_u8().* += 10; assert_equal::<u8>(4, p_u8().*);
  p_u8().* = 0; p_u8().* -= 1; assert_equal::<u8>(255, p_u8().*);
  p_u8().* = 240; p_u8().* >>= 4; assert_equal::<u8>(15, p_u8().*);
  p_u8().* = 31; p_u8().* <<= 4; assert_equal::<u8>(240, p_u8().*);
  p_u8().* = 200; p_u8().* /= 3; assert_equal::<u8>(66, p_u8().*);

  p_i16().* = 32767; p_i16().* += 1; assert_equal::<i16>(-32768, p_i16().*);
  p_i16().* = -1024; p_i16().* >>= 3; assert_equal::<i16>(-128, p_i16().*);
  p_u16().* = 65535; p_u16().* += 1; assert_equal::<u16>(0, p_u16().*);
  p_u16().* = 65535; p_u16().* >>= 8; assert_equal::<u16>(255, p_u16().*);

  p_i32().* = -2147483648; p_i32().* -= 1; assert_equal::<i32>(2147483647, p_i32().*);
  p_i32().* = -100; p_i32().* >>= 2; assert_equal::<i32>(-25, p_i32().*);
  p_u32().* = 4294967295; p_u32().* += 1; assert_equal::<u32>(0, p_u32().*);
  p_u32().* = 4294967295; p_u32().* >>= 28; assert_equal::<u32>(15, p_u32().*);
  p_u32().* = 4294967295; p_u32().* /= 2; assert_equal::<u32>(2147483647, p_u32().*);

  p_i64().* = 1; p_i64().* <<= 40; p_i64().* += 1; assert_equal::<i64>(1099511627777, p_i64().*);
  p_i64().* = -1; p_i64().* >>= 10; assert_equal::<i64>(-1, p_i64().*);
  p_u64().* = 18446744073709551615; p_u64().* >>= 60; assert_equal::<u64>(15, p_u64().*);
  p_u64().* = 18446744073709551615; p_u64().* += 1; assert_equal::<u64>(0, p_u64().*);
  p_u64().* = 0; p_u64().* -= 1; assert_equal::<u64>(18446744073709551615, p_u64().*);

  p_isize().* = 5; p_isize().* *= -3; assert_equal::<isize>(-15, p_isize().*);
  p_usize().* = 5; p_usize().* |= 8; p_usize().* ^= 1; assert_equal::<usize>(12, p_usize().*);

  p_f32().* = 2.0; p_f32().* *= 1.5; p_f32().* -= 1.0; assert_equal::<f32>(2.0, p_f32().*);
  p_f64().* = 1.5; p_f64().* += 1; p_f64().* /= 5.0; assert_equal::<f64>(0.5, p_f64().*);

  p_bool().* = true; p_bool().* &&= false; assert_equal::<bool>(false, p_bool().*);
  p_bool().* ||= true; assert_equal::<bool>(true, p_bool().*);

  calls = 0;
  p_i32().* = 0;
  p_i32().* += 1;
  p_i32().* <<= 3;
  assert_equal::<i32>(3, calls);
  assert_equal::<i32>(8, p_i32().*);

  let pp = 1232 as **i32;
  pp.* = 1088 as *i32;
  pp.*.* = 40;
  pp.*.* += 2;
  assert_equal::<i32>(42, pp.*.*);
}

fn next_slot(): *i32 { calls += 1; return (2048 as [*]i32)[calls]; }
fn make_pair(): Pair { return Pair{a: 7, b: 9}; }
fn make_ptrs(): Ptrs { calls += 1; return Ptrs{p: (2048 as [*]i32)[calls], q: 0 as *i32}; }
fn ident<T>(v: T): T { return v; }
fn pair_at(p: *Pair): Pair { return p.*; }

fn test_value_temporaries_do_not_clobber_target_address() {
  let slots = 2048 as [*]i32;
  slots[1].* = 100; slots[2].* = 200; slots[3].* = 300; slots[4].* = 400; slots[5].* = 500;
  calls = 0;
  next_slot().* += make_pair().a;
  assert_equal::<i32>(107, slots[1].*);
  next_slot().* += make_pair().b as i32;
  assert_equal::<i32>(209, slots[2].*);
  let tmp = Pair{a: 5, b: 6};
  next_slot().* += tmp.a;
  assert_equal::<i32>(305, slots[3].*);
  next_slot().* += ident::<i32>(3);
  assert_equal::<i32>(403, slots[4].*);
  let pp = 3000 as *Pair;
  pp.* = Pair{a: 11, b: 12};
  next_slot().* += pair_at(pp).a + pp.*.a + (pair_at(pp).b as i32);
  assert_equal::<i32>(534, slots[5].*);
  assert_equal::<i32>(5, calls);

  calls = 0;
  make_ptrs().p.* += 1000;
  assert_equal::<i32>(1107, slots[1].*);
  assert_equal::<i32>(1, calls);

  calls = 0;
  next_slot().* += next_slot().*;
  assert_equal::<i32>(1107 + 209, slots[1].*);
  assert_equal::<i32>(209, slots[2].*);
  assert_equal::<i32>(2, calls);
}

fn flag(): *bool { calls += 1; return 3500 as *bool; }
fn side(v: bool): bool { evals += 1; return v; }

fn test_short_circuit_through_deref_target() {
  let f = 3500 as *bool;

  f.* = false; calls = 0; evals = 0;
  flag().* &&= side(true);
  assert_equal::<bool>(false, f.*);
  assert_equal::<i32>(0, evals);
  assert_equal::<i32>(1, calls);

  f.* = true; calls = 0; evals = 0;
  flag().* ||= side(false);
  assert_equal::<bool>(true, f.*);
  assert_equal::<i32>(0, evals);
  assert_equal::<i32>(1, calls);

  f.* = true; calls = 0; evals = 0;
  flag().* &&= side(false);
  assert_equal::<bool>(false, f.*);
  assert_equal::<i32>(1, evals);
  assert_equal::<i32>(1, calls);

  f.* = false; calls = 0; evals = 0;
  flag().* ||= side(true);
  assert_equal::<bool>(true, f.*);
  assert_equal::<i32>(1, evals);
  assert_equal::<i32>(1, calls);

  let t: bool = false; evals = 0;
  t &&= side(true);
  assert_equal::<bool>(false, t);
  assert_equal::<i32>(0, evals);
  t ||= side(true);
  assert_equal::<bool>(true, t);
  assert_equal::<i32>(1, evals);
}

fn defer_slot(): *i32 { calls += 1; return (4096 as [*]i32)[calls]; }

fn with_defers() {
  defer defer_slot().* += 1;
  defer defer_slot().* *= 2;
  (4096 as [*]i32)[1].* = 10;
  (4096 as [*]i32)[2].* = 20;
  g = 1;
  defer g += 10;
  defer g <<= 1;
}

fn test_deferred_compound_assignment() {
  let slots = 4096 as [*]i32;
  calls = 0;
  with_defers();
  assert_equal::<i32>(20, slots[1].*);
  assert_equal::<i32>(21, slots[2].*);
  assert_equal::<i32>(2, calls);
  assert_equal::<i32>(12, g);
}

fn test_mixed_shift_count_widths() {
  let n32: i32 = 40;
  let n64: i64 = 4;
  let n16: u16 = 7;
  let n8: i8 = 3;

  let a64: i64 = 1; a64 <<= n32;
  let b64: i64 = 1; b64 = b64 << n32;
  assert_equal::<i64>(1099511627776, a64);
  assert_equal::<i64>(b64, a64);

  let c32: i32 = 256; c32 >>= n64;
  let d32: i32 = 256; d32 = d32 >> n64;
  assert_equal::<i32>(16, c32);
  assert_equal::<i32>(d32, c32);

  let e8: u8 = 1; e8 <<= n16;
  let f8: u8 = 1; f8 = f8 << n16;
  assert_equal::<u8>(128, e8);
  assert_equal::<u8>(f8, e8);

  let g16: i16 = -256; g16 >>= n8;
  let h16: i16 = -256; h16 = h16 >> n8;
  assert_equal::<i16>(-32, g16);
  assert_equal::<i16>(h16, g16);

  let x: i32 = 1; x <<= 33;
  let y: i32 = 1; y = y << 33;
  assert_equal::<i32>(y, x);

  let p64 = 5000 as *i64; p64.* = 1; p64.* <<= n32;
  assert_equal::<i64>(1099511627776, p64.*);
  let p32 = 5008 as *i32; p32.* = 256; p32.* >>= n64;
  assert_equal::<i32>(16, p32.*);
  let p8 = 5012 as *u8; p8.* = 1; p8.* <<= n16;
  assert_equal::<u8>(128, p8.*);
  let p16 = 5014 as *i16; p16.* = -256; p16.* >>= n8;
  assert_equal::<i16>(-32, p16.*);
  let q64 = 5016 as *u64; q64.* = 18446744073709551615; q64.* >>= n8;
  assert_equal::<u64>(2305843009213693951, q64.*);
}

fn bump_param(o: Outer): i32 {
  o.inner.x += 40;
  o.n *= 2;
  return o.inner.x + o.n;
}

fn bump_generic(b: Box<Box<i64>>): i64 {
  b.v.v <<= 2;
  b.v.v |= 1;
  return b.v.v;
}

fn test_struct_field_targets() {
  gs.n += 5;
  gs.inner.x *= 10;
  assert_equal::<i32>(6, gs.n);
  assert_equal::<i32>(20, gs.inner.x);

  let lo = Outer{n: 3, inner: Inner{x: 4}};
  lo.inner.x -= 1;
  lo.n <<= 2;
  assert_equal::<i32>(3, lo.inner.x);
  assert_equal::<i32>(12, lo.n);
  assert_equal::<i32>(67, bump_param(lo));
  assert_equal::<i32>(3, lo.inner.x);
  assert_equal::<i32>(12, lo.n);

  let arr = 5500 as [*]Pair;
  arr[1].* = Pair{a: 1, b: 2};
  arr[1].*.a += 10;
  arr[1].b.* *= 5;
  assert_equal::<i32>(11, arr[1].*.a);
  assert_equal::<i64>(10, arr[1].*.b);
  let p = arr[1];
  p.*.b -= 3;
  assert_equal::<i64>(7, p.b.*);

  let bb = Box::<Box<i64>>{v: Box::<i64>{v: 3}};
  assert_equal::<i64>(13, bump_generic(bb));
  let pb = 6000 as *Box<Box<i64>>;
  pb.* = bb;
  pb.*.v.v += 100;
  pb.v.*.v -= 1;
  assert_equal::<i64>(102, pb.v.v.*);
}

fn test_deref_assign_token_splitting() {
  let p = 6500 as *i32;
  let b = 6600 as *bool;
  p.*=7; assert_equal::<i32>(7, p.*);
  p.**=3; assert_equal::<i32>(21, p.*);
  p.*=-1; assert_equal::<i32>(-1, p.*);
  assert_equal::<bool>(true, p.*==-1);
  b.*=false; b.*=b.*==false; assert_equal::<bool>(true, b.*);
  b.*=!b.*; assert_equal::<bool>(false, b.*);
  for p.*=0; p.*<3; p.*+=1 {}
  assert_equal::<i32>(3, p.*);
  for p.*=0; p.*<3; p.**=2 { p.*+=1; }
  assert_equal::<i32>(6, p.*);
  p.* =2; p.*= p.**2; assert_equal::<i32>(4, p.*);
  let q = 6700 as **i32;
  q.*=p; q.*.*=9; q.*.**=2; assert_equal::<i32>(18, p.*);
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
