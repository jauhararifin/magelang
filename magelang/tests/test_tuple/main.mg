import mem "std/mem";
import wasm "std/wasm";
import dep "tests/test_tuple/dep";

struct Foo { value: i32 }
struct Bar { flag: bool }
struct Box[T] { value: (i32, T) }
struct Node[T] { links: (T, *Node[T]) }
struct Single[T] { value: (T) }
struct Empty {}

let global_first: i32 = global_tuple.0;
let global_tuple: (i32, (f32, bool)) = (later, (2.5, true));
let later: i32 = 17;
let zero_global: (i32, f64, (bool,));
let single_global: (i32) = (23,);
let global_equal: bool = global_tuple == global_tuple;
let global_different: bool = (1, 2) != (1, 3);
let events: i32;

fn pair[T, U](a: T, b: U): (T, U) { return (a, b); }
fn id[T](value: T): T { return value; }
fn equal[T](a: T, b: T): bool { return a == b; }
fn different[T](a: T, b: T): bool { return a != b; }
fn single_equal[T](a: T, b: T): bool { return (a,) == (b,); }
fn identity(value: (i32, f32)): (i32, f32) { return value; }
fn make(): (i32, f32) { return ((7, 1.5)); }
fn single[T](value: T): (T) { return (value,); }
fn single_identity(value: (i32)): (i32) { return value; }
fn make_empty(): () { events += 1; return (); }
fn effect() { events += 1; }
fn event(value: i32): i32 { events = events * 10 + value; return value; }

@main()
fn main() {
  let value: (i32, f32, Foo, Bar) = (42, 1.25, Foo{value: 7}, Bar{flag: true});
  if value.0 != 42 || value.1 != 1.25 || value.2.value != 7 || !value.3.flag { wasm.unreachable(); }
  value.0 += 2;
  value.2.value = 9;
  value.3 = Bar{};
  if value.0 != 44 || value.2.value != 9 || value.3.flag { wasm.unreachable(); }
  let copied = value;
  value.0 = 1;
  if copied.0 != 44 { wasm.unreachable(); }

  let inferred = (3, 2.5, true);
  let integer: isize = inferred.0;
  let float: f64 = inferred.1;
  if integer != 3 || float != 2.5 || !inferred.2 { wasm.unreachable(); }
  let grouped: (i32, f32) = (((2, 3.5)));
  grouped = (5, 6.5);
  if grouped.0 != 5 || grouped.1 != 6.5 { wasm.unreachable(); }
  let singleton: (u8,) = (255,);
  singleton.0 -= 1;
  if singleton.0 != 254 || (9,).0 != 9 || (2 + 3) != 5 { wasm.unreachable(); }
  let abbreviated: (u8) = singleton;
  let explicit: (u8,) = abbreviated;
  let single_zero: (i32);
  if explicit.0 != 254 || single_zero.0 != 0 || single_global.0 != 23 { wasm.unreachable(); }
  let nested_single: ((i32)) = ((7,),);
  let nested_pair: ((i32, f32)) = ((8, 9.5),);
  let named_single: (Foo) = (Foo{value: 10},);
  let empty_single: (()) = ((),);
  if nested_single.0.0 != 7 || nested_pair.0.1 != 9.5 || named_single.0.value != 10 { wasm.unreachable(); }
  let scalar: i32 = ((11));
  if scalar != 11 || single[(i32)]((12,)).0.0 != 12 { wasm.unreachable(); }
  let single_box = Single[(i64)]{value: ((13,),)};
  if single_box.value.0.0 != 13 { wasm.unreachable(); }
  let make_single: fn(i32): (i32) = single[i32];
  let pass_single: fn((i32)): (i32) = single_identity;
  if make_single(14).0 != 14 || pass_single((15,)).0 != 15 { wasm.unreachable(); }
  let single_ptr: *(i32) = mem.alloc[(i32)]();
  single_ptr.* = (16,);
  single_ptr.0.* += 1;
  if single_ptr.*.0 != 17 || wasm.size_of[(i32)]() != 4 { wasm.unreachable(); }
  mem.dealloc[(i32)](single_ptr);

  let many = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  if many.10 != 10 || many.1_0 != 10 { wasm.unreachable(); }

  let nested: (u8, (i64, f32), ()) = (5, (123, 4.5), ());
  if nested.1.0 != 123 || nested.1.1 != 4.5 { wasm.unreachable(); }
  nested.1 = (456, 7.5);
  nested.1.0 += 1;
  if nested.1.0 != 457 || nested.1.1 != 7.5 || nested.0 != 5 { wasm.unreachable(); }
  let inner = nested.1;
  if inner.0 != 457 { wasm.unreachable(); }
  let empty: () = nested.2;
  empty = ();
  let empty_first: ((), i64) = ((), 7);
  empty_first.0 = ();
  if empty_first.1 != 7 { wasm.unreachable(); }
  let all_empty: ((), ()) = ((), ());
  let all_empty = id[((), ())](all_empty);
  let ignored = all_empty.1;
  let with_opaque = (null, 5);
  if with_opaque.0 != null || with_opaque.1 != 5 { wasm.unreachable(); }

  if global_first != 17 || global_tuple.1.0 != 2.5 || !global_tuple.1.1 { wasm.unreachable(); }
  global_tuple.1.0 += 1;
  global_tuple.0 = 19;
  if global_tuple.1.0 != 3.5 || global_tuple.0 != 19 { wasm.unreachable(); }
  let zero: (i32, f64, (bool,));
  if zero.0 != 0 || zero.1 != 0 || zero.2.0 { wasm.unreachable(); }
  if zero_global.0 != 0 || zero_global.1 != 0 || zero_global.2.0 { wasm.unreachable(); }
  let zero_empty: ();
  let node: Node[i32];
  if node.links.0 != 0 || node.links.1 as usize != 0 { wasm.unreachable(); }

  if identity((8, 2.5)).0 != 8 || make().1 != 1.5 { wasm.unreachable(); }
  let indirect: fn((i32, f32)): (i32, f32) = identity;
  if indirect((3, 4.5)).1 != 4.5 { wasm.unreachable(); }
  let generic = pair[i32, (i64, bool)](11, (21, true));
  if generic.0 != 11 || generic.1.0 != 21 || !generic.1.1 { wasm.unreachable(); }
  let boxed = Box[(i64, f32)]{value: (5, (6, 7.5))};
  if id[Box[(i64, f32)]](boxed).value.1.0 != 6 { wasm.unreachable(); }
  if id[(i32, f32)](make()).1 != 1.5 { wasm.unreachable(); }
  if id[(i32, i32)]((8, 9)).1 != 9 { wasm.unreachable(); }
  let callable = (false, indirect);
  if callable.1((4, 5.5)).0 != 4 { wasm.unreachable(); }
  if dep.identity(make()).0 != 7 || dep.make().1 != 8.5 { wasm.unreachable(); }

  if wasm.size_of[(u8, i64, u16, f32)]() != 24 { wasm.unreachable(); }
  if wasm.align_of[(u8, i64, u16, f32)]() != 8 { wasm.unreachable(); }
  if wasm.size_of[()]() != 0 || wasm.align_of[()]() != 1 { wasm.unreachable(); }
  if wasm.size_of[(u8,)]() != 1 { wasm.unreachable(); }
  let ptr = mem.alloc[(u8, (i64, f32), ())]();
  ptr.* = nested;
  if ptr.0.* != 5 || ptr.1.0.* != 457 || ptr.1.1.* != 7.5 { wasm.unreachable(); }
  let address: *i64 = ptr.1.0;
  if address as usize - ptr as usize != 8 { wasm.unreachable(); }
  ptr.1.0.* += 1;
  ptr.*.1.1 = 8.5;
  if ptr.1.*.0 != 458 || ptr.1.1.* != 8.5 { wasm.unreachable(); }
  ptr.2.* = ();
  let empty_loaded = ptr.2.*;
  let loaded = ptr.*;
  if loaded.1.0 != 458 || loaded.1.1 != 8.5 { wasm.unreachable(); }
  let padding = (ptr as usize + 1) as *u8;
  padding.* = 255;
  if ptr.* != loaded { wasm.unreachable(); }

  let array = mem.alloc_array[(i32, f32)](2);
  array[0].* = (13, 14.5);
  array[1].* = make();
  let slice = *[(i32, f32)]{ptr: array, len: 2};
  slice[1].0.* += 1;
  if slice[0].1.* != 14.5 || slice[1].*.0 != 8 { wasm.unreachable(); }
  let tuple_slice = (5, slice);
  if tuple_slice.1[0].0.* != 13 { wasm.unreachable(); }

  events = 0;
  let ordered = (event(1), (event(2), event(3))).1;
  if events != 123 || ordered.0 != 2 || ordered.1 != 3 { wasm.unreachable(); }
  events = 0;
  let ignored = (event(1), make_empty()).1;
  if events != 2 { wasm.unreachable(); }
  events = 0;
  let empty = make_empty();
  if events != 1 { wasm.unreachable(); }

  let with_void = mem.alloc[(void, i32)]();
  with_void.* = (effect(), 7);
  let loaded_void = with_void.*;
  if loaded_void.1 != 7 || events != 2 { wasm.unreachable(); }
  if wasm.size_of[(void, i32)]() != 4 { wasm.unreachable(); }
  mem.dealloc[(void, i32)](with_void);

  mem.dealloc_array[(i32, f32)](array);
  mem.dealloc[(u8, (i64, f32), ())](ptr);
  test_equality();
}

fn test_equality() {
  if !global_equal || !global_different { wasm.unreachable(); }
  if () != () || !(() == ()) { wasm.unreachable(); }
  if (1,) != (1,) || (1,) == (2,) || !((1,) != (2,)) { wasm.unreachable(); }
  let tuple = (1, 2.5, true);
  if tuple != (1, 2.5, true) || !(tuple == (1, 2.5, true)) { wasm.unreachable(); }
  if tuple == (2, 2.5, true) || tuple == (1, 3.5, true) || tuple == (1, 2.5, false) { wasm.unreachable(); }
  let nested = ((), ((1,), (2.5, false)), ());
  let nested_copy = nested;
  if nested != nested_copy { wasm.unreachable(); }
  nested_copy.1.1.0 = 3.5;
  if nested == nested_copy { wasm.unreachable(); }
  if ((), ((),)) != ((), ((),)) { wasm.unreachable(); }

  let wide: (i8, u8, i16, u16, i32, u32, i64, u64, f32, f64) = (-1, 255, -123, 65535, -456, 4294967295, -4294967297, 18446744073709551615, 1.25, 2.5);
  let wide_copy = wide;
  if wide != wide_copy { wasm.unreachable(); }
  wide_copy.6 += 1;
  if wide == wide_copy { wasm.unreachable(); }
  wide_copy = wide;
  wide_copy.8 += 1;
  if wide == wide_copy { wasm.unreachable(); }

  let records = (Foo{value: 1}, Bar{flag: true}, Box[(i64, f32)]{value: (2, (3, 4.5))}, Empty{});
  let records_copy = records;
  if records != records_copy { wasm.unreachable(); }
  records_copy.2.value.1.0 += 1;
  if records == records_copy { wasm.unreachable(); }
  let foo = Foo{value: 1};
  let other_foo = Foo{value: 2};
  if foo != foo || foo == other_foo || !equal[Foo](foo, foo) || !different[Foo](foo, other_foo) { wasm.unreachable(); }
  let empty = Empty{};
  if empty != empty || !equal[Empty](empty, Empty{}) { wasm.unreachable(); }
  if !equal[()]((), ()) || different[()]((), ()) { wasm.unreachable(); }
  if !equal[(i32, f32)](make(), make()) || different[(i32, f32)](make(), make()) { wasm.unreachable(); }
  if !single_equal[(i32, f32)](make(), make()) || single_equal[i32](1, 2) { wasm.unreachable(); }
  if !different[(i32,)]((1,), (2,)) || different[(i32,)]((1,), (1,)) { wasm.unreachable(); }
  if global_tuple != id[(i32, (f32, bool))](global_tuple) { wasm.unreachable(); }

  let array = 1024 as [*]i32;
  let metadata = (1024 as *i32, array, *[i32]{ptr: array, len: 2}, identity);
  let changed = metadata;
  if metadata != changed { wasm.unreachable(); }
  changed.0 = 2048 as *i32;
  if metadata == changed { wasm.unreachable(); }
  changed = metadata;
  changed.1 = 2048 as [*]i32;
  if metadata == changed { wasm.unreachable(); }
  changed = metadata;
  changed.2 = *[i32]{ptr: array, len: 3};
  if metadata == changed { wasm.unreachable(); }
  changed = metadata;
  changed.2 = *[i32]{ptr: 2048 as [*]i32, len: 2};
  if metadata == changed { wasm.unreachable(); }
  changed = metadata;
  changed.3 = dep.identity;
  if metadata == changed { wasm.unreachable(); }
  let external = (*[opaque]{ptr: 1024 as [*]opaque, len: 2},);
  if external != external { wasm.unreachable(); }

  let zero64: f64 = 0;
  let nan64 = zero64 / zero64;
  if (zero64,) != (-zero64,) || (nan64,) == (nan64,) || !((nan64,) != (nan64,)) { wasm.unreachable(); }
  let zero32: f32 = 0;
  let nan32 = zero32 / zero32;
  if (zero32,) != (-zero32,) || (nan32,) == (nan32,) || !((nan32,) != (nan32,)) { wasm.unreachable(); }

  events = 0;
  let same = (event(1), event(2)) == (event(1), event(2));
  if !same || events != 1212 { wasm.unreachable(); }
  events = 0;
  let changed = (event(1), event(2)) != (event(3), event(4));
  if !changed || events != 1234 { wasm.unreachable(); }
  events = 0;
  let nested_temps = (event(1), (2, 3) == (2, 3)) == (event(1), (4, 5) == (4, 5));
  if !nested_temps || events != 11 { wasm.unreachable(); }
  events = 0;
  let empty_equal = make_empty() == make_empty();
  if !empty_equal || events != 2 { wasm.unreachable(); }
  events = 0;
  let empty_different = make_empty() != make_empty();
  if empty_different || events != 2 { wasm.unreachable(); }
  events = 0;
  let void_elements = (effect(),) == (effect(),);
  if !void_elements || events != 2 { wasm.unreachable(); }
  events = 0;
  let void_equal = effect() == effect();
  if !void_equal || events != 2 { wasm.unreachable(); }
}
