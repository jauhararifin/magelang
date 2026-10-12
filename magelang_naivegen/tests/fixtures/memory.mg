@native_import("malloc") fn malloc(size: usize): [*]u8;
@native_import("free") fn free(ptr: [*]u8);
@native_import("puts") fn puts(ptr: [*]u8): i32;
@intrinsic("size_of") fn size_of[T](): usize;
@intrinsic("align_of") fn align_of[T](): usize;
@intrinsic("unreachable") fn trap();
fn assert[T](expected: T, actual: T) { if expected != actual { trap(); } }
fn alloc[T](): *T { return malloc(size_of[T]()) as *T; }
fn id[T](value: T): T { return value; }

struct Pair { a: i32, b: i64 }
struct Record { tag: u8, pair: Pair, items: *[i32], tail: u16 }
struct Floats { a: f32, b: f64 }
let calls: i64;
let active: *[i32];
fn source(): *[i32] { calls = calls * 10 + 1; return active; }
fn index(): i64 { calls = calls * 10 + 2; active = *[i32]{}; return 1; }

@main()
fn main() {
  assert[usize](8, size_of[usize]());
  assert[usize](8, size_of[*i32]());
  assert[usize](16, size_of[*[i32]]());
  assert[usize](8, align_of[*[i32]]());
  assert[usize](16, size_of[Pair]());
  assert[usize](48, size_of[Record]());
  assert[usize](8, align_of[Record]());
  let raw = malloc(3 * size_of[i32]()) as [*]i32;
  defer free(raw as [*]u8);
  raw[0].* = 7;
  raw[1].* = 11;
  raw[2].* = 19;
  let slice = *[i32]{ptr: raw, len: 3};
  assert[i32](7, slice[0 as i8].*);
  assert[i32](11, slice[1 as u64].*);
  assert[i32](19, slice[2 as u16].*);
  assert[*[i32]](slice, id[*[i32]](slice));
  assert[bool](true, slice != *[i32]{ptr: raw, len: 2});
  assert[bool](true, slice != *[i32]{ptr: raw[1] as [*]i32, len: 3});
  active = slice;
  source()[index()].* += id[Pair](Pair{a: 5, b: 6}).a;
  assert[i64](12, calls);
  assert[i32](16, raw[1].*);
  assert[usize](0, active.len);
  let record = alloc[Record]();
  defer free(record as [*]u8);
  record.* = Record{tag: 9, pair: Pair{a: 2, b: 3}, items: slice, tail: 65535};
  record.*.pair.b += 40;
  record.pair.*.a *= 5;
  assert[i64](43, record.*.pair.b);
  assert[i32](10, record.pair.a.*);
  assert[u16](65535, record.tail.*);
  assert[i32](19, record.items.*[2].*);
  let copy = id[Record](record.*);
  assert[Record](copy, record.*);
  assert[i64](43, id[Record](copy).pair.b);
  assert[usize](3, id[Record](copy).items.len);
  let floats = alloc[Floats]();
  defer free(floats as [*]u8);
  floats.* = Floats{a: 1.5, b: -2.5};
  floats.a.* *= 2.0;
  floats.b.* /= 2.0;
  assert[Floats](Floats{a: 3.0, b: -1.25}, floats.*);
  let signed = alloc[i8]();
  defer free(signed as [*]u8);
  signed.* = -128;
  signed.* -= 1;
  assert[i8](127, signed.*);
  let pp = alloc[*Record]();
  defer free(pp as [*]u8);
  pp.* = record;
  assert[u8](9, pp.*.tag.*);
  let metadata = *[void]{ptr: 123 as [*]void, len: 1};
  assert[usize](123, metadata[0] as usize);
  assert[usize](0, (0 as *i32) as usize);
  assert[u8](98, "abc"[1].*);
  puts("native memory ok");
}
