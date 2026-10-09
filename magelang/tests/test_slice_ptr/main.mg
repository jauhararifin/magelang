import mem "std/mem";
import wasm "std/wasm";

struct Box[T] {
  tag: u8,
  items: *[T],
  tail: i64,
}

struct Node[T] {
  value: T,
  children: *[Node[T]],
}

let global_length: usize = global_items.len;
let global_items: *[u8] = *[u8]{ptr: "abc", len: message_length};
let message_length: usize = 3;
let empty_global: *[i32];
let active: *[i32];
let events: i32;

fn make[T](ptr: [*]T, len: usize): *[T] {
  return *[T]{len: len, ptr: ptr};
}

fn id[T](value: T): T { return value; }
fn first[T](value: *[T]): *T { return value[0]; }
fn identity(value: *[i32]): *[i32] { return value; }
fn equal[T](a: *[T], b: *[T]): bool { return a == b; }

fn source(): *[i32] {
  events = events * 10 + 1;
  return active;
}

fn index(): i64 {
  events = events * 10 + 2;
  active = *[i32]{};
  return 1;
}

@main()
fn main() {
  if global_length != 3 || global_items[2].* != 99 { wasm.unreachable(); }
  let empty: *[i32];
  if empty.len != 0 || empty_global.len != 0 { wasm.unreachable(); }
  if empty.ptr as usize != 0 { wasm.unreachable(); }
  if !(empty == empty_global) || empty != empty_global { wasm.unreachable(); }
  if wasm.size_of[*[i32]]() != 8 || wasm.align_of[*[i32]]() != 4 { wasm.unreachable(); }
  if wasm.size_of[Box[i32]]() != 24 || wasm.size_of[Node[i32]]() != 12 { wasm.unreachable(); }
  let node: Node[i32];
  if node.children.len != 0 { wasm.unreachable(); }

  let raw = mem.alloc_array[i32](3);
  raw[0].* = 7;
  raw[1].* = 11;
  raw[2].* = 19;
  let count: usize = 3;
  let s: *[i32] = make[i32](raw, count);
  if s.len != count || s.ptr != raw { wasm.unreachable(); }
  if first[i32](s).* != 7 || id[*[i32]](s)[2].* != 19 { wasm.unreachable(); }
  if s[0 as i8].* != 7 || s[0 as u8].* != 7 { wasm.unreachable(); }
  if s[1 as i16].* != 11 || s[1 as u16].* != 11 { wasm.unreachable(); }
  if s[1 as i32].* != 11 || s[1 as u32].* != 11 { wasm.unreachable(); }
  if s[2 as i64].* != 19 || s[2 as u64].* != 19 { wasm.unreachable(); }
  if s[2 as isize].* != 19 || s[count - 1].* != 19 { wasm.unreachable(); }
  s[1].* += 5;
  if raw[1].* != 16 { wasm.unreachable(); }
  let saved = s;
  s = make[i32](raw, count - 1);
  if saved.len != 3 || s.len != 2 { wasm.unreachable(); }
  let indirect: fn(*[i32]): *[i32] = identity;
  if indirect(s).len != 2 || indirect(s)[1].* != 16 { wasm.unreachable(); }

  let same = make[i32](raw, 2);
  let shifted = make[i32](raw[1] as [*]i32, 2);
  let shifted_shorter = make[i32](shifted.ptr, 1);
  if !(s == same) || s != same || !equal[i32](s, same) { wasm.unreachable(); }
  if s == saved || !(s != saved) { wasm.unreachable(); }
  if s == shifted || !(s != shifted) { wasm.unreachable(); }
  if s == shifted_shorter || !(s != shifted_shorter) { wasm.unreachable(); }
  if indirect(s) != id[*[i32]](same) { wasm.unreachable(); }

  let boxed = Box[i32]{tag: 5, items: s, tail: 123};
  if boxed.items.len != 2 || boxed.items[1].* != 16 { wasm.unreachable(); }
  boxed.items = saved;
  if boxed.items.len != 3 || boxed.tag != 5 || boxed.tail != 123 { wasm.unreachable(); }
  let box_ptr = mem.alloc[Box[i32]]();
  box_ptr.* = boxed;
  if box_ptr.items.*[2].* != 19 || box_ptr.tail.* != 123 { wasm.unreachable(); }
  box_ptr.*.items = s;
  if box_ptr.items.*.len != 2 { wasm.unreachable(); }

  let slot = mem.alloc[*[i32]]();
  slot.* = saved;
  if slot.*.len != 3 || slot.*[2].* != 19 { wasm.unreachable(); }
  let headers = slot as [*]usize;
  if headers[0].* != raw as usize || headers[1].* != 3 { wasm.unreachable(); }
  let nested = mem.alloc_array[*[i32]](2);
  nested[0].* = saved;
  nested[1].* = s;
  let outer: *[*[i32]] = make[*[i32]](nested, 2);
  if outer[0].*.len != 3 || outer[1].*[1].* != 16 { wasm.unreachable(); }
  if slot.* != outer[0].* || slot.* == outer[1].* { wasm.unreachable(); }
  if !equal[*[i32]](outer, make[*[i32]](nested, 2)) { wasm.unreachable(); }
  let records = mem.alloc_array[Box[i32]](2);
  let record_slice = make[Box[i32]](records, 2);
  record_slice[1].* = boxed;
  if record_slice[1].items.*[2].* != 19 { wasm.unreachable(); }

  let indices = mem.alloc_array[i64](1);
  indices[0].* = 1;
  let index_slice = make[i64](indices, 1);
  if saved[index_slice[0].*].* != 16 { wasm.unreachable(); }
  active = saved;
  if source()[index()].* != 16 || events != 12 { wasm.unreachable(); }
  if active.len != 0 { wasm.unreachable(); }
  active = saved;
  events = 0;
  source()[index()].* += 1;
  if raw[1].* != 17 || events != 12 { wasm.unreachable(); }
  active = saved;
  events = 0;
  if source().len != 3 || events != 1 { wasm.unreachable(); }

  active = s;
  events = 0;
  if !(source() == make[i32](raw, index() as usize + 1)) || events != 12 { wasm.unreachable(); }
  active = s;
  events = 0;
  if source() != make[i32](raw, index() as usize + 1) || events != 12 { wasm.unreachable(); }
  active = saved;
  events = 0;
  if !(source() != make[i32](raw, index() as usize + 1)) || events != 12 { wasm.unreachable(); }
  active = s;
  events = 0;
  if source() == make[i32](raw[1] as [*]i32, index() as usize + 1) || events != 12 { wasm.unreachable(); }

  let large = *[u8]{len: 0xffffffff};
  if large.len != 0xffffffff || large.len <= 0x7fffffff { wasm.unreachable(); }
  let missing_len = *[i32]{ptr: raw};
  if missing_len.len != 0 { wasm.unreachable(); }
  if missing_len == empty || !(missing_len != empty) { wasm.unreachable(); }
  let metadata = *[opaque]{ptr: 0xffffffff as [*]opaque, len: 0x80000000};
  if !equal[opaque](metadata, metadata) || metadata != metadata { wasm.unreachable(); }
  let different_metadata = *[opaque]{ptr: 0x80000000 as [*]opaque, len: 0xffffffff};
  if metadata == different_metadata || !(metadata != different_metadata) { wasm.unreachable(); }
  let zero_sized = *[void]{ptr: 0 as [*]void, len: 1};
  if zero_sized[0] as usize != 0 { wasm.unreachable(); }

  mem.dealloc_array[i64](indices);
  mem.dealloc_array[Box[i32]](records);
  mem.dealloc_array[*[i32]](nested);
  mem.dealloc[*[i32]](slot);
  mem.dealloc[Box[i32]](box_ptr);
  mem.dealloc_array[i32](raw);
}
