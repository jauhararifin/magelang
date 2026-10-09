@wasm_export("index_i8")
fn index_i8(index: i8, len: usize): *u8 {
  return *[u8]{ptr: 1024 as [*]u8, len: len}[index];
}

@wasm_export("index_u8")
fn index_u8(index: u8, len: usize): *u8 {
  return *[u8]{ptr: 1024 as [*]u8, len: len}[index];
}

@wasm_export("index_i16")
fn index_i16(index: i16, len: usize): *u8 {
  return *[u8]{ptr: 1024 as [*]u8, len: len}[index];
}

@wasm_export("index_u16")
fn index_u16(index: u16, len: usize): *u8 {
  return *[u8]{ptr: 1024 as [*]u8, len: len}[index];
}

@wasm_export("index_i32")
fn index_i32(index: i32, len: usize): *u8 {
  return *[u8]{ptr: 1024 as [*]u8, len: len}[index];
}

@wasm_export("index_u32")
fn index_u32(index: u32, len: usize): *u8 {
  return *[u8]{ptr: 1024 as [*]u8, len: len}[index];
}

@wasm_export("index_isize")
fn index_isize(index: isize, len: usize): *u8 {
  return *[u8]{ptr: 1024 as [*]u8, len: len}[index];
}

@wasm_export("index_usize")
fn index_usize(index: usize, len: usize): *u8 {
  return *[u8]{ptr: 1024 as [*]u8, len: len}[index];
}

@wasm_export("index_i64")
fn index_i64(index: i64, len: usize): *u8 {
  return *[u8]{ptr: 1024 as [*]u8, len: len}[index];
}

@wasm_export("index_u64")
fn index_u64(index: u64, len: usize): *u8 {
  return *[u8]{ptr: 1024 as [*]u8, len: len}[index];
}

@wasm_export("empty")
fn empty(): *i32 {
  let s: *[i32];
  return s[0];
}

@wasm_export("read")
fn read(index: i32): u8 {
  let s = *[u8]{ptr: 1024 as [*]u8, len: 2};
  return s[index].*;
}

@wasm_export("write")
fn write(index: i32) {
  let s = *[u8]{ptr: 1024 as [*]u8, len: 2};
  s[index].* = 7;
}

@wasm_export("update")
fn update(index: i32) {
  let s = *[u8]{ptr: 1024 as [*]u8, len: 2};
  s[index].* += 1;
}
