import wasi "std/wasi";
import mem "std/mem";
import vector "std/vector";

pub fn print_str(value: string) {
  var iovec = mem.alloc[wasi.IoVec]();
  iovec.* = value;

  wasi.fd_write(1, iovec, 1, 0 as *i32);
  mem.dealloc[wasi.IoVec](iovec);
}

pub fn strlen(p: [*]u8): usize {
  var i: usize = 0;
  while p[i].* != 0 {
    i = i + 1;
  }
  return i;
}

pub fn print_i8(val: i8) { print_i64(val as i64); }
pub fn print_i16(val: i16) { print_i64(val as i64); }
pub fn print_i32(val: i32) { print_i64(val as i64); }
pub fn print_isize(val: isize) { print_i64(val as i64); }

pub fn print_u8(val: u8) { print_u64(val as u64); }
pub fn print_u16(val: u16) { print_u64(val as u64); }
pub fn print_u32(val: u32) { print_u64(val as u64); }
pub fn print_usize(val: usize) { print_u64(val as u64); }

pub fn print_i64(val: i64) {
  if val == 0 {
    print_str("0");
    return;
  }

  var str = mem.alloc[vector.Vector[u8]]();
  vector.init_with_cap[u8](str, 10);

  var start: usize = 0;
  if val < 0 {
    vector.push[u8](str, 45); // ascii for '-'
    start = 1;
  }

  while val != 0 {
    var d = val % 10;
    if d < 0 {
      d = -d;
    }
    vector.push[u8](str, 48 + d as u8); // ascii for '0'
    val = val / 10;
  }

  var i: usize = start;
  var j = vector.len[u8](str) as usize - 1;
  while i < j {
    var tmp = vector.get[u8](str, i);
    vector.set[u8](str, i, vector.get[u8](str, j));
    vector.set[u8](str, j, tmp);
    i = i + 1;
    j = j - 1;
  }

  print_str(string{ptr: str.arr.*, len: vector.len[u8](str)});
  mem.dealloc[vector.Vector[u8]](str);
}

pub fn print_u64(val: u64) {
  if val == 0 {
    print_str("0");
    return;
  }

  var str = mem.alloc[vector.Vector[u8]]();
  vector.init_with_cap[u8](str, 10);

  while val != 0 {
    var d = val % 10;
    if d < 0 {
      d = -d;
    }
    vector.push[u8](str, 48 + d as u8); // ascii for '0'
    val = val / 10;
  }

  var i: usize = 0;
  var j = vector.len[u8](str) as usize - 1;
  while i < j {
    var tmp = vector.get[u8](str, i);
    vector.set[u8](str, i, vector.get[u8](str, j));
    vector.set[u8](str, j, tmp);
    i = i + 1;
    j = j - 1;
  }

  print_str(string{ptr: str.arr.*, len: vector.len[u8](str)});
  mem.dealloc[vector.Vector[u8]](str);
}

