import wasi "std/wasi";
import mem "std/mem";
import vector "std/vector";

fn print_str(p: [*]u8) {
  let len = strlen(p);
  let iovec = mem::alloc::<wasi::IoVec>();
  iovec.* = wasi::IoVec{
    len: len,
    p: p,
  };

  wasi::fd_write(1, iovec, 1, 0 as *i32);
  mem::dealloc::<wasi::IoVec>(iovec);
}

fn strlen(p: [*]u8): usize {
  let i: usize = 0;
  while p[i].* != 0 {
    i = i + 1;
  }
  return i;
}

fn print_i8(val: i8) { print_i64(val as i64); }
fn print_i16(val: i16) { print_i64(val as i64); }
fn print_i32(val: i32) { print_i64(val as i64); }
fn print_isize(val: isize) { print_i64(val as i64); }

fn print_u8(val: u8) { print_u64(val as u64); }
fn print_u16(val: u16) { print_u64(val as u64); }
fn print_u32(val: u32) { print_u64(val as u64); }
fn print_usize(val: usize) { print_u64(val as u64); }

fn print_i64(val: i64) {
  if val == 0 {
    print_str("0");
    return;
  }

  let str = mem::alloc::<vector::Vector<u8>>();
  vector::init_with_cap::<u8>(str, 10);

  let start: usize = 0;
  if val < 0 {
    vector::push::<u8>(str, 45); // ascii for '-'
    start = 1;
  }

  while val != 0 {
    let d = val % 10;
    if d < 0 {
      d = -d;
    }
    vector::push::<u8>(str, 48 + d as u8); // ascii for '0'
    val = val / 10;
  }

  let i: usize = start;
  let j = vector::len::<u8>(str) as usize - 1;
  while i < j {
    let tmp = vector::get::<u8>(str, i);
    vector::set::<u8>(str, i, vector::get::<u8>(str, j));
    vector::set::<u8>(str, j, tmp);
    i = i + 1;
    j = j - 1;
  }

  vector::push::<u8>(str, 0);
  print_str(str.arr.*);
  mem::dealloc::<vector::Vector<u8>>(str);
}

fn print_u64(val: u64) {
  if val == 0 {
    print_str("0");
    return;
  }

  let str = mem::alloc::<vector::Vector<u8>>();
  vector::init_with_cap::<u8>(str, 10);

  while val != 0 {
    let d = val % 10;
    if d < 0 {
      d = -d;
    }
    vector::push::<u8>(str, 48 + d as u8); // ascii for '0'
    val = val / 10;
  }

  let i: usize = 0;
  let j = vector::len::<u8>(str) as usize - 1;
  while i < j {
    let tmp = vector::get::<u8>(str, i);
    vector::set::<u8>(str, i, vector::get::<u8>(str, j));
    vector::set::<u8>(str, j, tmp);
    i = i + 1;
    j = j - 1;
  }

  vector::push::<u8>(str, 0);
  print_str(str.arr.*);
  mem::dealloc::<vector::Vector<u8>>(str);
}

