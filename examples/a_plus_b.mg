import fmt "std/fmt";
import mem "std/mem";
import wasi "std/wasi";

@main()
fn main() {
  let a = read_i32();
  let b = read_i32();

  let c = a + b;
  fmt::print_i32(c);
  fmt::print_str("\n");
}

fn read_i32(): i32 {
  let result: i32 = 0;

  // read non-numeric chars
  while true {
    let ch = read_single_char();
    if ch == 0 {
      break;
    }

    if ch >= '0' && ch <= '9' {
      result = (ch - '0') as i32;
      break;
    }
  }

  // read the integer
  while true {
    let ch = read_single_char();
    if ch >= '0' && ch <= '9' {
      result = result * 10 + (ch - '0') as i32;
    } else {
      break;
    }
  }

  return result;
}

fn read_single_char(): u8 {
  let iovec = mem::alloc::<wasi::IoVec>();
  iovec.p.* = mem::alloc_array::<u8>(1);
  iovec.len.* = 1;
  let n_read_ptr = mem::alloc::<i32>();
  wasi::fd_read(0, iovec, 1, n_read_ptr);
  let n_read = n_read_ptr.*;

  mem::dealloc::<wasi::IoVec>(iovec);
  mem::dealloc_array::<u8>(iovec.p.*);
  mem::dealloc::<i32>(n_read_ptr);

  if n_read == 0 {
    return 0;
  }
  return iovec.p.*[0].*;
}

fn print_single_char(ch: u8) {
  let iovec = mem::alloc::<wasi::IoVec>();
  iovec.p.* = mem::alloc_array::<u8>(1);
  iovec.p.*[0].* = ch;
  iovec.len.* = 1;
  let n_read_ptr = mem::alloc::<i32>();

  wasi::fd_write(1, iovec, 1, n_read_ptr);
  mem::dealloc::<wasi::IoVec>(iovec);
  mem::dealloc_array::<u8>(iovec.p.*);
  mem::dealloc::<i32>(n_read_ptr);
}
