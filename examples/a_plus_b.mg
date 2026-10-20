import fmt "std/fmt";
import mem "std/mem";
import wasi "std/wasi";

@main()
fn main() {
  var a = read_i32();
  var b = read_i32();

  var c = a + b;
  fmt.print_i32(c);
  fmt.print_str("\n");
}

fn read_i32(): i32 {
  var result: i32 = 0;

  // read non-numeric chars
  while true {
    var ch = read_single_char();
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
    var ch = read_single_char();
    if ch >= '0' && ch <= '9' {
      result = result * 10 + (ch - '0') as i32;
    } else {
      break;
    }
  }

  return result;
}

fn read_single_char(): u8 {
  var data = mem.alloc_array[u8](1);
  var iovec = mem.alloc[wasi.IoVec]();
  iovec.* = *[u8]{ptr: data, len: 1};
  var n_read_ptr = mem.alloc[i32]();
  wasi.fd_read(0, iovec, 1, n_read_ptr);
  var n_read = n_read_ptr.*;
  var ch = data[0].*;

  mem.dealloc[wasi.IoVec](iovec);
  mem.dealloc_array[u8](data);
  mem.dealloc[i32](n_read_ptr);

  if n_read == 0 {
    return 0;
  }
  return ch;
}

fn print_single_char(ch: u8) {
  var data = mem.alloc_array[u8](1);
  data[0].* = ch;
  var iovec = mem.alloc[wasi.IoVec]();
  iovec.* = *[u8]{ptr: data, len: 1};
  var n_written_ptr = mem.alloc[i32]();

  wasi.fd_write(1, iovec, 1, n_written_ptr);
  mem.dealloc[wasi.IoVec](iovec);
  mem.dealloc_array[u8](data);
  mem.dealloc[i32](n_written_ptr);
}
