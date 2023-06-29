import wasi "std/wasi";
import mem "std/mem";
import slice "std/slice";

let stdin_head: usize;
let stdin_tail: usize;
let stdin_buffer: []u8;

fn read_i64(): i64 {
  let c = read_next_char();
  while c < 48 || c > 57 {
    c = read_next_char();
  }

  let n: i64 = c as i64 - 48;

  c = peek_next_char();
  while c >= 48 && c < 58 {
    n = n * 10 + (c as i64 - 48);
    read_next_char();
    c = peek_next_char();
  }

  return n;
}

fn peek_next_char(): u8 {
  if stdin_buffer as usize == 0 {
    stdin_buffer = slice.make_slice[u8](256);
    stdin_head = 0;
    stdin_tail = 0;
  }

  while stdin_head >= stdin_tail {
    let n_read: *i32 = mem.alloc[i32]();
    wasi.fd_read(0, stdin_buffer as i32, 1, n_read as i32);
    stdin_head = 0;
    stdin_tail = *n_read as usize;
  }

  return stdin_buffer[stdin_head];
}

fn read_next_char(): u8 {
  let c = peek_next_char();
  stdin_head = stdin_head + 1;
  return c;
}

fn print_i64(n: i64) {
  let negative = n < 0;

  let s: [*]u8 = mem.alloc_array[u8](32);
  if n < 0 {
    n = n * -1;
  }

  let count: u32 = 0;
  if n == 0 {
    s[count] = 48 as u8; // the ascii for '0'
    count = count + 1;
  }

  while n > 0 {
    s[count] = (n % 10 + 48) as u8;
    count = count + 1;
    n = n / 10;
  }

  let final_count = count;
  if negative {
    final_count = final_count + 1;
  }
  let string = slice.make_slice[u8](final_count as usize);

  let i = 0;
  if negative {
    string[0] = 45; // the ascii for '-'
    i = 1;
  }

  let j = count as isize - 1;
  while j >= 0 {
    string[i] = s[j];
    i = i + 1;
    j = j - 1;
  }

  wasi.fd_write(1, string as i32, 1, 0);
}
