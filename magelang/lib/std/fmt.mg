import wasi "std/wasi";
import mem "std/mem";
import slice "std/slice";

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
  let string = mem.alloc[slice.Slice[u8]]();
  slice.init[u8](string, final_count as usize);

  let i = 0;
  if negative {
    slice.set[u8](string, 0, 45); // the ascii for '-'
    i = 1;
  }

  let j = count as isize - 1;
  while j >= 0 {
    slice.set[u8](string, i as usize, slice.get[u8](string, j as usize));
    i = i + 1;
    j = j - 1;
  }

  wasi.fd_write(1, string as i32, 1, 0);
}
