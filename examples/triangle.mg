import fmt "std/fmt";
import mem "std/mem";
import wasi "std/wasi";

fn main() {
  let n = fmt.read_i64();

  let i = 0;
  while i < n {
    let j = 0;
    while j < n-i-1 {
      wasi.fd_write(1, " " as i32, 1, 0);
      j = j + 1;
    }
    let j = 0;
    while j < (i*2+1) {
      wasi.fd_write(1, "*" as i32, 1, 0);
      j = j + 1;
    }
    wasi.fd_write(1, "\n" as i32, 1, 0);
    i = i + 1;
  }
}

