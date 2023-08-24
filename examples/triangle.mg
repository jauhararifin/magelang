import fmt "std/fmt";

@*main()
fn main() {
  let n = 10;
  let i = 0;
  while i < n {
    let j = 0;
    while j < n-i-1 {
      fmt.print_str(" ");
      j = j + 1;
    }

    j = 0;
    while j < 2*i+1 {
      fmt.print_str("*");
      j = j + 1;
    }

    fmt.print_str("\n");
    i = i + 1;
  }
}


