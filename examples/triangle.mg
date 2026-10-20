import fmt "std/fmt";

@main()
fn main() {
  var n = 10;
  var i = 0;
  while i < n {
    var j = 0;
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


