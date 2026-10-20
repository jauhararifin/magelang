import vector "std/vector";
import mem "std/mem";
import fmt "std/fmt";

@main()
fn main() {
  var vec_str = mem.alloc[vector.Vector[string]]();
  vector.init_with_cap[string](vec_str, 2);
  vector.push[string](vec_str, "This");
  vector.push[string](vec_str, " is");
  vector.push[string](vec_str, " a");
  vector.push[string](vec_str, " sentence");
  vector.push[string](vec_str, ".\n");

  var i: usize = 0;
  while i < vector.len[string](vec_str) {
    var s: string = vector.get[string](vec_str, i);
    fmt.print_str(s);
    i = i + 1;
  }

  fmt.print_i64(-123);
  fmt.print_str("\n");

  fmt.print_u64(123);
  fmt.print_str("\n");
}

