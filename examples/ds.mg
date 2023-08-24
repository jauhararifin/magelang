import vector "std/vector";
import mem "std/mem";
import fmt "std/fmt";

@main()
fn main() {
  let vec_str = mem.alloc.<vector.Vector.<[*]u8> >();
  vector.init_with_cap.<[*]u8>(vec_str, 2);
  vector.push.<[*]u8>(vec_str, "This");
  vector.push.<[*]u8>(vec_str, " is");
  vector.push.<[*]u8>(vec_str, " a");
  vector.push.<[*]u8>(vec_str, " sentence");
  vector.push.<[*]u8>(vec_str, ".\n");

  let i: usize = 0;
  while i < vector.len.<[*]u8>(vec_str) {
    let s: [*]u8 = vector.get.<[*]u8>(vec_str, i);
    fmt.print_str(s);
    i = i + 1;
  }

  fmt.print_i64(-123);
  fmt.print_str("\n");

  fmt.print_u64(123);
  fmt.print_str("\n");
}

