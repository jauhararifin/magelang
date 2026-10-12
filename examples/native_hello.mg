@native_import("puts")
fn puts(message: [*]u8): i32;

fn add(a: i64, b: i64): i64 {
  return a + b;
}

@main()
fn main() {
  let a: i64 = 20;
  let b: i64 = 22;
  if add(a, b) == 42 {
    puts("Hello from native Magelang!");
  }
}
