struct P { a: i32 }

@main()
fn main() {}

fn bool_receiver() {
  let b: bool = true;
  b += 1;
}

fn pointer_receiver() {
  let p = 0 as *P;
  p += 1;
}

fn mismatched_value() {
  let x: i32 = 1;
  x += 1.5;
  let y: i32 = 1;
  let z: i64 = 2;
  y += z;
}

fn not_assignable() {
  let p = 0 as *P;
  p.a += 1;
}

fn string_receiver() {
  let s: [*]u8 = "abc";
  s += 1;
}
