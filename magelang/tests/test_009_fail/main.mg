struct P { a: i32 }

@main()
fn main() {}

fn bool_receiver() {
  var b: bool = true;
  b += 1;
}

fn pointer_receiver() {
  var p = 0 as *P;
  p += 1;
}

fn mismatched_value() {
  var x: i32 = 1;
  x += 1.5;
  var y: i32 = 1;
  var z: i64 = 2;
  y += z;
}

fn not_assignable() {
  var p = 0 as *P;
  p.a += 1;
}

fn string_receiver() {
  var s: string = "abc";
  s += 1;
}
