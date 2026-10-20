struct P {
  a: i32,
  b: i32,
}

@main()
fn main() {
  var p = 1024 as *P;
  var q = 2048 as *i32;

  p.b = q;
}
