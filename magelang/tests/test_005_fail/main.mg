struct P {
  a: i32,
  b: i32,
}

@main()
fn main() {
  let p = 1024 as *P;
  let q = 2048 as *i32;

  p.b = q;
}
