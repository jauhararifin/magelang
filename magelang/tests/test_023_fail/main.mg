struct Empty<> {}

fn empty_generic<>() {}

fn invalid_return(): i32 {
  return ,;
}

@main()
fn main() {
  let value: i32<> = 1;
}
