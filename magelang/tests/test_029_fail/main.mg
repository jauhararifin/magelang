struct Box<T> {
  value: *T,
}

fn first<T>() {
  second<Box<T>>();
}

fn second<T>() {
  first<T>();
}

@main()
fn main() {
  first<i32>();
}
