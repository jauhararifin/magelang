struct Box<T> {
  value: *T,
}

fn expand<T>() {
  expand<Box<T>>();
}

@main()
fn main() {
  expand<i32>();
}
