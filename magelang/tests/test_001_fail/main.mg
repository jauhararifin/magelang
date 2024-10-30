fn main() {
  foo()
}

fn foo(): i32 {
  let a = 10
  return a;
}

struct A<T> {
  v: [*]T,
}

struct Something<T> {
  val: T,
}

struct Main {
  a: A<Something<i32>>,
}

fn unknown_type_test() {
  let a = A::<Something<i32>>{v: 0 as [*]T};
  let m = Main{a: a};
}

fn char_lit_test() {
  let x = A::<i32>{} == 'x';
}
