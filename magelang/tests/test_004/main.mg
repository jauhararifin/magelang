import mem "std/mem";

struct Foo<T, U, V> {
  a: *Bar<V, T, U>,
}

struct Bar<T, U, V> {
  a: *Baz<V, T, U>,
}

struct Baz<T, U, V> {
  a: *Qux<V, T, U>,
}

struct Qux<T, U, V> {
  a: *Foo<V, T, U>,
}

@main()
fn main() {
  let a = mem::alloc::<Foo<i32, f64, i64>>();
  a.a.* = mem::alloc::<Bar<i64,i32,f64>>();
  a.a.*.a.* = mem::alloc::<Baz<f64,i64,i32>>();
  a.a.*.a.*.a.* = mem::alloc::<Qux<i32,f64,i64>>();
  a.a.*.a.*.a.*.a.* = mem::alloc::<Foo<i64,i32,f64>>();
  a.a.*.a.*.a.*.a.*.a.* = mem::alloc::<Bar<f64,i64,i32>>();
  a.a.*.a.*.a.*.a.*.a.*.a.* = mem::alloc::<Baz<i32,f64,i64>>();
  a.a.*.a.*.a.*.a.*.a.*.a.*.a.* = mem::alloc::<Qux<i64,i32,f64>>();
  a.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.* = mem::alloc::<Foo<f64,i64,i32>>();
  a.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.* = mem::alloc::<Bar<i32,f64,i64>>();
  a.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.* = mem::alloc::<Baz<i64,i32,f64>>();
  a.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.* = mem::alloc::<Qux<f64,i64,i32>>();
  a.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.* = a;
}

struct Foo2<T, U, V> {
  a: *Bar2<V, T, U>,
}

struct Bar2<T, U, V> {
  a: *Baz2<V, T, U>,
}

struct Baz2<T, U, V> {
  a: *Qux2<V, T, U>,
}

struct Qux2<T, U, V> {
  a: *Foo2<V, T, U>,
}

fn another_test() {
  let a: Foo2<i32, f64, i64>;
}
