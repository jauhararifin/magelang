import mem "std/mem";

type Foo[T, U, V] struct {
  a: *Bar[V, T, U],
}

type Bar[T, U, V] struct {
  a: *Baz[V, T, U],
}

type Baz[T, U, V] struct {
  a: *Qux[V, T, U],
}

type Qux[T, U, V] struct {
  a: *Foo[V, T, U],
}

@main()
fn main() {
  let a = mem.alloc[Foo[i32, f64, i64]]();
  a.a.* = mem.alloc[Bar[i64,i32,f64]]();
  a.a.*.a.* = mem.alloc[Baz[f64,i64,i32]]();
  a.a.*.a.*.a.* = mem.alloc[Qux[i32,f64,i64]]();
  a.a.*.a.*.a.*.a.* = mem.alloc[Foo[i64,i32,f64]]();
  a.a.*.a.*.a.*.a.*.a.* = mem.alloc[Bar[f64,i64,i32]]();
  a.a.*.a.*.a.*.a.*.a.*.a.* = mem.alloc[Baz[i32,f64,i64]]();
  a.a.*.a.*.a.*.a.*.a.*.a.*.a.* = mem.alloc[Qux[i64,i32,f64]]();
  a.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.* = mem.alloc[Foo[f64,i64,i32]]();
  a.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.* = mem.alloc[Bar[i32,f64,i64]]();
  a.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.* = mem.alloc[Baz[i64,i32,f64]]();
  a.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.* = mem.alloc[Qux[f64,i64,i32]]();
  a.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.*.a.* = a;
}

type Foo2[T, U, V] struct {
  a: *Bar2[V, T, U],
}

type Bar2[T, U, V] struct {
  a: *Baz2[V, T, U],
}

type Baz2[T, U, V] struct {
  a: *Qux2[V, T, U],
}

type Qux2[T, U, V] struct {
  a: *Foo2[V, T, U],
}

fn another_test() {
  let a: Foo2[i32, f64, i64];
}
