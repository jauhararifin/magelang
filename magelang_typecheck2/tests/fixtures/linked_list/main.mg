import wasm "std/wasm";
import mem "std/mem";

// A generic struct that refers to itself through a pointer, used from a generic
// function whose type parameter has the same name as the struct's.
struct Node<T> {
  val: T,
  next: *Node<T>,
}

fn push<T>(head: *Node<T>, val: T): *Node<T> {
  let n = mem::alloc::<Node<T>>();
  n.val.* = val;
  n.next.* = head;
  return n;
}

fn sum(head: *Node<i64>): i64 {
  let total: i64 = 0;
  let curr = head;
  while curr as usize != 0 {
    total = total + curr.val.*;
    curr = curr.next.*;
  }
  return total;
}

// A generic struct instantiated (through a pointer) before it is declared.
struct Foo {
  a: *Bar<i32>,
}

struct Bar<T> {
  x: T,
}

// A generic struct that embeds, by value, a generic struct declared after it.
struct Outer<T> {
  inner: Inner<T>,
}

struct Inner<T> {
  x: T,
}

// A struct containing a generic struct applied to itself, through a pointer: finite.
struct Tree {
  children: *Bar<Tree>,
}

@main()
fn main() {
  test_linked_list();
  test_forward_declared_generic();
  test_nested_generic_by_value();
  test_self_application_through_pointer();
}

fn test_linked_list() {
  let head = 0 as *Node<i64>;
  head = push::<i64>(head, 1);
  head = push::<i64>(head, 2);
  head = push::<i64>(head, 3);
  assert_equal::<i64>(6, sum(head));
  assert_equal::<i64>(3, head.val.*);
}

fn test_forward_declared_generic() {
  let bar = mem::alloc::<Bar<i32>>();
  bar.x.* = 7;
  let foo = Foo{a: bar};
  assert_equal::<i32>(7, foo.a.x.*);
}

fn test_nested_generic_by_value() {
  let outer = Outer::<i32>{inner: Inner::<i32>{x: 9}};
  assert_equal::<i32>(9, outer.inner.x);
  assert_equal::<usize>(8, wasm::size_of::<Outer<i64>>());
}

fn test_self_application_through_pointer() {
  let tree = Tree{children: 0 as *Bar<Tree>};
  assert_equal::<usize>(0, tree.children as usize);
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
