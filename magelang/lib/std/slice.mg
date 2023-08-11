import mem "std/mem";

struct Slice[T]{
  p:   [*]T,
  len: usize,
}

fn init[T](this: *Slice[T], len: usize) {
  this.p = mem.alloc_array[T](len);
  this.len = len;
}

fn get[T](this: *Slice[T], index: usize): T {
  if index >= this.len {
    panic("index out of range");
  }
  return this.p[index];
}

fn set[T](this: *Slice[T], index: usize, val: T) {
  if index >= this.len {
    panic("index out of range");
  }
  this.p[index] = val;
}

