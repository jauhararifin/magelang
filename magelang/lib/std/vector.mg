import mem "std/mem";

struct Vector<T>{
  arr: [*]T,
  cap: usize,
  len: usize,
}

fn init<T>(v: *Vector<T>) {
  v.* = Vector::<T>{
    cap: 0,
    len: 0,
    arr: 0 as [*]T,
  };
}

fn init_with_cap<T>(v: *Vector<T>, cap: usize) {
  if v.arr.* as usize != 0 {
    mem::dealloc_array::<T>( v.arr.* );
  }

  let arr = mem::alloc_array::<T>(cap);
  v.* = Vector::<T>{
    cap: cap,
    len: 0,
    arr: arr,
  };
}

fn push<T>(vec: *Vector<T>, item: T) {
  if vec.len.* == vec.cap.* {
    let new_cap: usize = 1;
    if vec.cap.* != 0 {
      new_cap = vec.cap.* * 2;
    } else {
      mem::dealloc_array::<T>( vec.arr.* );
    }

    let arr = mem::alloc_array::<T>(new_cap);
    let i: usize = 0;
    while i < vec.len.* {
      arr[i].* = vec.arr.*[i].*;
      i = i + 1;
    }

    vec.cap.* = new_cap;
    vec.arr.* = arr;
  }

  vec.arr.*[vec.len.*].* = item;
  vec.len.* = vec.len.* + 1;
}

fn set<T>(vec: *Vector<T>, i: usize, val: T) {
  vec.arr.*[i].* = val;
}

fn get<T>(vec: *Vector<T>, i: usize): T {
  return vec.arr.*[i].*;
}

fn len<T>(vec: *Vector<T>): usize {
  return vec.len.*;
}

