import mem "std/mem";

fn make_slice[T](len: usize): []T {
  let array_ptr = mem.alloc_array[T](len);

  let the_slice = mem.alloc_array[usize](2);
  the_slice[0] = array_ptr as usize;
  the_slice[1] = len;

  return the_slice as []T;
}

fn sub_slice[T](s: []T, start: usize, end: usize): []T {
  let slice_container = mem.alloc_array[usize](2);
  slice_container[0] = s as usize;
  slice_container[1] = end - start;
  return slice_container as []T;
}

