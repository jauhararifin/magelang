import wasm "std/wasm";

let last_offset: usize = @data_end();

fn alloc[T](): *T {
  while last_offset % 8 != 0 {
    last_offset = last_offset + 1;
  }

  let ptr = last_offset;
  last_offset = last_offset + size_of[T]();

  let needed_page = (last_offset + 65535) / 65536;
  let need_additional = needed_page - wasm.memory_size();
  if need_additional > 0 {
    wasm.memory_grow(need_additional);
  }

  return ptr as *T;
}

fn alloc_array[T](len: usize): [*]T {
  while last_offset % 8 != 0 {
    last_offset = last_offset + 1;
  }

  let ptr = last_offset;
  last_offset = last_offset + size_of[T]() * len;

  let needed_page = (last_offset + 65535) / 65536;
  let need_additional = needed_page - wasm.memory_size();
  if need_additional > 0 {
    wasm.memory_grow(need_additional);
  }

  return ptr as [*]T;
}

#builtin("size_of")
fn size_of[T](): usize;
