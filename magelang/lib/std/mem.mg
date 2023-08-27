import wasm "std/wasm";

let free_list: *Header = 0 as *Header;
let page_size: usize = 65536;

// TODO: add footer in each allocation to make it easier to merge two
// free chunk together.
struct Header {
  next: *Header,
  size: usize,
}

fn alloc<T>(): *T {
  let ptr = alloc_size(wasm::size_of::<T>());
  return ptr as *T;
}

fn alloc_array<T>(len: usize): [*]T {
  let ptr = alloc_size(wasm::size_of::<T>() * len);
  return ptr as [*]T;
}

fn alloc_size(size: usize): usize {
  let header = find_free_chunk(size);
  if header != 0 {
    return header + wasm::size_of::<Header>();
  }

  grow(size);
  return find_free_chunk(size);
}

fn find_free_chunk(size: usize): usize {
  let prev: *Header = 0 as *Header;
  let curr = free_list;
  while curr as usize != 0 {
    if curr.size.* >= size {
      if prev as usize != 0 {
        prev.next.* = curr.next.*;
      }

      if curr == free_list {
        free_list = curr.next.*;
      }

      let end = curr as usize + wasm::size_of::<Header>() + size;
      if end % 8 != 0 {
        end = end + 8 - end % 8;
      }
      let chunk_end = curr as usize + wasm::size_of::<Header>() + curr.size.*;
      let remaining_size: usize = 0;
      if chunk_end > end {
        remaining_size = chunk_end - end;
      }

      if remaining_size > wasm::size_of::<Header>() + 1 {
        let splitted_header = end as *Header;
        splitted_header.* = Header {
          next: curr.next.*,
          size: remaining_size - wasm::size_of::<Header>(),
        };
        curr.* = Header {
          next: splitted_header,
          size: size,
        };
      }

      return curr as usize;
    }

    prev = curr;
    curr = curr.next.*;
  }

  return curr as usize;
}

fn grow(size: usize) {
  let new_page_count = wasm::size_of::<Header>() + size;
  let page_id = wasm::memory_grow(new_page_count);
  let new_header = (page_id * page_size) as *Header;
  new_header.* = Header {
    next: free_list,
    size: page_size - wasm::size_of::<Header>(),
  };
  free_list = new_header;
}

fn dealloc<T>(p: *T) {
  let header = (p as usize - wasm::size_of::<Header>()) as *Header;
  header.next.* = free_list;
  free_list = header;
}

fn dealloc_array<T>(p: [*]T) {
  let header = (p as usize - wasm::size_of::<Header>()) as *Header;
  header.next.* = free_list;
  free_list = header;
}

