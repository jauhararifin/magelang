import mem "std/mem";
import wasm "std/wasm";

struct FreeNode {
  next: *FreeNode,
}

struct SlabAllocator {
  bucket8: *FreeNode,
  bucket16: *FreeNode,
  bucket32: *FreeNode,
  bucket64: *FreeNode,
  bucket128: *FreeNode,
  bucket256: *FreeNode,
  bucket512: *FreeNode,
}

fn init(allocator: *SlabAllocator) {
  allocator.* = SlabAllocator{
    bucket8: 0 as *FreeNode,
    bucket16: 0 as *FreeNode,
    bucket32: 0 as *FreeNode,
    bucket64: 0 as *FreeNode,
    bucket128: 0 as *FreeNode,
    bucket256: 0 as *FreeNode,
    bucket512: 0 as *FreeNode,
  };
}

fn alloc<T>(allocator: *SlabAllocator): *T {
  return alloc_size(allocator, wasm::size_of::<T>()) as *T;
}

fn dealloc<T>(allocator: *SlabAllocator, ptr: *T) {
  dealloc_size(allocator, ptr as usize, wasm::size_of::<T>());
}

fn alloc_array<T>(allocator: *SlabAllocator, len: usize): [*]T {
  return alloc_size(allocator, wasm::size_of::<T>() * len) as [*]T;
}

fn dealloc_array<T>(allocator: *SlabAllocator, ptr: [*]T, len: usize) {
  dealloc_size(allocator, ptr as usize, wasm::size_of::<T>() * len);
}

fn alloc_size(allocator: *SlabAllocator, size: usize): usize {
  let class = class_size(size);
  if class == 0 {
    return mem::alloc_size(size);
  }

  if class == 8 {
    return alloc_from_bucket(allocator.bucket8, 8);
  }
  if class == 16 {
    return alloc_from_bucket(allocator.bucket16, 16);
  }
  if class == 32 {
    return alloc_from_bucket(allocator.bucket32, 32);
  }
  if class == 64 {
    return alloc_from_bucket(allocator.bucket64, 64);
  }
  if class == 128 {
    return alloc_from_bucket(allocator.bucket128, 128);
  }
  if class == 256 {
    return alloc_from_bucket(allocator.bucket256, 256);
  }
  return alloc_from_bucket(allocator.bucket512, 512);
}

fn dealloc_size(allocator: *SlabAllocator, ptr: usize, size: usize) {
  if ptr == 0 {
    return;
  }

  let class = class_size(size);
  if class == 0 {
    mem::dealloc::<u8>(ptr as *u8);
    return;
  }

  if class == 8 {
    dealloc_to_bucket(allocator.bucket8, ptr);
    return;
  }
  if class == 16 {
    dealloc_to_bucket(allocator.bucket16, ptr);
    return;
  }
  if class == 32 {
    dealloc_to_bucket(allocator.bucket32, ptr);
    return;
  }
  if class == 64 {
    dealloc_to_bucket(allocator.bucket64, ptr);
    return;
  }
  if class == 128 {
    dealloc_to_bucket(allocator.bucket128, ptr);
    return;
  }
  if class == 256 {
    dealloc_to_bucket(allocator.bucket256, ptr);
    return;
  }
  dealloc_to_bucket(allocator.bucket512, ptr);
}

fn class_size(size: usize): usize {
  if size <= 8 { return 8; }
  if size <= 16 { return 16; }
  if size <= 32 { return 32; }
  if size <= 64 { return 64; }
  if size <= 128 { return 128; }
  if size <= 256 { return 256; }
  if size <= 512 { return 512; }
  return 0;
}

fn alloc_from_bucket(bucket: **FreeNode, class: usize): usize {
  if bucket.* as usize != 0 {
    let node = bucket.*;
    bucket.* = node.next.*;
    return node as usize;
  }
  return mem::alloc_size(class);
}

fn dealloc_to_bucket(bucket: **FreeNode, ptr: usize) {
  let node = ptr as *FreeNode;
  node.next.* = bucket.*;
  bucket.* = node;
}
