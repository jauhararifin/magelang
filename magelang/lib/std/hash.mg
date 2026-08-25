fn mix(value: usize): usize {
  let x = value;
  x = x ^ (x >> 16);
  x = x * 0x7feb352d;
  x = x ^ (x >> 15);
  x = x * 0x846ca68b;
  x = x ^ (x >> 16);
  return x;
}

fn hash_usize(value: usize): usize {
  return mix(value);
}

fn hash_isize(value: isize): usize {
  return mix(value as usize);
}

fn hash_u8(value: u8): usize {
  return mix(value as usize);
}

fn hash_u16(value: u16): usize {
  return mix(value as usize);
}

fn hash_u32(value: u32): usize {
  return mix(value as usize);
}

fn hash_u64(value: u64): usize {
  return mix((value ^ (value >> 32)) as usize);
}

fn hash_i8(value: i8): usize {
  return mix(value as usize);
}

fn hash_i16(value: i16): usize {
  return mix(value as usize);
}

fn hash_i32(value: i32): usize {
  return mix(value as usize);
}

fn hash_i64(value: i64): usize {
  let v = value as u64;
  return hash_u64(v);
}

fn hash_bool(value: bool): usize {
  if value {
    return mix(1);
  }
  return mix(0);
}

fn hash_ptr<T>(value: *T): usize {
  return mix(value as usize);
}

fn hash_array_ptr<T>(value: [*]T): usize {
  return mix(value as usize);
}

fn hash_bytes(bytes: [*]u8, len: usize): usize {
  let h: usize = 2166136261;
  let i: usize = 0;
  while i < len {
    h = h ^ bytes[i].* as usize;
    h = h * 16777619;
    i = i + 1;
  }
  return mix(h);
}

fn hash_cstr(bytes: [*]u8): usize {
  let h: usize = 2166136261;
  let i: usize = 0;
  while bytes[i].* != 0 {
    h = h ^ bytes[i].* as usize;
    h = h * 16777619;
    i = i + 1;
  }
  return mix(h);
}
