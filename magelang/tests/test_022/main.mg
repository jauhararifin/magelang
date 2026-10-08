import mem "std/mem";
import wasm "std/wasm";

struct Pair[T] {
  value: T,
}

fn id[T](value: T): T {
  return value;
}

fn copy[T](value: T): T {
  return value;
}

fn increment(value: i32): i32 {
  return value + 1;
}

@main()
fn main() {
  let pair: Pair[i32]=Pair[i32]{value: 1};
  let same: bool = id[i32]==id[i32];
  let nested: bool = id[Pair[i32]]==id[Pair[i32]];
  if !same || !nested { wasm.unreachable(); }

  let boxed: (Pair)[i32] = (Pair)[i32]{value: copy[i32](42)};
  if boxed.value != 42 { wasm.unreachable(); }
  let outer = Pair[Pair[i32]]{value: boxed};
  if outer.value.value != 42 { wasm.unreachable(); }

  let data = mem.alloc_array[i32](2);
  data[0].* = 7;
  data[1].* = 11;
  if id[[*]i32](data)[1].* != 11 { wasm.unreachable(); }
  if id[*i32](data[0]).* != 7 { wasm.unreachable(); }
  if ((id))[i32](data[0].*) != 7 { wasm.unreachable(); }
  let f = id[fn(i32): i32](increment);
  if f(2) != 3 { wasm.unreachable(); }
  let casted = 1 as i64 + 2 * 3;
  if casted != 7 { wasm.unreachable(); }
  let second = (data as usize + wasm.size_of[i32]()) as *i32;
  if second.* != 11 { wasm.unreachable(); }
  if id[i32](2)<3 && id[i32](3)>2 {} else { wasm.unreachable(); }
  if id[i32](16)>>2 != 4 { wasm.unreachable(); }

  let index: i32 = 1;
  let typed: Pair[i32] = Pair[i32]{value: id[i32](data[index].*)};
  if typed.value != 11 { wasm.unreachable(); }
  let id = data;
  if id[index].* != 11 { wasm.unreachable(); }
  mem.dealloc_array[i32](data);
}
