import dep "tests/test_type_alias/dep";
import mem "std/mem";
import wasm "std/wasm";

type IoVec = *[u8];
type ChainedIoVec = IoVec;
type Forward = Later;
type Later = i32;
type Pair = (Forward, bool);
type Callback = fn(value: Forward): Later;
type PayloadAlias = Payload;
type NodePtr = *Node;
type StructAlias = RecursiveStruct;
type TupleStructAlias = (i32, TupleStruct);

struct Payload {
  value: Forward,
}

struct Node {
  next: NodePtr,
  value: i32,
}

struct RecursiveStruct {
  next: *StructAlias,
}

struct TupleStruct {
  next: *TupleStructAlias,
}

fn increment(value: Forward): Later {
  return value + 1;
}

@main()
fn main() {
  let iovec = mem.alloc[IoVec]();
  iovec.* = "hello";

  let alias: ChainedIoVec = iovec.*;
  if alias.len != 5 || alias[4].* != 111 {
    wasm.unreachable();
  }

  let pointer: *IoVec = iovec;
  let raw: **[u8] = pointer;
  if raw.*.len != 5 {
    wasm.unreachable();
  }

  let pair: Pair = (41, true);
  let callback: Callback = increment;
  if pair.1 != true || callback(pair.0) != 42 {
    wasm.unreachable();
  }

  let payload: PayloadAlias = PayloadAlias{value: 42};
  let node = mem.alloc[Node]();
  node.* = Node{next: 0 as NodePtr, value: payload.value};
  let bytes: dep.Bytes = "dependency";
  let number: dep.Number = 7;
  if node.value.* != 42 || node.next.* as usize != 0 || bytes.len != 10 || number != 7 {
    wasm.unreachable();
  }

  if wasm.size_of[IoVec]() != 8 || wasm.align_of[IoVec]() != 4 || wasm.size_of[*IoVec]() != 4 {
    wasm.unreachable();
  }

  mem.dealloc[Node](node);
  mem.dealloc[IoVec](iovec);
}
