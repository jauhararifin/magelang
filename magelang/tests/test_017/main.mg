import lib "tests/test_017/lib";
import wasm "std/wasm";

let initial: i32 = lib.count;
let initial_pair: lib.Pair<i32> = lib.Pair<i32>{value: 4};

@main()
fn main() {
  let pair: lib.Pair<i32> = lib.make_pair<i32>(lib.add(initial, initial_pair.value));
  if pair.value != 7 { wasm.unreachable(); }
  let plain: lib.Plain = lib.Plain{value: lib.current.value};
  if plain.value != 2 { wasm.unreachable(); }

  lib.count = 5;
  if lib.count != 5 { wasm.unreachable(); }

  let nested: lib.Pair<lib.Pair<i32>> = lib.make_pair<lib.Pair<i32>>(pair);
  if nested.value.value != 7 { wasm.unreachable(); }

  let lib = lib.Pair<i32>{value: 9};
  if lib.value != 9 { wasm.unreachable(); }
}
