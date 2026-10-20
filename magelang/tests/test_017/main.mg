import lib "tests/test_017/lib";
import wasm "std/wasm";

var initial: i32 = lib.count;
var initial_pair: lib.Pair[i32] = lib.Pair[i32]{value: 4};

@main()
fn main() {
  var grouped_count: i32 = (lib).count;
  if grouped_count != 3 { wasm.unreachable(); }
  if (lib).add(1, 2) != 3 { wasm.unreachable(); }

  var pair: lib.Pair[i32] = lib.make_pair[i32](lib.add(initial, initial_pair.value));
  if pair.value != 7 { wasm.unreachable(); }
  var plain: lib.Plain = lib.Plain{value: lib.current.value};
  if plain.value != 2 { wasm.unreachable(); }

  lib.count = 5;
  if lib.count != 5 { wasm.unreachable(); }
  if ((lib)).count != 5 { wasm.unreachable(); }

  var nested: lib.Pair[lib.Pair[i32]] = lib.make_pair[lib.Pair[i32]](pair);
  if nested.value.value != 7 { wasm.unreachable(); }
  var grouped: (lib).Pair[i32] = ((lib).Pair)[i32]{value: 8};
  if grouped.value != 8 { wasm.unreachable(); }
  if ((lib.make_pair))[i32](8).value != 8 { wasm.unreachable(); }
  if ((lib)).make_pair[i32](9).value != 9 { wasm.unreachable(); }
  if lib.bytes[0].* != 97 { wasm.unreachable(); }
  if (lib).bytes[1].* != 98 { wasm.unreachable(); }

  var lib = lib.Pair[i32]{value: 9};
  if lib.value != 9 { wasm.unreachable(); }
  if (lib).value != 9 { wasm.unreachable(); }
}
