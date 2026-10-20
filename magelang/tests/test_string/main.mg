import wasm "std/wasm";

var global: string = "global";

fn as_slice(value: string): *[u8] { return value; }
fn as_string(value: *[u8]): string { return value; }
fn first_byte(value: [*]u8): u8 { return value[0].*; }

@main()
fn main() {
  if global.len != 6 || global[5].* != 108 { wasm.unreachable(); }

  var inferred = "a\x00b";
  if inferred.len != 3 || inferred[0].* != 97 || inferred[1].* != 0 || inferred[2].* != 98 {
    wasm.unreachable();
  }
  if inferred.ptr[inferred.len].* != 0 { wasm.unreachable(); }

  var empty: string = "";
  if empty.len != 0 || empty.ptr[0].* != 0 { wasm.unreachable(); }

  var slice: *[u8] = as_slice("slice");
  var text: string = as_string(slice);
  if text.len != 5 || text[4].* != 101 { wasm.unreachable(); }

  var bytes: [*]u8 = "bytes".ptr;
  if bytes[5].* != 0 || first_byte("explicit".ptr) != 101 { wasm.unreachable(); }
}
