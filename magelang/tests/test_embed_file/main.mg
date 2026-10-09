import wasm "std/wasm";

@embed_file("tests/test_embed_file/data.bin")
let data: *[u8];

@main()
fn main() {
  if data.ptr as usize != 8 || data.len != 4 { wasm.unreachable(); }
  if data[0].* != 0 || data[1].* != 1 || data[2].* != 127 || data[3].* != 255 { wasm.unreachable(); }
  if data.ptr[data.len].* != 0 { wasm.unreachable(); }
  if wasm.data_end() != data.ptr as usize + data.len + 1 { wasm.unreachable(); }
}
