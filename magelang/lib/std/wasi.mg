#wasm_link("wasi_snapshot_preview1", "fd_write")
fn fd_write(fd: i32, iovec_addr: i32, count: i32, n_written_ptr: i32): i32;

#wasm_link("wasi_snapshot_preview1", "fd_read")
fn fd_read(fd: i32, iovec_addr: i32, count: i32, n_read_ptr: i32): i32;
