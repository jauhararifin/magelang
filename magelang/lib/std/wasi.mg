type IoVec = *[u8];

@wasm_import("wasi_snapshot_preview1", "fd_read")
fn fd_read(fd: usize, iovec_addr: *IoVec, count: usize, n_read: *i32): i32;

@wasm_import("wasi_snapshot_preview1", "fd_write")
fn fd_write(fd: usize, iovec_addr: *IoVec, count: usize, n_written_ptr: *i32): i32;

@wasm_import("wasi_snapshot_preview1", "fd_close")
fn fd_close(fd: i32): u16;

@wasm_import("wasi_snapshot_preview1", "sock_recv")
fn sock_recv(
  fd: i32,
  ri_data: [*]IoVec,
  count: usize,
  ri_flags: u16,
  ro_count: *usize,
  ro_flags: *u16
): u16;

@wasm_import("wasi_snapshot_preview1", "sock_send")
fn sock_send(fd: i32, si_data: [*]IoVec, count: usize, si_flags: u16, n_written: *usize): u16;

@wasm_import("wasi_snapshot_preview1", "sock_accept")
fn sock_accept(fd: i32, flags: u16, out_fd: *i32): u16;

@wasm_import("wasi_snapshot_preview1", "sock_shutdown")
fn sock_shutdown(fd: i32, flags: u8): u16;

type Subscription struct {
  userdata: u64,
  u:        SubscriptionU,
}

type SubscriptionU struct {
  tag: u8,

  _padding: i32,

  clock_id_or_fd: i32,
  timestamp:      u64,
  precision:      u64,
  flags:          u16,

  // TODO: support union data type, and then model this
  // struct into:
  // union {
  //  type SubscriptionClock struct {
  //    id:        u32,
  //    timestamp: u64,
  //    precision: u64,
  //    flags:     u16,
  //  }
  //  type SubscriptionFdReadWrite struct {
  //    fd: i32,
  //  }
  // }
}

let ERROR_EAGAIN: u16 = 6;

let EVENT_TYPE_CLOCK:    u8 = 0;
let EVENT_TYPE_FD_READ:  u8 = 1;
let EVENT_TYPE_FD_WRITE: u8 = 2;

type Event struct {
  userdata:     u64,
  errno:        u16,
  event_type:   u8,
  fd_readwrite: EventFdReadWrite,
}

type EventFdReadWrite struct {
  nbytes: u64,
  flags:  u16,
}

@wasm_import("wasi_snapshot_preview1", "poll_oneoff")
fn poll_oneoff(subscription: [*]Subscription, out: [*]Event, n_subscriptions: usize, n_events: *usize): u16;

@wasm_import("wasi_snapshot_preview1", "proc_exit")
fn proc_exit(code: u32);

