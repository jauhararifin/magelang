@native_import("puts")
fn puts(message: [*]u8): i32;

@native_import("putchar")
fn putchar(character: i32): i32;

@native_import("malloc")
fn malloc(size: usize): [*]u8;

@native_import("free")
fn free(pointer: [*]u8);

@native_import("exit")
fn exit(status: i32);

@intrinsic("size_of")
fn size_of[T](): usize;

@intrinsic("align_of")
fn align_of[T](): usize;

@intrinsic("unreachable")
fn trap();
