import native "std/native";

fn assert[T](actual: T, expected: T) {
    if actual != expected { native.trap(); }
}
fn remainder(value: i64, divisor: i64): i64 { return value % divisor; }
fn shift(value: i32, count: i64): i32 { return value << count; }
fn unsigned_shift(value: u64, count: i8): u64 { return value >> count; }
fn float_to_signed(value: f64): i32 { return value as i32; }
fn float_to_unsigned(value: f64): u64 { return value as u64; }
fn id[T](value: T): T { return value; }
struct Pair { a: i64, b: i64 }
let null_pointer: *i32 = 0 as *i32;
let empty_slice: *[i32] = *[i32]{ptr: 0 as [*]i32, len: 0};

@main()
fn main() {
    assert[i64](remainder(-9223372036854775808, -1), 0);
    let min: i32 = -2147483648;
    assert[i32](min % -1, 0);
    let small: i8 = -128;
    assert[i8](small % -1, 0);
    assert[i8](small / -1, -128);
    assert[i32](shift(1, 32), 1);
    assert[i32](shift(1, 33), 2);
    assert[i32](shift(1, -1), -2147483648);
    assert[u64](unsigned_shift(18446744073709551615, 64), 18446744073709551615);
    assert[u64](unsigned_shift(18446744073709551615, -1), 1);
    let byte: u8 = 1;
    byte <<= 32;
    assert[u8](byte, 1);
    byte <<= 8;
    assert[u8](byte, 0);
    let neg: i16 = -256;
    assert[i16](neg >> 32, -256);
    let wrapping: i64 = 9223372036854775807;
    assert[i64](wrapping + 1, -9223372036854775808);
    assert[i64](-id[i64](-9223372036854775808), -9223372036854775808);

    assert[i32](float_to_signed(-2147483648.9), -2147483648);
    assert[i32](float_to_signed(2147483647.9), 2147483647);
    assert[u64](float_to_unsigned(-0.9), 0);
    assert[u64](float_to_unsigned(18446744073709549568.0), 18446744073709549568);
    let negative_zero: f64 = -0.0;
    let infinity: f64 = 1.0 / negative_zero;
    assert[bool](infinity < 0.0, true);
    let single: f32 = 0.1;
    assert[f32]((single as f64) as f32, single);
    let nan: f32 = id[f32](0.0) / id[f32](0.0);
    assert[bool](nan == nan, false);
    assert[bool](nan < 1.0 || nan >= 1.0, false);
    assert[bool](nan != nan, true);
    let huge: u64 = 18446744073709551615;
    assert[bool](huge as f64 > 9223372036854775808.0, true);

    assert[usize](null_pointer as usize, 0);
    assert[bool](empty_slice == *[i32]{}, true);
    assert[usize]((18446744073709551615 as *u8) as usize, 18446744073709551615);
    assert[f64]((4096 as *u8) as f64, 4096.0);
    assert[usize]((id[f64](4096.9) as [*]u8) as usize, 4096);
    let zero_sized = *[void]{ptr: 0 as [*]void, len: 1};
    assert[usize](zero_sized[0] as usize, 0);
    let signed_pointer = id[i8](-1) as *u8;
    assert[usize](signed_pointer as usize, 18446744073709551615);
    let raw = native.malloc(32);
    if raw as usize == 0 { native.exit(1); }
    defer native.free(raw);
    let unaligned = raw[1] as *i64;
    unaligned.* = -123456789;
    assert[i64](unaligned.*, -123456789);
    let mutable = "a\"\\\x00z";
    assert[u8](mutable[1].*, 34);
    assert[u8](mutable[2].*, 92);
    assert[u8](mutable[3].*, 0);
    assert[u8](mutable[4].*, 122);
    mutable[0].* = 98;
    assert[u8](mutable[0].*, 98);

    let total: i64 = 0;
    for let i: i64 = 0; i < 200000; i += 1 {
        let pair = id[Pair](Pair{a: i, b: 1});
        total += pair.b;
    }
    assert[i64](total, 200000);
}
