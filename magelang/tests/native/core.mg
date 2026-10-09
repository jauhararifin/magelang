import native "std/native";

struct Pair[T] { first: T, second: i64 }
struct Record { tag: u8, pair: Pair[i32], data: *[i32] }
struct Empty {}

let initialized: i32 = read_later();
let later: i32 = 42;
let message: [*]u8 = "native core ok";
let global_pair: Pair[i32] = Pair[i32]{first: 3, second: 4};
let events: i64;
let active: *[i32];

fn read_later(): i32 { return later; }
fn id[T](value: T): T { return value; }
fn assert[T](actual: T, expected: T) {
    if actual != expected { native.trap(); }
}
fn factorial(value: i64): i64 {
    if value < 2 { return 1; }
    return value * factorial(value - 1);
}
fn side(value: bool): bool { events += 1; return value; }
fn mark(value: i64) { events = events * 10 + value; }
fn choose(value: bool): Pair[i32] {
    defer mark(3);
    if value {
        defer mark(1);
        return Pair[i32]{first: 7, second: 8};
    } else {
        defer mark(2);
        return Pair[i32]{first: 9, second: 10};
    }
}
fn captured_return(): i32 {
    let value: i32 = 7;
    defer value = 9;
    return value;
}
fn source(): *[i32] { mark(1); return active; }
fn index(): i64 { mark(2); active = *[i32]{}; return 1; }

@intrinsic("f64.floor")
fn floor(value: f64): f64;
@intrinsic("f32.ceil")
fn ceil(value: f32): f32;

@main()
fn main() {
    assert[i32](initialized, 42);
    assert[i64](factorial(10), 3628800);
    let indirect: fn(i64): i64 = factorial;
    assert[i64](indirect(6), 720);
    let generic = id[i64];
    assert[i64](generic(19), 19);
    assert[bool](id[i64] == id[i64], true);
    let sum: i32 = 0;
    for let i: i32 = 0; i < 10; i += 1 {
        if i == 2 { continue; }
        if i == 7 { break; }
        sum += i;
    }
    assert[i32](sum, 19);
    let i: i32 = 0;
    while i < 3 {
        i += 1;
        let j: i32 = 0;
        while j < 4 { j += 1; if j == 2 { break; } }
        assert[i32](j, 2);
    }
    assert[i32](i, 3);
    for ;; { break; }

    events = 0;
    assert[bool](false && side(true), false);
    assert[bool](true || side(false), true);
    let flag = false;
    flag &&= side(true);
    flag ||= side(true);
    assert[bool](flag, true);
    assert[i64](events, 1);

    events = 0;
    assert[i32](choose(true).first, 7);
    assert[i64](events, 13);
    events = 0;
    assert[i64](choose(false).second, 10);
    assert[i64](events, 23);
    assert[i32](captured_return(), 7);
    events = 0;
    {
        defer mark(9);
        for let i: i64 = 1; i < 4; i += 1 {
            defer mark(i);
            if i == 1 { continue; }
            if i == 2 { break; }
        }
        assert[i64](events, 12);
        defer { let value: i64 = 8; defer mark(value); mark(7); }
    }
    assert[i64](events, 12789);

    let small: i8 = 127;
    small += 1;
    assert[i8](small, -128);
    assert[i64](small as i64, -128);
    small /= -1;
    assert[i8](small, -128);
    let byte: u8 = 250;
    byte += 10;
    assert[u8](byte, 4);
    byte <<= 8;
    assert[u8](byte, 0);
    let signed: i16 = -1024;
    signed >>= 3 as u8;
    assert[i16](signed, -128);
    let unsigned: u16 = 65535;
    unsigned += 1;
    assert[u16](unsigned, 0);
    let narrow: i32 = -7;
    assert[i32](narrow / 2, -3);
    assert[i32](narrow % 3, -1);
    let bits: u32 = 4294967295;
    assert[u32](bits / 2, 2147483647);
    assert[bool](bits > 2147483647, true);
    bits += 1;
    assert[u32](bits, 0);
    let wide: u64 = 18446744073709551615;
    assert[u64](wide >> 60 as i8, 15);
    assert[bool](wide > 9223372036854775807, true);
    wide += 1;
    assert[u64](wide, 0);
    let shifted: i64 = 1;
    shifted <<= 40 as i32;
    assert[i64](shifted, 1099511627776);
    let native_word: usize = 4294967296;
    assert[usize](native_word + 1, 4294967297);
    assert[usize](native.size_of[usize](), 8);
    assert[usize](native.size_of[*i32](), 8);
    assert[usize](native.size_of[*[i32]](), 16);
    assert[usize](native.align_of[Pair[i32]](), 8);
    let native_signed: isize = -4294967296;
    assert[i64](native_signed as i64, -4294967296);
    let bitwise: i32 = 12;
    bitwise &= 10;
    bitwise |= 1;
    bitwise ^= 3;
    assert[i32](bitwise, 10);
    assert[i32](~bitwise, -11);
    assert[i32](-bitwise, -10);

    let float: f32 = 1.5;
    float *= 2.0;
    assert[f32](float, 3.0);
    assert[f64](float as f64, 3.0);
    assert[i8]((float + 255.0) as i8, 2);
    assert[f64]((small as f64), -128.0);
    let double: f64 = -3.75;
    assert[i32](double as i32, -3);
    assert[f64](floor(double), -4.0);
    assert[f32](ceil(float + 0.25), 4.0);
    assert[f32]((-double) as f32, 3.75);
    assert[bool](double < 0.0 && double <= -3.75 && float > 0.0 && float >= 3.0, true);
    let zero: f64 = 0.0;
    let nan = zero / zero;
    assert[bool](nan == nan, false);
    assert[bool](nan != nan, true);

    global_pair.first += 4;
    assert[i32](global_pair.first, 7);
    let copied = id[Pair[i32]](global_pair);
    copied.first = 11;
    assert[i32](global_pair.first, 7);
    let indirect_pair: fn(Pair[i32]): Pair[i32] = id[Pair[i32]];
    assert[Pair[i32]](indirect_pair(copied), copied);
    assert[Empty](id[Empty](Empty{}), Empty{});

    let raw = native.malloc(3 * native.size_of[i32]()) as [*]i32;
    if raw as usize == 0 { native.exit(1); }
    defer native.free(raw as [*]u8);
    raw[0].* = 7;
    raw[1].* = 11;
    raw[2].* = 19;
    let slice = *[i32]{ptr: raw, len: 3};
    assert[i32](slice[0 as i8].*, 7);
    assert[i32](slice[1 as u16].*, 11);
    assert[i32](slice[2 as i64].*, 19);
    assert[*[i32]](id[*[i32]](slice), slice);
    assert[bool](slice != *[i32]{ptr: raw, len: 2}, true);
    active = slice;
    events = 0;
    source()[index()].* += 5;
    assert[i64](events, 12);
    assert[i32](raw[1].*, 16);
    assert[usize](active.len, 0);

    let record = Record{tag: 9, pair: copied, data: slice};
    let pointer = native.malloc(native.size_of[Record]()) as *Record;
    if pointer as usize == 0 { native.exit(1); }
    defer native.free(pointer as [*]u8);
    pointer.* = id[Record](record);
    pointer.*.pair.first += 10;
    pointer.pair.second.* += 20;
    pointer.*.data = *[i32]{ptr: raw, len: 2};
    assert[i32](pointer.pair.first.*, 21);
    assert[i64](pointer.pair.second.*, 24);
    assert[usize](pointer.data.*.len, 2);
    assert[u8](pointer.tag.*, 9);
    let step = (raw as usize + native.size_of[i32]()) as *i32;
    assert[i32](step.*, 16);
    assert[usize](step as usize - raw as usize, 4);
    native.puts(message);
}
