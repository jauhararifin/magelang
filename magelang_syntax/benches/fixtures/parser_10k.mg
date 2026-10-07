// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair<T, U> {
    first: T,
    second: U,
}
struct Vec3 {
    x: f64,
    y: f64,
    z: f64,
}
struct Job {
    id: u64,
    next: *Job,
    samples: [*]f32,
    callback: fn(*Job, i32): bool,
}
let default_limit: i32 = 100;
let epsilon: f64 = 0.000_001;
let enabled: bool = true;
let greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity<T>(value: T): T {
    return value;
}
fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
    return Pair<T, U>{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    let x = left.x * right.x;
    let y = left.y * right.y;
    let z = left.z * right.z;
    return x + y + z;
}
fn classify(value: i32): i32 {
    if value <= 0 {
        return -1;
    } else if value == 1 || value == 2 {
        return 0;
    } else {
        return 1;
    }
}
fn process(job: *Job, count: i32): i64 {
    let total: i64 = 0;
    let index: i32 = 0;
    let mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        let sample = job.samples[index].*;
        let rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for let i = 0; i != count; i += 1 {
        let mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    let arithmetic = a + b * 3 - (a / 2) % 7;
    let bits = (a << 2) | (b >> 1) ^ ~a;
    let ordered = arithmetic >= bits && a != b;
    let either = ordered || false;
    let negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    let decimal = 1_234_567;
    let hexadecimal = 0xdead_beef;
    let octal = 0o755;
    let binary = 0b1010_1100;
    let floating = 6.022e23;
    let newline = '\n';
    let nothing = null;
    let text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity<i32>(value);
}
