// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}

// A representative mix of declarations, statements, types, and expressions.
import math "std/math";
@layout("packed")
struct Pair[T, U] {
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
var default_limit: i32 = 100;
var epsilon: f64 = 0.000_001;
var enabled: bool = true;
var greeting: [*]u8 = "benchmark\n";
@wasm_import("host", "log")
fn host_log(message: [*]u8, length: usize): i32;
fn identity[T](value: T): T {
    return value;
}
fn make_pair[T, U](first: T, second: U): Pair[T, U] {
    return Pair[T, U]{first: first, second: second};
}
fn dot(left: Vec3, right: Vec3): f64 {
    var x = left.x * right.x;
    var y = left.y * right.y;
    var z = left.z * right.z;
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
    var total: i64 = 0;
    var index: i32 = 0;
    var mask: u64 = 0xff;
    defer host_log("done\n", 5);
    while index != count {
        var sample = job.samples[index].*;
        var rounded = sample as i64;
        total += rounded;
        index += 1;
        if rounded % 2 == 0 {
            continue;
        }
        if total > 10_000 {
            break;
        }
    }
    for var i = 0; i != count; i += 1 {
        var mixed = (i * 31 + 7) ^ (i >> 2);
        total += mixed as i64;
    }
    mask = (mask << 8) | 0xaa;
    job.id.* = job.id.* & mask;
    return total;
}
fn expressions(a: i32, b: i32, ptr: *i32): bool {
    var arithmetic = a + b * 3 - (a / 2) % 7;
    var bits = (a << 2) | (b >> 1) ^ ~a;
    var ordered = arithmetic >= bits && a != b;
    var either = ordered || false;
    var negated = !either;
    ptr.* = arithmetic;
    ptr.* += bits;
    return !negated;
}
fn literals(): i64 {
    var decimal = 1_234_567;
    var hexadecimal = 0xdead_beef;
    var octal = 0o755;
    var binary = 0b1010_1100;
    var floating = 6.022e23;
    var newline = '\n';
    var nothing = null;
    var text = "escaped:\t\\\"\x21";
    return decimal + hexadecimal + octal + binary;
}
fn nested(value: i32): i32 {
    {
        defer host_log("scope", 5);
        if value > default_limit {
            return classify(value);
        }
    }
    return identity[i32](value);
}
