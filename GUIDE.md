# Magelang Programming Language Guide

This guide documents the Magelang language as implemented in this repository. Magelang is a small,
WIP systems language that currently targets WebAssembly. The syntax is influenced by Go, Rust, and C,
with explicit types, mutable variables, package imports, structs, pointers, generics, and direct WASM
interop.

> Magelang is still experimental. Syntax and semantics may change.

## Table of Contents

- [Quick Start](#quick-start)
- [Program Structure](#program-structure)
- [Packages and Imports](#packages-and-imports)
- [Comments](#comments)
- [Types](#types)
- [Variables and Globals](#variables-and-globals)
- [Functions](#functions)
- [Generics and Constraints](#generics-and-constraints)
- [Structs](#structs)
- [Pointers and Memory](#pointers-and-memory)
- [Expressions and Operators](#expressions-and-operators)
- [Statements and Control Flow](#statements-and-control-flow)
- [Annotations and WebAssembly Interop](#annotations-and-webassembly-interop)
- [Standard Library](#standard-library)
- [Complete Examples](#complete-examples)
- [Current Limitations](#current-limitations)

## Quick Start

Set `MAGELANG_ROOT` so Magelang can find the standard library:

```bash
export MAGELANG_ROOT=./magelang
```

Create `hello.mg`:

```mg
import fmt "std/fmt";

@main()
fn main() {
  fmt::print_str("Hello, Magelang!\n");
}
```

Run it directly:

```bash
cargo run -- run hello
```

Or compile it to WebAssembly:

```bash
cargo run -- compile hello -o hello.wasm
wasmtime hello.wasm
```

The package name `hello` maps to `hello.mg` in the current directory, unless the path resolves to a
standard library package first.

## Command Line

The compiler binary supports these commands:

```bash
magelang parse <file.mg> [-o ast.txt]
magelang analyze <package> [-d] [-o analysis.txt]
magelang compile <package> [-d] [-n] [-o output.wasm]
magelang run <package> [-d]
```

Where:

- `parse` parses one source file and prints the AST.
- `analyze` resolves packages, typechecks, and prints the analyzed module.
- `compile` emits a `.wasm` file.
- `run` compiles in memory and runs the module with Wasmtime + WASI.
- `-d` enables debug-style error reporting.
- `-n` / `--noopt` disables Binaryen optimization for `compile`.

## Program Structure

A Magelang source file is a package. Top-level declarations can appear in any order.

Top-level items are:

```mg
import fmt "std/fmt";

let global_counter: i32 = 0;

struct Point {
  x: i32,
  y: i32,
}

fn add(a: i32, b: i32): i32 {
  return a + b;
}
```

A package can contain:

- imports
- global variables
- structs
- functions

Every top-level item is currently public to importing packages.

## Packages and Imports

Import another package with:

```mg
import name "path/to/package";
```

Use items from that package with `name::item`:

```mg
import fmt "std/fmt";
import mem "std/mem";

fn main() {
  fmt::print_str("hello\n");
  let p = mem::alloc::<i32>();
}
```

Package paths do not include the `.mg` extension. For example:

```mg
import math "lib/math";
```

loads `lib/math.mg`, unless `std` resolution finds a standard library package first.

### Standard Library Resolution

Magelang looks for standard library packages under `lib/`. It tries several locations, including:

- Cargo manifest directory when running from this workspace
- `$MAGELANG_ROOT/lib/`
- paths relative to the compiler executable
- `~/magelang/lib/`

For development in this repository, use:

```bash
export MAGELANG_ROOT=./magelang
```

## Comments

Line comments start with `//`:

```mg
// This is a comment.
let x: i32 = 1; // This is also a comment.
```

Block comments are not supported yet.

## Types

### Primitive Types

Magelang has these primitive types:

```mg
i8 i16 i32 i64 isize
u8 u16 u32 u64 usize
f32 f64
bool
void
opaque
```

- `i*` are signed integers.
- `u*` are unsigned integers.
- `isize` and `usize` are pointer-sized integer types for the current WASM target.
- `f32` and `f64` are floating point types.
- `bool` is either `true` or `false`.
- `void` is the no-value type.
- `opaque` represents an opaque host/WebAssembly reference-like value.

### Integer and Floating Literals

Integer literals are initially untyped and become concrete from context:

```mg
let a: i32 = 10;
let b: u64 = 10;
let c = 10; // defaults to isize when no context is available
```

Floating literals are also initially untyped:

```mg
let a: f32 = 1.5;
let b: f64 = 1.5;
let c = 1.5; // defaults to f64 when no context is available
```

Supported number forms include:

```mg
100
1_000_000
1e2
0xff
0o755
0b1010_0101
0777
3.1415
1.2e-3
```

### Boolean Literals

```mg
let yes: bool = true;
let no: bool = false;
```

### Character Literals

Character literals are integer constants containing the code point value:

```mg
let newline: u8 = '\n';
let letter_a: i32 = 'a';
```

### String Literals

String literals are null-terminated byte arrays and have type `[*]u8`:

```mg
let msg: [*]u8 = "hello\n";
```

Supported escapes include common escapes such as `\n`, `\t`, `\\`, `\"`, and raw bytes like
`\x00`.

### Pointer Types

Magelang has two pointer types:

```mg
*T    // pointer to one T
[*]T  // pointer to an array of T
```

Examples:

```mg
let p: *i32;
let array: [*]i32;
```

See [Pointers and Memory](#pointers-and-memory) for details.

### Struct Types

```mg
struct Point {
  x: i32,
  y: i32,
}
```

Use the struct name as a type:

```mg
let p: Point;
```

### Function Types

Function values have `fn(...)` types:

```mg
fn inc(x: i32): i32 {
  return x + 1;
}

let f: fn(i32): i32 = inc;
let y = f(41);
```

Function type parameters can optionally be named for readability:

```mg
let f: fn(value: i32): i32 = inc;
```

## Variables and Globals

### Local Variables

Local variables use `let`:

```mg
fn main() {
  let a: i32 = 10;
  let b = 20;
  let c: i32;

  c = a + b;
}
```

Local variables are mutable by default. There is no `const` or immutable local variable feature yet.

Local variables can shadow earlier locals:

```mg
fn main() {
  let x: i32 = 10;
  let x: bool = true;
}
```

### Global Variables

Global variables also use `let`, but must have explicit types:

```mg
let counter: i32 = 0;
let default_zero: i64;
```

Globals without an initializer are zero-initialized.

Global initializers may depend on other globals and functions. The compiler computes a global
initialization order and reports cycles.

```mg
let a: i64 = b + 1;
let b: i64 = 41;
```

## Functions

Define functions with `fn`:

```mg
fn add(a: i32, b: i32): i32 {
  return a + b;
}
```

A function with no return type returns `void`:

```mg
fn print_done() {
  fmt::print_str("done\n");
}
```

Return from a function with `return`:

```mg
fn abs(x: i32): i32 {
  if x < 0 {
    return -x;
  }
  return x;
}
```

Function declarations can appear in any order:

```mg
fn a(): i32 {
  return b();
}

fn b(): i32 {
  return 10;
}
```

### Main Function

A function annotated with `@main()` is run during module initialization:

```mg
@main()
fn main() {
  // program entry point
}
```

A main function must not take parameters and must not return a value.

Some WASM runtimes expect an exported `_start` function. You can also export a function:

```mg
@main()
@wasm_export("_start")
fn main() {
  // entry point for WASI-style runtimes
}
```

## Generics and Constraints

Functions and structs can be generic.

### Generic Functions

```mg
fn identity<T>(value: T): T {
  return value;
}

fn main() {
  let a = identity::<i32>(10);
  let b = identity::<bool>(true);
}
```

Generic function calls use `::<...>`:

```mg
identity::<i32>(10)
```

### Generic Structs

```mg
struct Box<T> {
  value: T,
}

fn main() {
  let b = Box::<i32>{ value: 10 };
}
```

In type positions, generic type arguments are written without `::`:

```mg
let b: Box<i32>;
let p: *Box<i32>;
```

In expression positions, such as struct literals, use `::<...>`:

```mg
let b = Box::<i32>{ value: 10 };
```

### Explicit Generic Constraints

Magelang does not infer generic capabilities from function bodies. If generic code uses an operation
on a type parameter, the required constraint must be written explicitly using a `where` clause.

```mg
fn max<T>(a: T, b: T): T
  where T: @ordered<T>
{
  if a < b {
    return b;
  }
  return a;
}
```

Multiple constraints are separated by commas:

```mg
fn sum_array<T>(values: [*]T, len: usize): T
  where T: @numeric, T: @derefable
{
  let result: T = 0;
  let i: usize = 0;
  while i < len {
    result = result + values[i].*;
    i = i + 1;
  }
  return result;
}
```

### Built-in Constraints

| Constraint | Meaning | Commonly needed for |
|---|---|---|
| `@numeric` | Type supports numeric arithmetic | `+`, `-`, `*`, `/`, unary `+`, unary `-`, numeric literals |
| `@integer` | Type is an integer type | `%`, `|`, `&`, `^`, `<<`, `>>`, `~`; also satisfies `@numeric` |
| `@comparable<T>` | Type can be equality-compared with `T` | `==`, `!=` |
| `@ordered<T>` | Type can be ordered compared with `T` | `<`, `>`, `<=`, `>=`; also satisfies `@comparable<T>` |
| `@derefable` | Generic type can be loaded/stored through pointers | `p.*`, `array[i].*` when pointed type contains a type parameter |
| `@castable<U>` | Type can be cast to `U` | `value as U` |

Examples:

```mg
fn eq<T>(a: T, b: T): bool
  where T: @comparable<T>
{
  return a == b;
}

fn min<T>(a: T, b: T): T
  where T: @ordered<T>
{
  if a < b {
    return a;
  }
  return b;
}

fn add<T>(a: T, b: T): T
  where T: @numeric
{
  return a + b;
}

fn bit_or<T>(a: T, b: T): T
  where T: @integer
{
  return a | b;
}

fn cast<T, U>(value: T): U
  where T: @castable<U>
{
  return value as U;
}
```

### Generic Recursion

Generic functions can call themselves or other generic functions, but constraints must be propagated
explicitly:

```mg
fn even_countdown<T>(n: T): T
  where T: @integer, T: @comparable<T>
{
  let zero: T = 0;
  if n == zero {
    return zero;
  }
  let one: T = 1;
  return odd_countdown::<T>(n - one);
}

fn odd_countdown<T>(n: T): T
  where T: @integer, T: @comparable<T>
{
  let zero: T = 0;
  if n == zero {
    return zero;
  }
  let one: T = 1;
  return even_countdown::<T>(n - one);
}
```

If a generic function calls another generic function, the caller must have constraints sufficient to
instantiate the callee:

```mg
fn needs_ordered<T>(a: T, b: T): T
  where T: @ordered<T>
{
  if a < b {
    return b;
  }
  return a;
}

fn caller<T>(a: T, b: T): T
  where T: @ordered<T>
{
  return needs_ordered::<T>(a, b);
}
```

## Structs

Define structs with `struct`:

```mg
struct Vec2 {
  x: f64,
  y: f64,
}
```

Construct a struct with a struct literal:

```mg
let p = Vec2{ x: 1.0, y: 2.0 };
```

Fields can be omitted; omitted fields are zero-initialized:

```mg
let p = Vec2{ x: 1.0 }; // y is zero
```

Access fields with `.`:

```mg
let x = p.x;
p.y = 3.0;
```

### Structs Behind Pointers

When a struct is behind a pointer, field selection returns the address of the field:

```mg
let p = mem::alloc::<Vec2>();
p.x.* = 1.0;
p.y.* = 2.0;
```

Here `p.x` has type `*f64`, so `p.x.*` reads or writes the actual value.

### Generic Structs

```mg
struct Pair<T, U> {
  first: T,
  second: U,
}

fn main() {
  let p = Pair::<i32, bool>{ first: 1, second: true };
}
```

Generic structs currently do not have `where` constraints. Put constraints on functions that operate
on them.

### Circular Structs

A struct cannot directly contain itself because it would have infinite size:

```mg
// Invalid
struct Node {
  next: Node,
}
```

Use a pointer for recursive data structures:

```mg
struct Node {
  value: i32,
  next: *Node,
}
```

## Pointers and Memory

Magelang exposes pointer operations directly. This is useful for low-level code and WASM interop.

### Unit Pointers

A unit pointer `*T` points to one value of type `T`.

```mg
import mem "std/mem";

fn main() {
  let p = mem::alloc::<i32>();
  p.* = 123;
  let x = p.*;
  mem::dealloc::<i32>(p);
}
```

Dereference with `.*`.

### Array Pointers

An array pointer `[*]T` points to a sequence of values:

```mg
let arr = mem::alloc_array::<i32>(10);
arr[0].* = 42;
arr[1].* = arr[0].* + 1;
```

Indexing an array pointer returns a unit pointer to the element. Therefore:

```mg
arr[i]    // *T
arr[i].*  // T
```

### Null Pointers

A common null pointer pattern is casting zero:

```mg
let p: *i32 = 0 as *i32;
let a: [*]i32 = 0 as [*]i32;
```

### Pointer Casts

Pointers can be cast to and from integral types:

```mg
let p = mem::alloc::<i32>();
let addr = p as usize;
let again = addr as *i32;
```

### Memory Allocation

The standard library provides a simple allocator in `std/mem`:

```mg
import mem "std/mem";

let p = mem::alloc::<i32>();
let arr = mem::alloc_array::<i32>(16);

mem::dealloc::<i32>(p);
mem::dealloc_array::<i32>(arr);
```

## Expressions and Operators

### Arithmetic

```mg
let x = a + b;
let y = a - b;
let z = a * b;
let q = a / b;
```

Arithmetic works on numeric types. `%` works only on integers.

### Bitwise Operations

```mg
let a = x | y;
let b = x & y;
let c = x ^ y;
let d = x << 1;
let e = x >> 1;
let f = ~x;
```

Bitwise operations require integer types.

### Boolean Operations

```mg
let a = true && false;
let b = true || false;
let c = !a;
```

The operands of `&&`, `||`, and `!` must be `bool`.

### Comparisons

Equality:

```mg
x == y
x != y
```

Ordering:

```mg
x < y
x <= y
x > y
x >= y
```

For generic equality, use `@comparable<T>`. For generic ordering, use `@ordered<T>`.

### Casts

Use `as` for explicit casts:

```mg
let x: i64 = 42 as i64;
let y: f64 = x as f64;
let addr: usize = p as usize;
let p2: *i32 = addr as *i32;
```

Current casts support numeric-to-numeric and integral/pointer conversions.

### Operator Precedence

From lowest to highest precedence:

1. `||`
2. `&&`
3. `|`
4. `^`
5. `&`
6. `==`, `!=`
7. `<`, `<=`, `>`, `>=`
8. `<<`, `>>`
9. `+`, `-`
10. `*`, `/`, `%`
11. unary `+`, `-`, `!`, `~`
12. calls, indexing, field selection, dereference

Use parentheses when in doubt:

```mg
let x = (a + b) * c;
```

## Statements and Control Flow

### Blocks

A block is a sequence of statements inside `{}`:

```mg
{
  let x = 1;
  let y = x + 2;
}
```

### Assignment

```mg
x = 10;
p.* = 20;
arr[i].* = 30;
point.x = 40;
```

Assignment requires the same type on both sides. Use explicit casts when needed.

### If / Else

```mg
if x < 0 {
  return -x;
} else if x == 0 {
  return 0;
} else {
  return x;
}
```

Conditions must be `bool`.

### While

```mg
let i = 0;
let sum = 0;
while i < 10 {
  sum = sum + i;
  i = i + 1;
}
```

### Break and Continue

```mg
while true {
  if should_skip() {
    continue;
  }
  if should_stop() {
    break;
  }
}
```

### Return

```mg
return;
return value;
```

A non-void function must return a value on every path.

## Annotations and WebAssembly Interop

Annotations are written with `@name(...)` before an item.

### `@main()`

Marks the function to run on module initialization:

```mg
@main()
fn main() {
}
```

### `@wasm_import(module, name)`

Imports a host/WASM function. Imported functions have no body and end with `;`:

```mg
@wasm_import("wasi_snapshot_preview1", "fd_write")
fn fd_write(fd: i32, iovec_addr: *IoVec, count: i32, n_written_ptr: *i32): i32;
```

### `@wasm_export(name)`

Exports a function:

```mg
@wasm_export("add")
fn add(a: i32, b: i32): i32 {
  return a + b;
}
```

### `@intrinsic(name)`

Declares a compiler-provided intrinsic. The standard library uses this for low-level WASM functions:

```mg
@intrinsic("size_of")
fn size_of<T>(): usize;
```

Most user programs should import `std/wasm` rather than declare intrinsics directly.

### `@embed_file(path)`

Embeds a file into the data segment and initializes a byte pointer global to its address:

```mg
@embed_file("assets/message.txt")
let message: [*]u8;
```

The embedded bytes are not automatically null-terminated unless the source file contains a null byte.

## Standard Library

The current standard library is small and lives under `magelang/lib/std`.

### `std/fmt`

Printing helpers:

```mg
import fmt "std/fmt";

fmt::print_str("hello\n");
fmt::print_i32(123);
fmt::print_i64(123);
fmt::print_u64(123);
fmt::print_usize(123);
```

### `std/mem`

A simple allocator:

```mg
import mem "std/mem";

let p = mem::alloc::<i32>();
p.* = 10;
mem::dealloc::<i32>(p);

let arr = mem::alloc_array::<i32>(10);
mem::dealloc_array::<i32>(arr);
```

### `std/vector`

A generic growable vector:

```mg
import mem "std/mem";
import vector "std/vector";

let v = mem::alloc::<vector::Vector<i32>>();
vector::init::<i32>(v);
vector::push::<i32>(v, 10);
vector::push::<i32>(v, 20);

let n = vector::len::<i32>(v);
let first = vector::get::<i32>(v, 0);
vector::set::<i32>(v, 1, 30);
```

Other helpers include `reserve`, `resize`, `insert`, `remove`, `pop`, `back`, `clear`, `cap`,
`is_empty`, `swap`, and `deinit`.

### `std/alg`

Common generic algorithms:

```mg
import alg "std/alg";

let hi = alg::max::<i32>(10, 20);
let lo = alg::min::<i32>(10, 20);
let bounded = alg::clamp::<i32>(value, 0, 100);
let total = alg::sum::<i32>(items, len);
```

Also includes `product`, `fill`, `copy`, `reverse`, `contains`, and `index_of`.

### `std/sort`

Sorting helpers for array pointers:

```mg
import sort "std/sort";

sort::sort::<i32>(items, len);
let ok = sort::is_sorted::<i32>(items, len);
```

### `std/hash`, `std/hashmap`, and `std/hashset`

`std/hash` provides integer, pointer, byte-slice, and C-string hash helpers. `std/hashmap` and
`std/hashset` provide open-addressed hash tables for key types that can be cast to `usize`, compared,
and dereferenced.

```mg
import hashmap "std/hashmap";

let map = mem::alloc::<hashmap::HashMap<i32, i64>>();
hashmap::init::<i32, i64>(map);
hashmap::set::<i32, i64>(map, 10, 100);

let out = mem::alloc::<i64>();
if hashmap::get::<i32, i64>(map, 10, out) {
  // out.* is 100
}
```

### `std/priority_queue`

Binary heap priority queues:

```mg
import pq "std/priority_queue";

let queue = mem::alloc::<pq::PriorityQueue<i32>>();
pq::init_min::<i32>(queue);
pq::push::<i32>(queue, 3);
pq::push::<i32>(queue, 1);
let smallest = pq::pop::<i32>(queue);
```

Use `init_max` for a max-heap.

### `std/slab`

A segregated free-list allocator with fixed size classes up to 512 bytes, plus fallback to
`std/mem` for larger allocations:

```mg
import slab "std/slab";

let allocator = mem::alloc::<slab::SlabAllocator>();
slab::init(allocator);

let p = slab::alloc::<i32>(allocator);
p.* = 123;
slab::dealloc::<i32>(allocator, p);
```

### `std/wasm`

Compiler intrinsics for WASM:

```mg
import wasm "std/wasm";

let size = wasm::size_of::<i32>();
let align = wasm::align_of::<i32>();
let pages = wasm::memory_size();
let old_pages = wasm::memory_grow(1);
wasm::unreachable();
```

### `std/wasi`

Raw WASI bindings and structs. Most user code should use `std/fmt` unless it needs direct WASI calls.

## Complete Examples

### Generic Max, Min, and Clamp

```mg
fn max<T>(a: T, b: T): T
  where T: @ordered<T>
{
  if a < b {
    return b;
  }
  return a;
}

fn min<T>(a: T, b: T): T
  where T: @ordered<T>
{
  if a < b {
    return a;
  }
  return b;
}

fn clamp<T>(value: T, low: T, high: T): T
  where T: @ordered<T>
{
  return min::<T>(max::<T>(value, low), high);
}
```

### Generic Sum

```mg
fn sum<T>(values: [*]T, len: usize): T
  where T: @numeric, T: @derefable
{
  let result: T = 0;
  let i: usize = 0;
  while i < len {
    result = result + values[i].*;
    i = i + 1;
  }
  return result;
}
```

### Generic Sort

```mg
fn sort<T>(values: [*]T, len: usize)
  where T: @ordered<T>, T: @derefable
{
  let i: usize = 0;
  while i < len {
    let j = i + 1;
    while j < len {
      if values[j].* < values[i].* {
        swap::<T>(values, i, j);
      }
      j = j + 1;
    }
    i = i + 1;
  }
}

fn swap<T>(values: [*]T, a: usize, b: usize)
  where T: @derefable
{
  let tmp = values[a].*;
  values[a].* = values[b].*;
  values[b].* = tmp;
}
```

### Hello World Without `std/fmt`

This example calls WASI directly:

```mg
@main()
@wasm_export("_start")
fn main() {
  let msg = "Hello, world\n";
  let iovec = 40 as *IoVec;

  iovec.p.* = msg;
  iovec.len.* = 13;

  fd_write(1, iovec, 1, 0 as *i32);
}

struct IoVec {
  p: [*]u8,
  len: i32,
}

@wasm_import("wasi_snapshot_preview1", "fd_write")
fn fd_write(fd: i32, iovec_addr: *IoVec, count: i32, n_written_ptr: *i32): i32;
```

## Current Limitations

Magelang is intentionally small and still under development. Current limitations include:

- WebAssembly is the only backend.
- No package manager yet.
- No methods; functions are always free functions.
- No function overloading.
- No `for` loop yet; use `while`.
- No block comments.
- No immutable variables or constants yet.
- No traits/interfaces; generic constraints are built-in and explicit.
- No value array type yet; use array pointers `[*]T`.
- Memory management is manual.
- Generic struct constraints are not supported; constrain functions instead.

Despite these limitations, Magelang can already express low-level WASM programs, generic algorithms,
manual memory management, direct WASI interop, and simple reusable standard-library code.
