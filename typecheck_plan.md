# `magelang_typecheck` → `magelang_typecheck2`: analysis and plan

Companion to `BUGS.md` / `FIX_PLAN.md`. This document records why the type checker is
being rewritten, the design of the replacement crate `magelang_typecheck2`, and how the
rewrite is verified. The old crate stays in the workspace, untouched, until the new one
has been reviewed; wiring `magelang_wasmgen` and the `magelang` CLI to the new crate is a
separate step (§4), to be done after the crate itself has been reviewed.

## Decisions taken

| Decision | Choice |
|---|---|
| Instantiation strategy | **Re-check generic bodies per instantiation** (no IR substitution / monomorphization pass) |
| Definition-time checking of generic bodies | **Kept** — unused generic code with an error still fails to compile |
| Struct typing | **Nominal** for named structs (fixes BUGS.md #2) |
| Where the code lives | New crate `magelang_typecheck2`, same public IR as the old crate |
| Consumers | Not switched yet: reviewed in isolation first, integration with `magelang_wasmgen` and `magelang` afterwards (§4); the old crate is kept for reference until then |
| Branch | `feat_typecheck2` off `master` |

## 1. Where the complexity in the old crate comes from

1. **Generic definitions are represented as `Type`s** (`TypeKind::GenericStruct` /
   `GenericFunc`, `ty.rs:392-399`). A type constructor pretends to be a type, so every
   consumer has to handle "types that aren't types", and `GenericType` carries a
   `RefCell<HashMap>` mono-cache *inside the interned type* (`ty.rs:439-443`).
2. **Function types are sometimes nominal.** Instantiated generic functions get
   `TypeKind::Inst`, so to let `let f: fn(i32): i32 = id::<i32>` compile,
   `is_assignable_with` (`ty.rs:380-389`) accepts equal kind *or* equal repr. That escape
   hatch makes structurally identical named structs interchangeable (BUGS.md #2).
3. **Struct bodies are filled lazily with an ordering dependency.** `specialize` can only
   fill an instance body once the generic's own body exists; the repair loop in
   `init_body` (`ty.rs:121-139`) is a no-op because `specialize` returns early on a cache
   hit (BUGS.md #3).
4. **Three whole-tree passes for generic functions:** discovery
   (`analyze.rs:842-1018`), IR deep-copy with substitution (`expr.rs:24-182`,
   `statement.rs:29-89`), and type substitution (`ty.rs:142-313`), plus two more full
   `ExprKind` walks in `global_init.rs` and wasmgen's `data.rs`.
5. **Scope objects double as phase state** (`analyze.rs:193-251`): AST + `OnceCell`s
   filled by later phases, and exported publicly although unused outside the crate.
6. **Copy-paste in `expr.rs`:** six near-identical binary-operator checkers plus a trait
   with five macro families (~650 lines); untyped-constant conversion written three
   times; duplicated path lookup; a statement interner that is the only reason
   `Expr`/`Statement`/`Float` implement `Hash`. One real bug: `expr.rs:1295` and `:1377`
   compute `b_is_untyped_float` from `a.ty`.

## 2. The design decision

Magelang has no bounds/traits, so a type parameter is completely opaque inside a generic
body: it can be assigned, passed and compared with `==`/`!=`, nothing else. Any concrete
type therefore supports a superset of what the placeholder allows, and a body that passes
the definition check passes for every instantiation (the one exception is `opaque`, whose
`==` is only allowed against `null`).

| Option | Mechanism | Machinery |
|---|---|---|
| A. check once, substitute (old) | check with placeholder types, deep-copy the checked IR with substitution | type substitution, IR substitution, discovery walk, body-fill ordering protocol |
| **B. instantiate by re-checking** | run the same checker again with `T ↦ concrete type`; the checker records instantiation requests as it meets `f::<i32>` | an instance cache and a worklist |

B is chosen, plus the definition-time check (the checker run once with `T ↦ Param`, result
discarded). This deletes `substitute`, `specialize`, `init_body`, `monomorphize`,
`get_all_monomorphized_funcs`, `mono_cache` and four `TypeKind` variants.

Trade-offs: each generic body is checked 1 + N times instead of once + N substitution
copies (a wash — substitution is also a full tree walk that allocates); diagnostics that
only exist for one instantiation (`opaque ==`) are reported at the body position; the
definition-check IR is discarded. Duplicate diagnostics are not a concern: `ErrorManager`
stores errors in an `IndexSet` keyed on (pos, message) and sorts on `take()`.

A cleaned-up A was considered: its type-level substitution is small, but struct bodies then
need a template-then-substitute protocol, and mutually recursive generic structs leave the
template "in progress" when an instance is requested — the exact class of subtlety that
produced bug #3. Re-resolving the field ASTs per instance has no such state.

## 3. Target design (as implemented in `magelang_typecheck2`)

### Data model

```rust
// ty.rs — a Type is a fully applied, interned, immutable value
pub struct Type<'a> { pub kind: TypeKind<'a>, pub repr: TypeRepr<'a> }
pub enum TypeKind<'a> {
    Named { def_id: DefId<'a>, type_args: &'a TypeArgs<'a> }, // struct instance; empty args for non-generic
    Anonymous,                                                // everything else, incl. ALL function types
}
pub enum TypeRepr<'a> { Unknown, Void, Opaque, Bool, UntypedInt, Int(..), UntypedFloat, Float(..),
    Ptr(&Type), ArrayPtr(&Type), Func(FuncType), Struct(StructType) /* only under Named */,
    Param(TypeParam) /* only during definition checks */ }
```

- Equality/hash: `Named` compares `(def_id, type_args)`; `Anonymous` compares `repr`.
- Assignability: `a == b || either is Unknown` — nominal for structs, structural for
  pointers and functions. Instantiated generic functions have anonymous function types.

```rust
// def.rs — what the source declares (phase 2), immutable afterwards
enum Def { Import, Struct(StructDef), Func(FuncDef), Global(GlobalDef) }   // stored in Definitions, keyed by DefId
StructDef { def_id, pos, type_params: &[Symbol], node, identity: OnceCell<&Type> /* phase 3 */ }
FuncDef   { def_id, pos, type_params, node, annotations, sig: OnceCell<&Type> /* 3 */, body_ok: Cell<bool> /* 4 */ }
GlobalDef { def_id, pos, node, annotations, ty: OnceCell<&Type> /* 3 */, value: OnceCell<Expr> /* 4 */ }

// instance.rs — the only generic machinery
Instances { structs: (DefId, &TypeArgs) → &Type, funcs: (DefId, &TypeArgs) → &FuncInstance, pending: VecDeque<&FuncInstance> }
FuncInstance { def_id, type_args, ty: &Type, body: OnceCell<&Statement> /* phase 5 */, queued: Cell<bool> }
struct_instance(def, args, pos) -> &Type          // cache-insert first, then fill the body from the field ASTs under {params ↦ args}
func_instance(def, args, pos)   -> &FuncInstance  // resolve the signature under {params ↦ args}; queue if concrete and new
```

Scopes keep three namespaces (types / values / imports — a struct and a function may share
a name), but entries are `DefId`s or plain types, not objects with mutable state. Non-generic
functions are instances with empty type arguments, so there is exactly one body-checking
path: `func::check_body(def, instance)`.

### Invariants (documented in `instance.rs`)

1. `(def_id, args)` maps to at most one instance; args are interned, identity is pointer identity.
2. A struct instance is cached *before* its body is computed. The only way to observe an
   instance without a body is a by-value reference to itself during its own body
   computation — an infinite-size type, diagnosed in phase 6. Expression checking never
   runs inside body computation, so it always sees complete bodies.
3. Instances whose args contain a `Param` are definition-check artifacts: never queued,
   never emitted. Requests made *during* a definition check are not queued either
   (the instance is queued when a concrete body first needs it).
4. Instantiation depth is capped (64 nested type arguments); beyond it the compiler
   reports "type instantiation is too deep" instead of looping.

### Driver phases (`analyze.rs`)

```
1. load ASTs, circular-import check                       (loader.rs, unchanged logic)
2. declare: DefId → Def per package; redeclarations reported
3. signatures: global types; every fn's signature under {params ↦ Param}; every struct's identity instance
4. bodies: global initializers; definition-check every generic fn body (discarded; sets body_ok)
5. drain `pending`: check_body(def, args) for every concrete function instance; requests made
   while checking are appended; instances of a def whose definition check failed are skipped
6. struct cycle check over all instances (identity instances first, so definition-level
   messages keep their old wording); global init order over instance bodies
7. build Module: packages in load order, globals/functions in declaration order,
   instances in creation order
```

### Why bug #3 disappears

`struct Node<T> { val: T, next: *Node<T> }` with `fn push<T>(head: *Node<T>, val: T)`:
phase 3 caches the identity `Node<[T]>` and resolves its fields (`*Node<T>` hits the
cache); phase 4 definition-checks `push` against that body; phase 5 checks `push::<i32>`
under `T ↦ i32`, creating `Node<[i32]>` with a complete body on the spot. No cell is read
before it is filled, whatever the declaration order.

Bug #8 (`struct A<T>{x: T}  struct B { a: A<B> }`) is caught in phase 6 because instances
have real bodies: `B → A<B> → B`.

## 4. Contract with `magelang_wasmgen`

Unchanged: `Module`/`Package`/`Global`/`Func` (incl. `Func.typeargs = None` for non-generic),
`Expr`/`ExprKind`, `Statement`, `FuncType`, `StructBody.fields`, `StructType.body`,
`TypeArgs`, `DefId`, `Annotation`, `Type`'s `Hash`/`Eq`, the `is_*` predicates, mangled names.

Edits: `mangling.rs` matches `TypeKind::Named { def_id, type_args }`; the four `unreachable!`
arms on `TypeRepr::TypeArg` become `TypeRepr::Param`; `use magelang_typecheck::` → `use magelang_typecheck2::`.

## 5. Language-visible changes

1. Named structs are nominal (`jauhar.mg`'s `g(x)` with `x: P` into a `Q` parameter is now an error).
2. Generic function instances have structural function types (`id::<i32> == other::<i32>` is no longer a type error).
3. Errors that only exist for one instantiation (`==` on `opaque` through `T`) are diagnostics instead of codegen panics.
4. Polymorphic recursion reports "type instantiation is too deep" instead of hanging.
5. Invalid integer literals get type `Unknown` (BUGS.md #15) and pointer↔float casts are rejected (#13);
   both turn compiler panics into diagnostics.
6. BUGS.md #2, #3, #8, #22 fall out of the design.

## 6. Verification

- `cargo test` for all crates; the five existing `expected_errors` files must stay byte-identical.
- Golden wasm: every example and passing fixture compiled with the old pipeline is byte-compared
  against the new pipeline (output is deterministic; wasmgen sorts functions by mangled name).
- New fixtures: generics round trip and instantiation chains (positive), the linked list and
  forward-declared generic structs (#3), nominal structs (#2), infinite type through a generic
  argument (#8), instantiation depth, and definition-time errors in unused generic code.

## 7. Status (2026-08-27)

Implemented in `magelang_typecheck2/` and **not yet integrated**: `magelang_wasmgen` and
the `magelang` CLI still use `magelang_typecheck`. Outside the crate, the only edits are
the `magelang_typecheck2` entry in the workspace `members` (so the crate can be built and
tested in place), the regenerated `Cargo.lock`, and this document. The crate follows the
workspace's move to edition 2024 / indexmap 2.

| File | Role | Lines |
|---|---|---|
| `lib.rs` | public IR (`Module`, `Package`, `Global`, `Func`, `DefId`) and re-exports | 101 |
| `analyze.rs` | driver: phases 2–7, `Context`, error-counting reporter | 341 |
| `loader.rs` | phase 1: load packages, circular imports | 133 |
| `def.rs` | phase 2: `Definitions`, `StructDef`/`FuncDef`/`GlobalDef`, builtin scope | 279 |
| `scope.rs` | persistent scopes; type/value/import entries; binding type parameters | 121 |
| `ty.rs` | `Type`, `TypeKind::{Named, Anonymous}`, `TypeRepr`, predicates | 372 |
| `resolve.rs` | type expressions, type arguments, signatures, path lookup | 186 |
| `instance.rs` | struct/function instances, worklist, depth limit — all the generics machinery | 208 |
| `func.rs` | `check_body(def, instance)` — one path for definition checks and instantiations | 59 |
| `expr.rs` | expression checking; table-driven binary operators; one untyped-constant coercion | 1220 |
| `statement.rs` | statement checking | 329 |
| `global_init.rs` | global initialization order over instance bodies | 246 |
| `cycle.rs` | infinite-size structs over instances | 117 |
| `errors.rs`, `interner.rs`, `path.rs` | unchanged apart from one new diagnostic | 365 |
| `tests/fixtures.rs` | crate-local harness (below) | 95 |
| **total** | (old crate: 5,449) | **4,172** |

### Verification so far

- `cargo test -p magelang_typecheck2`: 6 fixture tests + 1 unit test pass;
  `cargo clippy -p magelang_typecheck2 --tests`: no findings.
- The fixtures live in `magelang_typecheck2/tests/fixtures/`. `tests/fixtures.rs` runs
  `analyze` on each package (finding `std/*` through `MAGELANG_ROOT`, pointed at
  `../magelang`). A fixture with `expected_errors` must produce exactly those diagnostics;
  one without must check cleanly, and its `expected_instances` file lists every concrete
  instance of a generic function the module ends up with (sorted), which shows the
  instantiation cascade at a glance:
  - `generics_roundtrip` — generic structs/functions, instantiation chain
    `outer<i32> → inner<Wrap<i32>> → wrap<Wrap<i32>>`, generic functions as values,
    `size_of` of instances, generic structs through pointers.
  - `linked_list` — BUGS.md #3 (`Node<T>` + `push<T>`, same parameter name),
    forward-declared generic structs, nested generic structs by value, `Bar<Tree>` inside
    `Tree` through a pointer.
  - `nominal_structs_fail` (BUGS.md #2), `infinite_type_fail` (BUGS.md #8),
    `instantiation_depth_fail` (depth limit), `definition_check_fail` (definition-time
    errors in unused generic code, type-argument count errors, `opaque ==` reported
    from inside `assert_equal<opaque>`).
- During development the crate was also wired into wasmgen and the CLI temporarily: the
  13 CLI integration tests passed with the five pre-existing `expected_errors` files
  byte-identical, and every example and passing fixture compiled to byte-identical `.wasm`
  with the old and the new checker. That wiring was reverted pending review; the steps to
  redo it are in §4 (dependency lines, `use` paths, `mangling.rs`, four `TypeRepr::Param`
  arms). The positive fixtures above can then move to `magelang/tests/` to be executed.

### Follow-ups

- After integration: delete `magelang_typecheck` and rename `magelang_typecheck2`.
- Diagnostics raised while checking an instantiation (`definition_check_fail`, line 35)
  should say which instantiation (`in assert_equal::<opaque>, requested at …`).
- Checked statements are arena-allocated and never dropped (`Box`/`BigInt` inside them
  leak until the process exits); same as before, worth a `Drop`-free IR eventually.
