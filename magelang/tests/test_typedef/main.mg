import wasm "std/wasm";
import dep "tests/test_typedef/dep";

type Count i32;
type ForwardCount LaterCount;
type LaterCount i32;
type CountAlias = Count;
type OtherCount i32;
type Point struct {
  x: i32,
  y: i32,
}
type PointCopy Point;
type Box[T] struct { value: T }
type IntBox Box[i32];
type Shape = struct {
  width: i32,
  height: i32,
};
type Nested struct {
  inner: struct { value: i32 },
}
type Recursive struct {
  next: *struct { owner: Recursive },
}
type Pair (Count, bool);
type Callback fn(value: Count): Count;

fn increment(value: Count): Count {
  return value + 1;
}

@main()
fn main() {
  let external: dep.ExternalCount = 42;
  let external_record = dep.ExternalRecord{value: external};
  let count: Count = 41;
  let forward: ForwardCount = 42;
  let alias: CountAlias = count;
  let raw: i32 = alias as i32;
  let callback: Callback = increment;
  if external_record.value != 42 || callback(count) != 42 || raw != 41 || forward != 42 {
    wasm.unreachable();
  }

  let point: Point = Point{x: 20, y: 22};
  let copy: PointCopy = point as PointCopy;
  let box: IntBox = IntBox{value: copy.x + copy.y};
  if box.value != 42 {
    wasm.unreachable();
  }

  let shape: Shape = Shape{width: 6, height: 7};
  let anonymous: struct { width: i32, height: i32 } = struct { width: i32, height: i32 }{
    width: shape.width,
    height: shape.height,
  };
  if anonymous.width * anonymous.height != 42 {
    wasm.unreachable();
  }

  let nested: Nested = Nested{inner: struct { value: i32 }{value: 42}};
  let pair: Pair = (count, true);
  if nested.inner.value != 42 || pair.0 != 41 || !pair.1 {
    wasm.unreachable();
  }
}
