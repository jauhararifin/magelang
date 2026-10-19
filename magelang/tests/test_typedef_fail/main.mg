type First i32;
type Second i32;

let mismatch: First = 1 as Second;
let underlying: i32 = 1 as First;

type Point struct { value: i32 }
type PointCopy Point;
let struct_mismatch: Point = PointCopy{value: 42};

type CycleA CycleB;
type CycleB CycleA;

type MixedCycle AliasCycle;
type AliasCycle = MixedCycle;

type Recursive struct {
  nested: struct { value: Recursive },
}
