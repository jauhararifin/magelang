fn equal[T](a: T, b: T): bool { return a == b; }
fn different[T](a: T, b: T): bool { return a != b; }

fn invalid() {
  let eq = equal[(opaque, i32)]((null, 1), (null, 2));
  let ne = different[((opaque,),)](((null,),), ((null,),));
  let pointer: *(opaque, i32);
  let loaded = pointer.*;
  pointer.* = (null, 1);
}

type Hidden struct { value: opaque }
type Wrapped struct { value: (Hidden, i32) }

fn invalid_comparisons() {
  let direct = (null, 1) == (null, 1);
  let nested = (1, (null,)) != (1, (null,));
  let record = Hidden{} == Hidden{};
  let wrapped = (Wrapped{},) == (Wrapped{},);
}
