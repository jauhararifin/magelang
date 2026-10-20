fn equal[T](a: T, b: T): bool { return a == b; }
fn different[T](a: T, b: T): bool { return a != b; }

fn invalid() {
  var eq = equal[(opaque, i32)]((null, 1), (null, 2));
  var ne = different[((opaque,),)](((null,),), ((null,),));
  var pointer: *(opaque, i32);
  var loaded = pointer.*;
  pointer.* = (null, 1);
}

struct Hidden { value: opaque }
struct Wrapped { value: (Hidden, i32) }

fn invalid_comparisons() {
  var direct = (null, 1) == (null, 1);
  var nested = (1, (null,)) != (1, (null,));
  var record = Hidden{} == Hidden{};
  var wrapped = (Wrapped{},) == (Wrapped{},);
}
