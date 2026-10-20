struct Direct { value: (i32, Direct) }
struct Indirect { value: (bool, (Other,)) }
struct Other { value: Indirect }
struct Generic[T] { value: (T, Generic[T]) }

var a: (i32,) = (b.0,);
var b: (i32,) = (a.0,);

struct Grouped { value: (Grouped) }
