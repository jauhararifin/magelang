struct Direct { value: (i32, Direct) }
struct Indirect { value: (bool, (Other,)) }
struct Other { value: Indirect }
struct Generic[T] { value: (T, Generic[T]) }

let a: (i32,) = (b.0,);
let b: (i32,) = (a.0,);

struct Grouped { value: (Grouped) }
