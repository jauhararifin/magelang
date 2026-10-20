@intrinsic("data.end")
pub fn data_end(): usize;

@intrinsic("size_of")
pub fn size_of[T](): usize;

@intrinsic("align_of")
pub fn align_of[T](): usize;

@intrinsic("memory.size")
pub fn memory_size(): usize;

@intrinsic("memory.grow")
pub fn memory_grow(sz: usize): usize;

@intrinsic("table.get")
pub fn table_get(id: usize): opaque;

@intrinsic("table.set")
pub fn table_set(id: usize, val: opaque);

@intrinsic("unreachable")
pub fn unreachable();
