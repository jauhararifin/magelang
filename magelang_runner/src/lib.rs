use wasmtime::*;
use wasmtime_wasi::sync::WasiCtxBuilder;

pub fn run(module: &[u8]) {
    let engine = Engine::default();
    let mut linker = Linker::new(&engine);
    wasmtime_wasi::add_to_linker(&mut linker, |s| s).expect("cannot add wasi to linker");

    let wasi = WasiCtxBuilder::new()
        .inherit_stdio()
        .inherit_env()
        .expect("cannot build wasi context")
        .inherit_args()
        .expect("cannot build wasi context")
        .build();
    let mut store = Store::new(&engine, wasi);

    let module = Module::new(&engine, module).expect("cannot load module");
    linker
        .module(&mut store, "some_module_name", &module)
        .expect("cannot set module");
}
