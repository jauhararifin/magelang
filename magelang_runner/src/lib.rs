use wasmtime::{Caller, Engine, Linker, Module, Store};

#[derive(Default)]
pub struct Runner {}

impl Runner {
    pub fn run(&self, module_bin: &[u8]) {
        let engine = Engine::default();
        let module = Module::from_binary(&engine, module_bin).unwrap();
        let mut linker = Linker::new(&engine);
        // wasmtime_wasi::add_to_linker(&mut linker, |cx| *cx).expect("cannot init wasi");
        linker
            .func_wrap("std/fmt", "print_i64", |_: Caller<'_, u32>, n: i64| {
                println!("Got {} from WebAssembly", n);
            })
            .unwrap();

        // let wasi_ctx = WasiCtxBuilder::new().inherit_stdio().build();

        let mut store = Store::new(&engine, 0);
        _ = linker.instantiate(&mut store, &module).unwrap();
    }
}
