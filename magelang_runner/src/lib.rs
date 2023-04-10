use wasmtime::{Caller, Engine, Linker, Module, Store};

#[derive(Default)]
pub struct Runner {}

impl Runner {
    pub fn run(&self, module_bin: &[u8]) {
        let engine = Engine::default();
        let module = Module::from_binary(&engine, module_bin).unwrap();
        let mut linker = Linker::new(&engine);
        linker
            .func_wrap("examples/a", "print_i64", |caller: Caller<'_, u32>, n: i64| {
                println!("Got {} from WebAssembly", n);
                println!("my host state is: {}", caller.data());
            })
            .unwrap();

        let mut store = Store::new(&engine, 0);
        _ = linker.instantiate(&mut store, &module).unwrap();
    }
}
