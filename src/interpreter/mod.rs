mod value;
mod dtype;

use self::value::{Value, ValueI8};
use crate::semantic::{BlockStmt, FnDef, Program, Statement, TypeKind};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

struct WalkerInterpreter {
    globals: HashMap<String, Rc<Value>>,
    function_ptr: HashMap<String, Rc<FnDef>>,
}

impl WalkerInterpreter {
    fn new(program: Program) -> Self {
        Self {
            globals: HashMap::new(),
            function_ptr: program
                .functions
                .into_iter()
                .map(|(key, value)| (key, Rc::new(value)))
                .collect(),
        }
    }

    fn init(&mut self) {
        // TODO: fill in the globals.
    }

    fn run(&mut self) {
        let main_func = Rc::clone(
            self.function_ptr
                .get("mail")
                .expect("no main function found"),
        );
        self.run_func(&main_func);
    }

    fn run_func(&mut self, func: &FnDef) -> Value {
        let func_type = if let TypeKind::Fn(func_type) = &func.id.typ.kind {
            func_type
        } else {
            panic!("");
        };

        let mut symbols = self.copy_symbols(&self.globals);
        for param in func_type.params.iter() {
            // TODO: fix, don't use void.
            symbols.insert(param.name.clone(), Rc::new(Value::Void));
        }

        self.run_statement(&func.body, &mut symbols);
        unimplemented!();
    }

    fn run_statement(&self, statement: &Statement, symbols: &mut HashMap<String, Rc<Value>>) {
        match statement {
            Statement::Block(stmt) => unimplemented!(),
            _ => unimplemented!(),
        }
    }

    fn run_block_stmt(&self, statement: &BlockStmt, symbols: &mut HashMap<String, Rc<Value>>) {
        let mut symbols = self.copy_symbols(&symbols);
        for stmt in statement.statements.iter() {
            self.run_statement(&stmt, &mut symbols)
        }
    }

    fn copy_symbols(&self, symbols: &HashMap<String, Rc<Value>>) -> HashMap<String, Rc<Value>> {
        symbols
            .iter()
            .map(|(key, val)| (key.clone(), val.clone()))
            .collect()
    }
}
