use magelang_typecheck::{Annotation, Expr, ExprKind, Module, Statement};
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;

#[derive(Default)]
pub(crate) struct Data {
    bytes: HashMap<Rc<[u8]>, u32>,
    files: HashMap<Rc<Path>, u32>,
    next_offset: u32,
    data: Vec<(Rc<[u8]>, u32)>,
}

impl Data {
    pub(crate) fn build<'ctx>(module: &Module<'ctx>) -> Self {
        let mut s = Self::default();

        let globals = module.packages.iter().flat_map(|pkg| &pkg.globals);
        for global in globals.clone() {
            s.init_from_expr(&global.value);
        }
        for global in globals {
            s.init_from_annotations(&global.annotations);
        }

        let functions = module.packages.iter().flat_map(|pkg| &pkg.functions);
        for func in functions {
            s.init_from_stmt(&func.statement);
        }

        s
    }

    fn init_from_expr<'ctx>(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Bytes(buff) => {
                if !self.bytes.contains_key(buff) {
                    let next_offset = self.next_offset;
                    self.next_offset += buff.len() as u32;
                    self.bytes.insert(buff.clone(), next_offset);
                    self.data.push((buff.clone(), next_offset));
                }
            }
            ExprKind::Invalid
            | ExprKind::ConstI8(..)
            | ExprKind::ConstI16(..)
            | ExprKind::ConstI32(..)
            | ExprKind::ConstI64(..)
            | ExprKind::ConstIsize(..)
            | ExprKind::ConstF32(..)
            | ExprKind::ConstF64(..)
            | ExprKind::ConstBool(..)
            | ExprKind::Zero
            | ExprKind::Local(..)
            | ExprKind::Global(..)
            | ExprKind::Func(..)
            | ExprKind::FuncInst(..) => {}
            ExprKind::StructLit(_, values) => {
                for val in values {
                    self.init_from_expr(val);
                }
            }
            ExprKind::GetElement(expr, _)
            | ExprKind::GetElementAddr(expr, _)
            | ExprKind::Neg(expr)
            | ExprKind::BitNot(expr)
            | ExprKind::Not(expr)
            | ExprKind::Cast(expr, _)
            | ExprKind::Deref(expr) => self.init_from_expr(expr),
            ExprKind::GetIndex(expr, index) => {
                self.init_from_expr(expr);
                self.init_from_expr(index);
            }
            ExprKind::Call(callee, args) => {
                self.init_from_expr(callee);
                for arg in args {
                    self.init_from_expr(arg);
                }
            }
            ExprKind::Add(a, b)
            | ExprKind::Sub(a, b)
            | ExprKind::Mul(a, b)
            | ExprKind::Div(a, b)
            | ExprKind::Mod(a, b)
            | ExprKind::BitOr(a, b)
            | ExprKind::BitAnd(a, b)
            | ExprKind::BitXor(a, b)
            | ExprKind::ShiftLeft(a, b)
            | ExprKind::ShiftRight(a, b)
            | ExprKind::And(a, b)
            | ExprKind::Or(a, b)
            | ExprKind::Eq(a, b)
            | ExprKind::NEq(a, b)
            | ExprKind::Gt(a, b)
            | ExprKind::GEq(a, b)
            | ExprKind::Lt(a, b)
            | ExprKind::LEq(a, b) => {
                self.init_from_expr(a);
                self.init_from_expr(b);
            }
        }
    }

    fn init_from_annotations<'ctx>(&mut self, annotations: &[Annotation]) {
        let Some(filepath) = Self::get_embed_file_annotation(annotations) else {
            return;
        };

        let mut f = File::open(filepath).expect("todo: repport error: cannot read file");
        let mut buff = Vec::default();
        f.read_to_end(&mut buff)
            .expect("todo: report error: cannot read file");

        let next_offset = self.next_offset;
        self.next_offset += buff.len() as u32;
        self.files.insert(filepath.into(), next_offset);
        self.data.push((buff.into(), next_offset));
    }

    fn get_embed_file_annotation<'a>(annotations: &'a [Annotation]) -> Option<&'a Path> {
        let mut found = false;
        let mut result = None;
        for annotation in annotations {
            let name = annotation.name.as_str();
            if name != "embed_file" {
                continue;
            }

            if found {
                todo!("todo: report error: found duplicated @embed_file annotation");
            }
            found = true;

            if annotation.arguments.len() != 1 {
                todo!(
                    "expected to have 1 argument in embed_file, but found {}",
                    annotation.arguments.len()
                );
            }

            let filepath = annotation.arguments[0].as_str();
            let filepath = Path::new(filepath);

            if !filepath.exists() {
                todo!("todo: report error: {filepath:?} doesn't exist");
            }
            if !filepath.is_file() {
                todo!("todo: report error: {filepath:?} is not a file");
            }

            result = Some(filepath);
        }

        result
    }

    fn init_from_stmt<'ctx>(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Block(statements) => {
                for stmt in statements {
                    self.init_from_stmt(stmt);
                }
            }
            Statement::NewLocal(expr) => {
                self.init_from_expr(&expr);
            }
            Statement::If(if_stmt) => {
                self.init_from_expr(&if_stmt.cond);
                self.init_from_stmt(&if_stmt.body);
                if let Some(ref else_body) = if_stmt.else_stmt {
                    self.init_from_stmt(else_body);
                }
            }
            Statement::While(while_stmt) => {
                self.init_from_expr(&while_stmt.cond);
                self.init_from_stmt(&while_stmt.body);
            }
            Statement::Return(Some(val)) => {
                self.init_from_expr(val);
            }
            Statement::Expr(expr) => self.init_from_expr(expr),
            Statement::Assign(target, value) => {
                self.init_from_expr(target);
                self.init_from_expr(value);
            }
            Statement::Native | Statement::Return(..) | Statement::Continue | Statement::Break => {}
        }
    }
}
