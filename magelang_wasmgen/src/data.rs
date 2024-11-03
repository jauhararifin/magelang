use crate::context::Context;
use crate::errors::CodegenError;
use magelang_syntax::{ErrorReporter, Pos};
use magelang_typecheck::{Annotation, Expr, ExprKind, Statement};
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;
use wasm_helper as wasm;

const EMBED_FILE_ANNOTATION_NAME: &str = "embed_file";

pub(crate) struct DataManager<'ctx, E> {
    ctx: Context<'ctx, E>,

    literals: HashMap<&'ctx [u8], usize>,
    files: HashMap<Rc<Path>, usize>,
    next_offset: usize,
    data: Vec<(&'ctx [u8], usize)>,
}

impl<'ctx, E> DataManager<'ctx, E> {
    fn new(ctx: Context<'ctx, E>) -> Self {
        Self {
            ctx,
            literals: HashMap::default(),
            files: HashMap::default(),
            // next offset starts at 8 because some runtime don't allow address zero to be used.
            next_offset: 8,
            data: Vec::default(),
        }
    }
}

impl<'ctx, E> DataManager<'ctx, E> {
    pub(crate) fn data_end(&self) -> usize {
        self.next_offset
    }

    fn init_from_expr(&mut self, expr: &Expr<'ctx>) {
        match &expr.kind {
            ExprKind::Bytes(buff) => {
                if !self.literals.contains_key(buff) {
                    let next_offset = self.next_offset;
                    self.next_offset += buff.len();
                    self.literals.insert(buff, next_offset);
                    self.data.push((*buff, next_offset));
                }
            }
            ExprKind::Invalid
            | ExprKind::ConstInt(..)
            | ExprKind::ConstI8(..)
            | ExprKind::ConstI16(..)
            | ExprKind::ConstI32(..)
            | ExprKind::ConstI64(..)
            | ExprKind::ConstIsize(..)
            | ExprKind::ConstFloat(..)
            | ExprKind::ConstF32(..)
            | ExprKind::ConstF64(..)
            | ExprKind::ConstBool(..)
            | ExprKind::Zero
            | ExprKind::Local(..)
            | ExprKind::Global(..)
            | ExprKind::Func(..)
            | ExprKind::FuncInst(..) => {}
            ExprKind::StructLit(_, values) => {
                for val in values.iter() {
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
                for arg in args.iter() {
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

    pub(crate) fn get_min_page(&self) -> usize {
        (self.next_offset + wasm_helper::PAGE_SIZE as usize - 1) / (wasm_helper::PAGE_SIZE as usize)
    }

    pub(crate) fn get_bytes(&self, bytes: &[u8]) -> Option<usize> {
        self.literals.get(bytes).cloned()
    }

    pub(crate) fn get_file(&self, path: &Path) -> Option<usize> {
        self.files.get(path).cloned()
    }

    pub(crate) fn take(self) -> Data<'ctx> {
        let num_pages = self.get_min_page();

        let mut datas = Vec::default();
        for (data, offset) in self.data {
            datas.push(wasm::Data {
                init: wasm::Bytes(data),
                mode: wasm::DataMode::Active {
                    memory: 0,
                    offset: wasm::Expr(vec![wasm::Instr::I32Const(offset as i32)]),
                },
            });
        }

        Data { num_pages, datas }
    }
}

impl<'ctx, E: ErrorReporter> DataManager<'ctx, E> {
    pub(crate) fn build(ctx: Context<'ctx, E>) -> Self {
        let mut s = Self::new(ctx);
        s.init();
        s
    }

    fn init(&mut self) {
        let globals = self.ctx.module.packages.iter().flat_map(|pkg| &pkg.globals);
        for global in globals.clone() {
            self.init_from_expr(&global.value);
        }
        for global in globals {
            self.init_from_annotations(&global.annotations);
        }

        let functions = self
            .ctx
            .module
            .packages
            .iter()
            .flat_map(|pkg| &pkg.functions);
        for func in functions {
            self.init_from_stmt(func.statement);
        }
    }

    fn init_from_annotations(&mut self, annotations: &'ctx [Annotation]) {
        let Some((pos, filepath)) = self.get_embed_file_annotation(annotations) else {
            return;
        };

        let buff = self.get_file_contents(pos, filepath);

        let next_offset = self.next_offset;
        self.next_offset += buff.len();
        self.files.insert(filepath.into(), next_offset);
        self.data.push((buff, next_offset));
    }

    fn init_from_stmt(&mut self, stmt: &Statement<'ctx>) {
        match stmt {
            Statement::Block(statements) => {
                for stmt in statements.iter() {
                    self.init_from_stmt(stmt);
                }
            }
            Statement::NewLocal { id: _, value } => {
                self.init_from_expr(value);
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

    fn get_file_contents(&self, pos: Pos, filepath: &Path) -> &'ctx [u8] {
        let mut f = match File::open(filepath) {
            Ok(f) => f,
            Err(err) => {
                self.ctx.errors.cannot_read_file(pos, filepath, err);
                return &[];
            }
        };

        let mut buff = Vec::default();
        if let Err(err) = f.read_to_end(&mut buff) {
            self.ctx.errors.cannot_read_file(pos, filepath, err);
            return &[];
        }

        self.ctx.arena.alloc_slice_copy(&buff)
    }

    pub(crate) fn get_embed_file_annotation(
        &self,
        annotations: &'ctx [Annotation],
    ) -> Option<(Pos, &'ctx Path)> {
        let mut result = None;
        for annotation in annotations.iter().rev() {
            let name = annotation.name.as_str();
            if name != EMBED_FILE_ANNOTATION_NAME {
                continue;
            }

            if annotation.arguments.len() != 1 {
                self.ctx.errors.annotation_arg_mismatch(annotation, 1);
                continue;
            }

            if result.is_some() {
                self.ctx.errors.duplicated_annotation(annotation);
                continue;
            }

            let filepath = annotation.arguments[0].as_str();
            let filepath = Path::new(filepath);
            result = Some((annotation.pos, filepath));
        }

        result
    }
}

pub(crate) struct Data<'ctx> {
    pub(crate) num_pages: usize,
    pub(crate) datas: Vec<wasm::Data<'ctx>>,
}
