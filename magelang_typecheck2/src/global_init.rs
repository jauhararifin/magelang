//! Phase 6: the order in which globals are initialized, and circular initialization.
//!
//! A global depends on every global mentioned by its initializer, directly or through the
//! bodies of the functions it calls (transitively).

use crate::analyze::Context;
use crate::errors::SemanticError;
use crate::expr::{Expr, ExprKind};
use crate::statement::Statement;
use crate::ty::TypeArgs;
use crate::DefId;
use indexmap::{IndexMap, IndexSet};
use magelang_syntax::{ErrorReporter, Pos};

type FuncKey<'a> = (DefId<'a>, &'a TypeArgs<'a>);
type DependencyList<'a> = IndexMap<DefId<'a>, (Pos, IndexSet<DefId<'a>>)>;

pub(crate) fn compute_init_order<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
) -> Vec<DefId<'a>> {
    let dependencies = build_dependency_list(ctx);
    sort_init_order(ctx, &dependencies)
}

fn build_dependency_list<'a, E: ErrorReporter>(ctx: &Context<'a, '_, E>) -> DependencyList<'a> {
    let mut list = DependencyList::default();
    for def in ctx.defs.globals.values() {
        let value = def
            .value
            .get()
            .expect("global values are checked in phase 4");
        let mut collector = Collector {
            ctx,
            empty_args: ctx.define_typeargs(&[]),
            dependencies: IndexSet::default(),
            visited: IndexSet::default(),
            visiting: IndexSet::default(),
        };
        collector.expr(value);
        list.insert(def.def_id, (def.node.pos, collector.dependencies));
    }
    list
}

struct Collector<'c, 'a, 'syn, E> {
    ctx: &'c Context<'a, 'syn, E>,
    empty_args: &'a TypeArgs<'a>,
    dependencies: IndexSet<DefId<'a>>,
    visited: IndexSet<FuncKey<'a>>,
    visiting: IndexSet<FuncKey<'a>>,
}

impl<'c, 'a, 'syn, E: ErrorReporter> Collector<'c, 'a, 'syn, E> {
    fn func(&mut self, key: FuncKey<'a>) {
        if self.visited.contains(&key) || self.visiting.contains(&key) {
            return;
        }
        let Some(body) = self
            .ctx
            .instances
            .get_func(key.0, key.1)
            .and_then(|inst| inst.body.get())
        else {
            return;
        };

        self.visiting.insert(key);
        self.statement(body);
        self.visiting.shift_remove(&key);
        self.visited.insert(key);
    }

    fn statement(&mut self, stmt: &Statement<'a>) {
        match stmt {
            Statement::Native | Statement::Continue | Statement::Break => {}
            Statement::NewLocal { value, .. } => self.expr(value),
            Statement::Block(statements) => {
                for stmt in statements.iter() {
                    self.statement(stmt);
                }
            }
            Statement::If(stmt) => {
                self.expr(&stmt.cond);
                self.statement(&stmt.body);
                if let Some(else_stmt) = &stmt.else_stmt {
                    self.statement(else_stmt);
                }
            }
            Statement::While(stmt) => {
                self.expr(&stmt.cond);
                self.statement(&stmt.body);
            }
            Statement::Return(Some(expr)) | Statement::Expr(expr) => self.expr(expr),
            Statement::Return(None) => {}
            Statement::Assign(receiver, value) => {
                self.expr(receiver);
                self.expr(value);
            }
        }
    }

    fn expr(&mut self, expr: &Expr<'a>) {
        match &expr.kind {
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
            | ExprKind::Bytes(..)
            | ExprKind::Local(..)
            | ExprKind::Func(..)
            | ExprKind::FuncInst(..) => {}
            ExprKind::Global(def_id) => {
                self.dependencies.insert(*def_id);
            }
            ExprKind::StructLit(_, values) => {
                for value in values.iter() {
                    self.expr(value);
                }
            }
            ExprKind::GetElement(value, _)
            | ExprKind::GetElementAddr(value, _)
            | ExprKind::Deref(value)
            | ExprKind::Neg(value)
            | ExprKind::BitNot(value)
            | ExprKind::Not(value)
            | ExprKind::Cast(value, _) => self.expr(value),
            ExprKind::GetIndex(value, index) => {
                self.expr(value);
                self.expr(index);
            }
            ExprKind::Call(callee, args) => {
                self.expr(callee);
                for arg in args.iter() {
                    self.expr(arg);
                }
                match callee.kind {
                    ExprKind::Func(def_id) => self.func((def_id, self.empty_args)),
                    ExprKind::FuncInst(def_id, type_args) => self.func((def_id, type_args)),
                    _ => {}
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
                self.expr(a);
                self.expr(b);
            }
        }
    }
}

fn sort_init_order<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    dependencies: &DependencyList<'a>,
) -> Vec<DefId<'a>> {
    let mut init_order = Vec::default();
    let mut visited = IndexSet::<DefId<'a>>::default();
    let mut in_chain = IndexSet::<DefId<'a>>::default();

    for name in dependencies.keys() {
        if !visited.contains(name) {
            visit_global(
                ctx,
                dependencies,
                *name,
                &mut visited,
                &mut in_chain,
                &mut init_order,
            );
        }
    }

    init_order
}

fn visit_global<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    dependencies: &DependencyList<'a>,
    name: DefId<'a>,
    visited: &mut IndexSet<DefId<'a>>,
    in_chain: &mut IndexSet<DefId<'a>>,
    init_order: &mut Vec<DefId<'a>>,
) {
    visited.insert(name);
    in_chain.insert(name);

    let Some((_, deps)) = dependencies.get(&name) else {
        in_chain.shift_remove(&name);
        return;
    };

    for dep in deps.iter() {
        let Some((pos, _)) = dependencies.get(dep) else {
            continue;
        };

        if in_chain.contains(dep) {
            report_circular_initialization(ctx, in_chain, *dep, *pos);
            continue;
        }

        if !visited.contains(dep) {
            visit_global(ctx, dependencies, *dep, visited, in_chain, init_order);
        }
    }

    in_chain.shift_remove(&name);
    init_order.push(name);
}

fn report_circular_initialization<E: ErrorReporter>(
    ctx: &Context<'_, '_, E>,
    in_chain: &IndexSet<DefId>,
    start: DefId,
    pos: Pos,
) {
    let chain = in_chain
        .iter()
        .skip_while(|name| **name != start)
        .map(|name| name.to_string())
        .collect::<Vec<_>>();
    ctx.errors.circular_initialization(pos, &chain);
}
