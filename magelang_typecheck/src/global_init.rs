use crate::analyze::{Context, ValueObject};
use crate::errors::SemanticError;
use crate::expr::{Expr, ExprKind};
use crate::statement::Statement;
use crate::DefId;
use indexmap::{IndexMap, IndexSet};
use magelang_syntax::{ErrorReporter, Pos};

pub(crate) fn check_circular_global_intitialization<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
) -> Vec<DefId<'a>> {
    let dep_list = build_global_initialization_dependency_list(ctx);
    sort_global_initialization_order(ctx, &dep_list)
}

fn build_global_initialization_dependency_list<'a, E>(
    ctx: &Context<'a, '_, E>,
) -> IndexMap<DefId<'a>, (Pos, IndexSet<DefId<'a>>)> {
    let mut adjlist = IndexMap::<DefId, (Pos, IndexSet<DefId>)>::default();

    for value_object in ctx
        .scopes
        .values()
        .flat_map(|scopes| scopes.value_scopes.iter())
        .map(|(_, obj)| obj)
    {
        let ValueObject::Global(global_object) = value_object else {
            continue;
        };

        let dependencies = collect_global_dependencies(
            ctx,
            global_object.value.get().expect("missing global expr"),
        );
        adjlist.insert(global_object.def_id, (global_object.node.pos, dependencies));
    }

    adjlist
}

fn collect_global_dependencies<'a, E>(
    ctx: &Context<'a, '_, E>,
    expr: &Expr<'a>,
) -> IndexSet<DefId<'a>> {
    let mut dependencies = IndexSet::default();
    let mut visited_funcs = IndexSet::default();
    let mut visiting_funcs = IndexSet::default();
    collect_expr_dependencies(
        ctx,
        expr,
        &mut dependencies,
        &mut visited_funcs,
        &mut visiting_funcs,
    );
    dependencies
}

fn collect_func_dependencies<'a, E>(
    ctx: &Context<'a, '_, E>,
    def_id: DefId<'a>,
    dependencies: &mut IndexSet<DefId<'a>>,
    visited_funcs: &mut IndexSet<DefId<'a>>,
    visiting_funcs: &mut IndexSet<DefId<'a>>,
) {
    if visited_funcs.contains(&def_id) || visiting_funcs.contains(&def_id) {
        return;
    }

    let Some(ValueObject::Func(func_object)) = ctx
        .scopes
        .get(def_id.package)
        .and_then(|scope| scope.value_scopes.lookup(def_id.name))
    else {
        return;
    };

    visiting_funcs.insert(def_id);
    collect_statement_dependencies(
        ctx,
        func_object.body.get().expect("missing function body"),
        dependencies,
        visited_funcs,
        visiting_funcs,
    );
    visiting_funcs.remove(&def_id);
    visited_funcs.insert(def_id);
}

fn collect_statement_dependencies<'a, E>(
    ctx: &Context<'a, '_, E>,
    stmt: &Statement<'a>,
    dependencies: &mut IndexSet<DefId<'a>>,
    visited_funcs: &mut IndexSet<DefId<'a>>,
    visiting_funcs: &mut IndexSet<DefId<'a>>,
) {
    match stmt {
        Statement::Native | Statement::Continue | Statement::Break => {}
        Statement::NewLocal { id: _, value } => {
            collect_expr_dependencies(ctx, value, dependencies, visited_funcs, visiting_funcs)
        }
        Statement::Block(statements) => {
            for stmt in statements.iter() {
                collect_statement_dependencies(
                    ctx,
                    stmt,
                    dependencies,
                    visited_funcs,
                    visiting_funcs,
                );
            }
        }
        Statement::If(stmt) => {
            collect_expr_dependencies(ctx, &stmt.cond, dependencies, visited_funcs, visiting_funcs);
            collect_statement_dependencies(
                ctx,
                &stmt.body,
                dependencies,
                visited_funcs,
                visiting_funcs,
            );
            if let Some(else_stmt) = &stmt.else_stmt {
                collect_statement_dependencies(
                    ctx,
                    else_stmt,
                    dependencies,
                    visited_funcs,
                    visiting_funcs,
                );
            }
        }
        Statement::While(stmt) => {
            collect_expr_dependencies(ctx, &stmt.cond, dependencies, visited_funcs, visiting_funcs);
            collect_statement_dependencies(
                ctx,
                &stmt.body,
                dependencies,
                visited_funcs,
                visiting_funcs,
            );
        }
        Statement::Return(Some(expr)) | Statement::Expr(expr) => {
            collect_expr_dependencies(ctx, expr, dependencies, visited_funcs, visiting_funcs)
        }
        Statement::Return(None) => {}
        Statement::Assign(receiver, value) => {
            collect_expr_dependencies(ctx, receiver, dependencies, visited_funcs, visiting_funcs);
            collect_expr_dependencies(ctx, value, dependencies, visited_funcs, visiting_funcs);
        }
    }
}

fn collect_expr_dependencies<'a, E>(
    ctx: &Context<'a, '_, E>,
    expr: &Expr<'a>,
    dependencies: &mut IndexSet<DefId<'a>>,
    visited_funcs: &mut IndexSet<DefId<'a>>,
    visiting_funcs: &mut IndexSet<DefId<'a>>,
) {
    match expr.kind {
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
            dependencies.insert(def_id);
        }
        ExprKind::StructLit(_, values) => {
            for expr in values {
                collect_expr_dependencies(ctx, expr, dependencies, visited_funcs, visiting_funcs);
            }
        }
        ExprKind::GetElement(value, _)
        | ExprKind::GetElementAddr(value, _)
        | ExprKind::Deref(value)
        | ExprKind::Neg(value)
        | ExprKind::BitNot(value)
        | ExprKind::Not(value)
        | ExprKind::Cast(value, _) => {
            collect_expr_dependencies(ctx, value, dependencies, visited_funcs, visiting_funcs)
        }
        ExprKind::GetIndex(value, index) => {
            collect_expr_dependencies(ctx, value, dependencies, visited_funcs, visiting_funcs);
            collect_expr_dependencies(ctx, index, dependencies, visited_funcs, visiting_funcs);
        }
        ExprKind::Call(callee, args) => {
            collect_expr_dependencies(ctx, callee, dependencies, visited_funcs, visiting_funcs);
            for expr in args {
                collect_expr_dependencies(ctx, expr, dependencies, visited_funcs, visiting_funcs);
            }

            match callee.kind {
                ExprKind::Func(def_id) | ExprKind::FuncInst(def_id, _) => {
                    collect_func_dependencies(
                        ctx,
                        def_id,
                        dependencies,
                        visited_funcs,
                        visiting_funcs,
                    );
                }
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
            collect_expr_dependencies(ctx, a, dependencies, visited_funcs, visiting_funcs);
            collect_expr_dependencies(ctx, b, dependencies, visited_funcs, visiting_funcs);
        }
    }
}

fn sort_global_initialization_order<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    dep_list: &IndexMap<DefId<'a>, (Pos, IndexSet<DefId<'a>>)>,
) -> Vec<DefId<'a>> {
    let mut init_order = Vec::default();
    let mut visited = IndexSet::<DefId>::default();
    let mut in_chain = IndexSet::<DefId>::default();

    for name in dep_list.keys() {
        if visited.contains(name) {
            continue;
        }
        visit_global(
            ctx,
            dep_list,
            *name,
            &mut visited,
            &mut in_chain,
            &mut init_order,
        );
    }

    init_order
}

fn visit_global<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    dep_list: &IndexMap<DefId<'a>, (Pos, IndexSet<DefId<'a>>)>,
    name: DefId<'a>,
    visited: &mut IndexSet<DefId<'a>>,
    in_chain: &mut IndexSet<DefId<'a>>,
    init_order: &mut Vec<DefId<'a>>,
) {
    visited.insert(name);
    in_chain.insert(name);

    let Some((_, dependencies)) = dep_list.get(&name) else {
        in_chain.remove(&name);
        return;
    };

    for dep in dependencies.iter() {
        if !dep_list.contains_key(dep) {
            continue;
        }

        if in_chain.contains(dep) {
            let pos = dep_list
                .get(dep)
                .or_else(|| dep_list.get(&name))
                .map(|(pos, _)| *pos)
                .expect("missing global dependency position");
            report_circular_initialization(ctx, in_chain, *dep, pos);
            continue;
        }

        if !visited.contains(dep) {
            visit_global(ctx, dep_list, *dep, visited, in_chain, init_order);
        }
    }

    in_chain.remove(&name);
    init_order.push(name);
}

fn report_circular_initialization<E: ErrorReporter>(
    ctx: &Context<'_, '_, E>,
    in_chain: &IndexSet<DefId>,
    start: DefId,
    pos: Pos,
) {
    let mut chain = Vec::default();
    let mut started = false;
    for name in in_chain {
        if name == &start {
            started = true;
        }
        if started {
            chain.push(*name);
        }
    }

    let mut chain_str = Vec::default();
    for name in chain {
        let display = format!("{name}");
        chain_str.push(display);
    }

    ctx.errors.circular_initialization(pos, &chain_str);
}
