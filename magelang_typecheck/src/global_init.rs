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
    let dep_list = build_initialization_dependency_list(ctx);

    let mut init_order = Vec::default();
    let mut visited = IndexSet::<DefId>::default();
    let mut in_chain = IndexSet::<DefId>::default();
    for (name, (pos, _)) in &dep_list {
        if !is_global(ctx, *name) {
            continue;
        }
        if visited.contains(name) {
            continue;
        }
        assert!(in_chain.is_empty());

        let mut stack = vec![*name];
        while let Some(name) = stack.pop() {
            if in_chain.contains(&name) {
                in_chain.remove(&name);
                init_order.push(name);
                continue;
            }

            stack.push(name);
            visited.insert(name);
            in_chain.insert(name);

            let empty = &IndexSet::default();
            let dependencies = dep_list
                .get(&name)
                .map(|(_, deps)| deps)
                .unwrap_or(empty)
                .iter();
            for dep in dependencies {
                if !visited.contains(dep) {
                    stack.push(*dep);
                } else if in_chain.contains(dep) && is_global(ctx, *dep) {
                    report_circular_initialization(ctx, &in_chain, *dep, *pos);
                }
            }
        }
    }

    init_order
}

fn is_global<E>(ctx: &Context<'_, '_, E>, def_id: DefId) -> bool {
    ctx.scopes
        .get(def_id.package)
        .and_then(|scope| scope.value_scopes.lookup(def_id.name))
        .map(|value_object| matches!(value_object, ValueObject::Global(..)))
        .unwrap_or_default()
}

fn build_initialization_dependency_list<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
) -> IndexMap<DefId<'a>, (Pos, IndexSet<DefId<'a>>)> {
    let mut adjlist = IndexMap::<DefId, (Pos, IndexSet<DefId>)>::default();
    let global_objects = ctx
        .scopes
        .values()
        .flat_map(|scopes| scopes.value_scopes.iter())
        .map(|(_, obj)| obj);

    for value_object in global_objects {
        enum Item<'a, 'b> {
            Statement(&'a Statement<'a>),
            Expr(&'b Expr<'a>),
        }

        let mut stack = vec![];
        let def_id: DefId;
        let pos: Pos;
        match value_object {
            ValueObject::Func(obj) => {
                stack.push(Item::Statement(
                    obj.body.get().expect("missing function body"),
                ));
                def_id = obj.def_id;
                pos = obj.pos;
            }
            ValueObject::Global(obj) => {
                stack.push(Item::Expr(obj.value.get().expect("missing global expr")));
                def_id = obj.def_id;
                pos = obj.node.pos;
            }
            _ => continue,
        };

        let mut dependencies = IndexSet::<DefId>::default();
        while let Some(item) = stack.pop() {
            match item {
                Item::Statement(stmt) => match stmt {
                    Statement::NewLocal(_, expr) => {
                        stack.push(Item::Expr(expr));
                    }
                    Statement::Block(statements) => {
                        for stmt in statements.iter() {
                            stack.push(Item::Statement(stmt));
                        }
                    }
                    Statement::If(stmt) => {
                        stack.push(Item::Expr(&stmt.cond));
                        stack.push(Item::Statement(&stmt.body));
                        if let Some(else_stmt) = &stmt.else_stmt {
                            stack.push(Item::Statement(else_stmt));
                        }
                    }
                    Statement::While(stmt) => {
                        stack.push(Item::Expr(&stmt.cond));
                        stack.push(Item::Statement(&stmt.body));
                    }
                    Statement::Return(Some(expr)) => {
                        stack.push(Item::Expr(expr));
                    }
                    Statement::Expr(expr) => {
                        stack.push(Item::Expr(expr));
                    }
                    Statement::Assign(receiver, value) => {
                        stack.push(Item::Expr(receiver));
                        stack.push(Item::Expr(value));
                    }
                    _ => (),
                },
                Item::Expr(expr) => match expr.kind {
                    ExprKind::StructLit(_, values) => {
                        for expr in values {
                            stack.push(Item::Expr(expr));
                        }
                    }
                    ExprKind::Global(def_id)
                    | ExprKind::Func(def_id)
                    | ExprKind::FuncInst(def_id, _) => {
                        dependencies.insert(def_id);
                    }
                    ExprKind::GetElement(value, _)
                    | ExprKind::GetElementAddr(value, _)
                    | ExprKind::GetIndex(value, _)
                    | ExprKind::Deref(value)
                    | ExprKind::Neg(value)
                    | ExprKind::BitNot(value)
                    | ExprKind::Not(value)
                    | ExprKind::Cast(value, _) => {
                        stack.push(Item::Expr(value));
                    }
                    ExprKind::Call(callee, args) => {
                        stack.push(Item::Expr(callee));
                        for expr in args {
                            stack.push(Item::Expr(expr));
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
                        stack.push(Item::Expr(a));
                        stack.push(Item::Expr(b));
                    }
                    _ => (),
                },
            }
        }

        adjlist.insert(def_id, (pos, dependencies));
    }

    adjlist
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
