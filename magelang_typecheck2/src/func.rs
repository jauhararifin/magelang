//! Checking a function body for one instance. The same routine serves the definition
//! check of a generic function (type arguments are its own parameters) and every
//! concrete instantiation.

use crate::analyze::Context;
use crate::def::FuncDef;
use crate::errors::SemanticError;
use crate::instance::FuncInstance;
use crate::scope::ValueEntry;
use crate::statement::{get_statement_from_block, Statement, StatementContext};
use crate::Symbol;
use indexmap::IndexMap;
use magelang_syntax::ErrorReporter;

pub(crate) fn check_body<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    def: &FuncDef<'a>,
    inst: &FuncInstance<'a>,
) -> Statement<'a> {
    let Some(body) = &def.node.body else {
        return Statement::Native;
    };

    let func_type = inst
        .ty
        .as_func()
        .expect("a function instance has a function type");

    let scope = ctx
        .package_scope(def.def_id.package)
        .bind_type_params(def.type_params, inst.type_args);

    // Parameters are the first locals, in declaration order. A duplicated name (already
    // reported) keeps its slot so that local ids stay aligned with the parameters.
    let mut locals = IndexMap::<Symbol<'a>, ValueEntry<'a>>::default();
    for (id, param) in def.node.signature.parameters.iter().enumerate() {
        let name = ctx.define_symbol(&param.name.value);
        locals.entry(name).or_insert(ValueEntry::Local {
            id,
            ty: func_type.params[id],
        });
    }
    let scope = scope.with_locals(locals);

    let return_type = func_type.return_type;
    let stmt_ctx = StatementContext::new(
        ctx,
        &scope,
        def.node.signature.parameters.len(),
        return_type,
    );
    let result = get_statement_from_block(&stmt_ctx, body);

    if !return_type.is_void() && !result.is_returning {
        ctx.errors.missing_return(def.node.pos);
    }

    result.statement
}
