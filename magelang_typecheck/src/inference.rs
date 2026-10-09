use crate::analyze::Context;
use crate::ty::{BitSize, FloatType, Type, TypeArgs, TypeKind, TypeRepr};
use magelang_syntax::Pos;
use std::collections::VecDeque;
use std::iter::zip;

#[derive(Clone, Copy)]
pub(crate) enum InferTerm<'a> {
    Variable(usize),
    Rigid(&'a Type<'a>),
    // Only the target signature's type parameters are variables. Argument types,
    // including an enclosing function's type parameters, remain rigid.
    Parameter(&'a Type<'a>),
}

#[derive(Clone, Copy)]
pub(crate) enum ConstraintKind<'a> {
    Assignable { target: InferTerm<'a>, source: &'a Type<'a> },
    Identical { a: InferTerm<'a>, b: &'a Type<'a> },
}

#[derive(Clone, Copy)]
pub(crate) struct Constraint<'a> {
    pub kind: ConstraintKind<'a>,
    pub pos: Pos,
}

pub(crate) enum InferenceError<'a> {
    Underconstrained(Vec<usize>),
    Conflict { pos: Pos, expected: &'a Type<'a>, actual: &'a Type<'a> },
}

pub(crate) fn solve<'a>(
    ctx: &Context<'a, '_>,
    parameter_count: usize,
    constraints: Vec<Constraint<'a>>,
) -> Result<&'a TypeArgs<'a>, InferenceError<'a>> {
    let mut bindings = vec![None; parameter_count];
    let mut defaults = vec![None; parameter_count];
    let mut literals = Vec::new();
    let mut worklist = VecDeque::from(constraints);

    while let Some(mut constraint) = worklist.pop_front() {
        let pos = constraint.pos;
        let is_assignable = matches!(constraint.kind, ConstraintKind::Assignable { .. });
        let (expected, actual) = match &mut constraint.kind {
            ConstraintKind::Assignable { target, source } => (target, *source),
            ConstraintKind::Identical { a, b } => (a, *b),
        };
        if actual.is_unknown() {
            continue;
        }

        match *expected {
            InferTerm::Variable(index) => {
                if is_assignable && matches!(actual.repr, TypeRepr::UntypedInt | TypeRepr::UntypedFloat) {
                    // Literal defaults must not override concrete evidence from other constraints.
                    if defaults[index].is_none() || matches!(actual.repr, TypeRepr::UntypedFloat) {
                        defaults[index] = Some(actual);
                    }
                    literals.push(constraint);
                } else if let Some(binding) = bindings[index] {
                    *expected = InferTerm::Rigid(binding);
                    worklist.push_front(constraint);
                } else {
                    bindings[index] = Some(actual);
                }
            }
            InferTerm::Parameter(parameter) => {
                if let TypeRepr::TypeArg(type_param) = parameter.repr {
                    *expected = InferTerm::Variable(type_param.index);
                    worklist.push_front(constraint);
                    continue;
                }

                if let (TypeKind::Inst(parameter), TypeKind::Inst(argument)) = (&parameter.kind, &actual.kind)
                    && parameter.def_id == argument.def_id
                    && parameter.type_args.len() == argument.type_args.len()
                {
                    for (parameter, argument) in zip(parameter.type_args, argument.type_args) {
                        worklist.push_back(Constraint {
                            kind: ConstraintKind::Identical { a: InferTerm::Parameter(parameter), b: argument },
                            pos,
                        });
                    }
                    continue;
                }

                match (&parameter.repr, &actual.repr) {
                    (TypeRepr::Ptr(parameter), TypeRepr::Ptr(argument))
                    | (TypeRepr::ArrayPtr(parameter), TypeRepr::ArrayPtr(argument)) => {
                        worklist.push_back(Constraint {
                            kind: ConstraintKind::Identical { a: InferTerm::Parameter(parameter), b: argument },
                            pos,
                        });
                    }
                    (TypeRepr::Func(parameter_func), TypeRepr::Func(argument_func))
                        if parameter_func.params.len() == argument_func.params.len()
                            && (is_assignable
                                || (matches!(parameter.kind, TypeKind::Anonymous)
                                    && matches!(actual.kind, TypeKind::Anonymous))) =>
                    {
                        for (parameter, argument) in zip(parameter_func.params, argument_func.params)
                            .chain(std::iter::once((&parameter_func.return_type, &argument_func.return_type)))
                        {
                            worklist.push_back(Constraint {
                                kind: ConstraintKind::Identical { a: InferTerm::Parameter(parameter), b: argument },
                                pos,
                            });
                        }
                    }
                    _ => {
                        *expected = InferTerm::Rigid(parameter);
                        worklist.push_front(constraint);
                    }
                }
            }
            InferTerm::Rigid(expected) => {
                let satisfied = expected.is_unknown()
                    || if is_assignable {
                        expected.is_assignable_with(actual)
                            || (matches!(actual.repr, TypeRepr::UntypedInt | TypeRepr::UntypedFloat)
                                && expected.is_arithmetic())
                    } else {
                        expected == actual
                    };
                if !satisfied {
                    return Err(InferenceError::Conflict { pos, expected, actual });
                }
            }
        }
    }

    for (binding, default) in zip(&mut bindings, defaults) {
        if binding.is_none()
            && let Some(default) = default
        {
            let repr = match default.repr {
                TypeRepr::UntypedInt => TypeRepr::Int(true, BitSize::ISize),
                TypeRepr::UntypedFloat => TypeRepr::Float(FloatType::F64),
                _ => unreachable!("only untyped literals provide defaults"),
            };
            *binding = Some(ctx.define_type(Type { kind: TypeKind::Anonymous, repr }));
        }
    }

    for constraint in literals {
        let ConstraintKind::Assignable { target: InferTerm::Variable(index), source } = constraint.kind else {
            unreachable!("only assignable literal constraints are deferred")
        };
        let expected = bindings[index].expect("literal constraint provides a default");
        if !expected.is_arithmetic() {
            return Err(InferenceError::Conflict { pos: constraint.pos, expected, actual: source });
        }
    }

    let unresolved = bindings.iter().enumerate().filter_map(|(i, ty)| ty.is_none().then_some(i)).collect::<Vec<_>>();
    if !unresolved.is_empty() {
        return Err(InferenceError::Underconstrained(unresolved));
    }
    let type_args = bindings.into_iter().map(Option::unwrap).collect::<Vec<_>>();
    Ok(ctx.define_typeargs(&type_args))
}
