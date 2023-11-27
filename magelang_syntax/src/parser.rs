use crate::ast::*;
use crate::error::ErrorReporter;
use crate::scanner::scan;
use crate::token::{File, Pos, Token, TokenKind};
use std::collections::VecDeque;
use std::fmt::Display;

pub fn parse(errors: &impl ErrorReporter, file: &File) -> PackageNode {
    let scan_result = scan(errors, file);

    let mut comments = Vec::default();
    let mut filtered_tokens = VecDeque::default();
    for tok in scan_result.into_iter() {
        if tok.kind == TokenKind::Comment {
            comments.push(tok);
        } else {
            filtered_tokens.push_back(tok);
        }
    }

    let last_pos = filtered_tokens
        .back()
        .map(|tok| tok.pos)
        .unwrap_or(file.offset);
    let mut parser = FileParser::new(errors, filtered_tokens, last_pos);

    let items = parse_root(&mut parser);
    PackageNode { items, comments }
}

struct FileParser<'a, Error> {
    errors: &'a Error,
    tokens: VecDeque<Token>,
    last_pos: Pos,
}

fn parse_root<E: ErrorReporter>(f: &mut FileParser<E>) -> Vec<ItemNode> {
    let mut items = Vec::<ItemNode>::default();

    while !f.is_empty() {
        if let Some(item) = parse_item_node(f) {
            items.push(item);
        }
    }

    items
}

fn parse_item_node<E: ErrorReporter>(f: &mut FileParser<E>) -> Option<ItemNode> {
    let annotations = parse_annotations(f);

    let tok = f.token();
    let item = match &tok.kind {
        TokenKind::Import => parse_import(f, annotations).map(ItemNode::Import),
        TokenKind::Struct => parse_struct(f, annotations).map(ItemNode::Struct),
        TokenKind::Let => parse_global(f, annotations).map(ItemNode::Global),
        TokenKind::Fn => parse_func(f, annotations).map(ItemNode::Function),
        _ => {
            if let Some(annotation) = annotations.last() {
                f.errors.dangling_annotations(annotation.pos);
            }
            None
        }
    };

    if item.is_none() {
        let stopping_token = &[
            TokenKind::Let,
            TokenKind::Fn,
            TokenKind::Import,
            TokenKind::Struct,
            TokenKind::SemiColon,
        ];
        f.skip_until_before(stopping_token);
        f.take_if(TokenKind::SemiColon);
    }

    item
}

fn parse_annotations<E: ErrorReporter>(f: &mut FileParser<E>) -> Vec<AnnotationNode> {
    let mut result = Vec::default();

    while let Some(at_sign) = f.take_if(TokenKind::AtSign) {
        let pos = at_sign.pos;
        let name = f.take_if(TokenKind::Ident);
        if name.is_none() {
            f.unexpected("annotation identifier");
        }

        let args = parse_sequence(
            f,
            TokenKind::OpenBrac,
            TokenKind::Comma,
            TokenKind::CloseBrac,
            |this| this.take_if(TokenKind::StringLit),
        );
        if args.is_none() {
            f.unexpected("annotation arguments");
        }

        let Some(name) = name else { continue };
        let Some((_, arguments, _)) = args else {
            continue;
        };
        result.push(AnnotationNode {
            pos,
            name,
            arguments,
        });
    }

    result
}

fn parse_sequence<T, F, E: ErrorReporter>(
    f: &mut FileParser<E>,
    begin_tok: TokenKind,
    delim_tok: TokenKind,
    end_tok: TokenKind,
    parse_fn: F,
) -> Option<(Token, Vec<T>, Token)>
where
    F: Fn(&mut FileParser<E>) -> Option<T>,
{
    let opening = f.take_if(begin_tok)?;

    let mut items = Vec::<T>::default();
    while f.kind() != end_tok && f.kind() != TokenKind::Eof {
        if let Some(item) = parse_fn(f) {
            items.push(item);
            f.take_if(delim_tok);
        } else {
            f.take_if(delim_tok);
            break;
        }
    }

    let Some(closing) = f.take_if(end_tok) else {
        f.errors.missing(opening.pos, format!("closing {end_tok}"));
        return None;
    };

    Some((opening, items, closing))
}

fn parse_import<E: ErrorReporter>(
    f: &mut FileParser<E>,
    annotations: Vec<AnnotationNode>,
) -> Option<ImportNode> {
    let import_tok = f.take_if(TokenKind::Import)?;
    let pos = import_tok.pos;
    let name = f.take(TokenKind::Ident)?;
    let path = f.take(TokenKind::StringLit)?;
    f.take(TokenKind::SemiColon)?;
    Some(ImportNode {
        pos,
        annotations,
        name,
        path,
    })
}

fn parse_global<E: ErrorReporter>(
    f: &mut FileParser<E>,
    annotations: Vec<AnnotationNode>,
) -> Option<GlobalNode> {
    let let_tok = f.take_if(TokenKind::Let)?;
    let pos = let_tok.pos;

    let name = f.take(TokenKind::Ident)?;
    f.take(TokenKind::Colon)?;
    let ty = parse_type_expr(f)?;
    let value = if f.take_if(TokenKind::Equal).is_some() {
        parse_expr(f, true)
    } else {
        None
    };
    f.take(TokenKind::SemiColon)?;

    Some(GlobalNode {
        pos,
        annotations,
        name,
        ty,
        value,
    })
}

fn parse_type_expr<E: ErrorReporter>(f: &mut FileParser<E>) -> Option<TypeExprNode> {
    let tok = f.token();
    match &tok.kind {
        TokenKind::OpenSquare => {
            let tok = f.pop();
            f.take(TokenKind::Mul)?;
            let close_tok = f.take(TokenKind::CloseSquare)?;
            let ty = if let Some(ty) = parse_type_expr(f) {
                ty
            } else {
                f.errors.missing(close_tok.pos, "pointee type");
                TypeExprNode::Invalid(tok.pos)
            };
            Some(TypeExprNode::ArrayPtr(ArrayPtrTypeNode {
                pos: tok.pos,
                ty: Box::new(ty),
            }))
        }
        TokenKind::Mul => {
            let tok = f.pop();
            let ty = if let Some(ty) = parse_type_expr(f) {
                ty
            } else {
                f.errors.missing(tok.pos, "pointee type");
                TypeExprNode::Invalid(tok.pos)
            };
            Some(TypeExprNode::Ptr(PtrTypeNode {
                pos: tok.pos,
                ty: Box::new(ty),
            }))
        }
        TokenKind::Fn => {
            let tok = f.pop();

            // TODO: consider adding optional param name.
            let param_result = parse_sequence(
                f,
                TokenKind::OpenBrac,
                TokenKind::Comma,
                TokenKind::CloseBrac,
                parse_type_expr,
            );
            if param_result.is_none() {
                f.errors.missing(tok.pos, "function parameter list");
            }

            let return_type = if let Some(colon_tok) = f.take_if(TokenKind::Colon) {
                if let Some(expr) = parse_type_expr(f) {
                    Some(expr)
                } else {
                    f.errors.missing(colon_tok.pos, "return type");
                    None
                }
            } else {
                None
            };

            let end_pos = return_type
                .as_ref()
                .map(|expr| expr.pos())
                .or(param_result.as_ref().map(|(_, _, close_tok)| close_tok.pos))
                .unwrap_or(tok.pos);

            let parameters = param_result
                .map(|(_, params, _)| params)
                .unwrap_or_default();

            Some(TypeExprNode::Func(FuncTypeNode {
                pos: tok.pos,
                params: parameters,
                return_type: return_type.map(Box::new),
                end_pos,
            }))
        }
        TokenKind::OpenBrac => {
            f.pop();
            let inner_ty = parse_type_expr(f);
            f.take(TokenKind::CloseBrac);
            inner_ty.map(Box::new).map(TypeExprNode::Grouped)
        }
        TokenKind::Ident => {
            let pos = tok.pos;
            if let Some(path) = parse_path_for_type(f) {
                Some(TypeExprNode::Path(path))
            } else {
                Some(TypeExprNode::Invalid(pos))
            }
        }
        _ => None,
    }
}

fn parse_path_for_type<E: ErrorReporter>(f: &mut FileParser<E>) -> Option<PathNode> {
    let ident = f.take_if(TokenKind::Ident)?;

    let mut names = vec![ident];
    while f.take_if(TokenKind::DoubleColon).is_some() {
        let ident = f.take(TokenKind::Ident)?;
        names.push(ident);
    }

    let args = if f.kind() == TokenKind::Lt {
        parse_sequence(
            f,
            TokenKind::Lt,
            TokenKind::Comma,
            TokenKind::Gt,
            parse_type_expr,
        )
        .map(|(_, type_args, _)| type_args)?
    } else {
        Vec::default()
    };

    Some(PathNode { names, args })
}

fn parse_struct<E: ErrorReporter>(
    f: &mut FileParser<E>,
    annotations: Vec<AnnotationNode>,
) -> Option<StructNode> {
    let struct_tok = f.take_if(TokenKind::Struct)?;
    let pos = struct_tok.pos;

    let name = f.take(TokenKind::Ident);

    let type_params = if f.kind() == TokenKind::Lt {
        let result = parse_sequence(
            f,
            TokenKind::Lt,
            TokenKind::Comma,
            TokenKind::Gt,
            |parser| parser.take(TokenKind::Ident),
        );
        result.map(|(_, type_params, _)| {
            type_params
                .into_iter()
                .map(TypeParameterNode::from)
                .collect()
        })
    } else {
        None
    };

    let fields = parse_sequence(
        f,
        TokenKind::OpenBlock,
        TokenKind::Comma,
        TokenKind::CloseBlock,
        |parser| {
            let name = parser.take(TokenKind::Ident)?;
            parser.take(TokenKind::Colon);
            let ty = parse_type_expr(parser)?;
            Some(StructFieldNode {
                pos: name.pos,
                name,
                ty,
            })
        },
    );
    if fields.is_none() {
        f.errors
            .unexpected_parsing(f.token().pos, "struct body", f.token().kind);
    }
    let fields = fields.map(|(_, fields, _)| fields).unwrap_or_default();

    Some(StructNode {
        pos,
        annotations,
        name: name?,
        type_params: type_params.unwrap_or_default(),
        fields,
    })
}

fn parse_func<E: ErrorReporter>(
    f: &mut FileParser<E>,
    annotations: Vec<AnnotationNode>,
) -> Option<FunctionNode> {
    let signature = parse_signature(f, annotations)?;
    let pos = signature.pos;
    if f.take_if(TokenKind::SemiColon).is_some() {
        return Some(FunctionNode {
            pos,
            signature,
            body: None,
        });
    }

    let Some(body) = parse_block_stmt(f) else {
        f.errors.missing(signature.end_pos, "function body");
        return None;
    };
    Some(FunctionNode {
        pos,
        signature,
        body: Some(body),
    })
}

fn parse_signature<E: ErrorReporter>(
    f: &mut FileParser<E>,
    annotations: Vec<AnnotationNode>,
) -> Option<SignatureNode> {
    let func = f.take(TokenKind::Fn)?;
    let pos = func.pos;
    let name = f.take(TokenKind::Ident)?;

    let type_params = if f.kind() == TokenKind::Lt {
        let (_, type_parameters, _) = parse_sequence(
            f,
            TokenKind::Lt,
            TokenKind::Comma,
            TokenKind::Gt,
            |parser| parser.take(TokenKind::Ident),
        )?;
        type_parameters
            .into_iter()
            .map(TypeParameterNode::from)
            .collect()
    } else {
        Vec::default()
    };

    let param_result = parse_sequence(
        f,
        TokenKind::OpenBrac,
        TokenKind::Comma,
        TokenKind::CloseBrac,
        parse_parameter,
    );
    if param_result.is_none() {
        f.errors.missing(name.pos, "function parameter list");
    }

    let return_type = if let Some(colon_tok) = f.take_if(TokenKind::Colon) {
        if let Some(expr) = parse_type_expr(f) {
            Some(expr)
        } else {
            f.errors.missing(colon_tok.pos, "return type");
            None
        }
    } else {
        None
    };

    let end_pos = return_type
        .as_ref()
        .map(|expr| expr.pos())
        .or(param_result.as_ref().map(|(_, _, close_tok)| close_tok.pos))
        .unwrap_or(name.pos);

    let parameters = param_result
        .map(|(_, params, _)| params)
        .unwrap_or_default();

    Some(SignatureNode {
        pos,
        annotations,
        name,
        type_params,
        parameters,
        return_type,
        end_pos,
    })
}

fn parse_parameter<E: ErrorReporter>(f: &mut FileParser<E>) -> Option<ParameterNode> {
    let name = f.take_if(TokenKind::Ident)?;
    let pos = name.pos;
    f.take(TokenKind::Colon);
    let ty = parse_type_expr(f)?;
    Some(ParameterNode { pos, name, ty })
}

fn parse_stmt<E: ErrorReporter>(f: &mut FileParser<E>) -> Option<StatementNode> {
    Some(match f.kind() {
        TokenKind::Let => StatementNode::Let(parse_let_stmt(f)?),
        TokenKind::If => StatementNode::If(parse_if_stmt(f)?),
        TokenKind::While => StatementNode::While(parse_while_stmt(f)?),
        TokenKind::OpenBlock => StatementNode::Block(parse_block_stmt(f)?),
        TokenKind::Continue => StatementNode::Continue(f.take(TokenKind::Continue).unwrap()),
        TokenKind::Break => StatementNode::Break(f.take(TokenKind::Break).unwrap()),
        TokenKind::Return => StatementNode::Return(parse_return_stmt(f)?),
        _ => {
            let expr = parse_expr(f, true)?;
            let pos = expr.pos();
            if f.take_if(TokenKind::Equal).is_some() {
                let value = parse_expr(f, true)?;
                StatementNode::Assign(AssignStatementNode {
                    pos,
                    receiver: expr,
                    value,
                })
            } else {
                StatementNode::Expr(expr)
            }
        }
    })
}

fn parse_let_stmt<E: ErrorReporter>(f: &mut FileParser<E>) -> Option<LetStatementNode> {
    let let_tok = f.take(TokenKind::Let)?;
    let pos = let_tok.pos;

    let name = f.take(TokenKind::Ident)?;

    if f.take_if(TokenKind::Colon).is_some() {
        let ty = parse_type_expr(f)?;
        if f.take_if(TokenKind::Equal).is_some() {
            let value = parse_expr(f, true)?;
            f.take(TokenKind::SemiColon)?;
            Some(LetStatementNode {
                pos,
                name,
                kind: LetKind::TypeValue { ty, value },
            })
        } else {
            f.take(TokenKind::SemiColon)?;
            Some(LetStatementNode {
                pos,
                name,
                kind: LetKind::TypeOnly { ty },
            })
        }
    } else {
        f.take(TokenKind::Equal)?;
        let value = parse_expr(f, true)?;
        f.take(TokenKind::SemiColon)?;
        Some(LetStatementNode {
            pos,
            name,
            kind: LetKind::ValueOnly { value },
        })
    }
}

fn parse_if_stmt<E: ErrorReporter>(f: &mut FileParser<E>) -> Option<IfStatementNode> {
    let if_tok = f.take(TokenKind::If)?;
    let pos = if_tok.pos;

    let Some(condition) = parse_expr(f, false) else {
        f.errors.missing(if_tok.pos, "if condition");
        return None;
    };

    let Some(body) = parse_block_stmt(f) else {
        f.errors.missing(if_tok.pos, "if body");
        return None;
    };

    let else_node = f.take_if(TokenKind::Else).and_then(|_| {
        let stmt = if f.kind() == TokenKind::If {
            let else_if = parse_if_stmt(f)?;
            StatementNode::If(else_if)
        } else {
            let else_body = parse_block_stmt(f)?;
            StatementNode::Block(else_body)
        };
        Some(Box::new(stmt))
    });

    Some(IfStatementNode {
        pos,
        condition,
        body,
        else_node,
    })
}

fn parse_while_stmt<E: ErrorReporter>(f: &mut FileParser<E>) -> Option<WhileStatementNode> {
    let while_tok = f.take(TokenKind::While)?;
    let pos = while_tok.pos;

    let Some(condition) = parse_expr(f, false) else {
        f.errors.missing(while_tok.pos, "while condition");
        return None;
    };

    let Some(body) = parse_block_stmt(f) else {
        f.errors.missing(while_tok.pos, "while body");
        return None;
    };

    Some(WhileStatementNode {
        pos,
        condition,
        body,
    })
}

fn parse_block_stmt<E: ErrorReporter>(f: &mut FileParser<E>) -> Option<BlockStatementNode> {
    let open = f.take_if(TokenKind::OpenBlock)?;
    let pos = open.pos;
    let mut statements = vec![];
    loop {
        let tok = f.token();
        if tok.kind == TokenKind::Eof || tok.kind == TokenKind::CloseBlock {
            break;
        }

        if let Some(stmt) = parse_stmt(f) {
            statements.push(stmt);
        } else {
            f.skip_until_before(&[TokenKind::SemiColon]);
            f.tokens.pop_front();
        }
    }
    f.take(TokenKind::CloseBlock);
    Some(BlockStatementNode { pos, statements })
}

fn parse_return_stmt<E: ErrorReporter>(f: &mut FileParser<E>) -> Option<ReturnStatementNode> {
    let return_tok = f.take(TokenKind::Return)?;
    let pos = return_tok.pos;
    if f.take_if(TokenKind::SemiColon).is_some() {
        return Some(ReturnStatementNode { pos, value: None });
    }

    let Some(value) = parse_expr(f, true) else {
        f.errors
            .unexpected_parsing(pos, "type expression", "nothing");
        return None;
    };

    f.take(TokenKind::SemiColon)?;
    Some(ReturnStatementNode {
        pos,
        value: Some(value),
    })
}

fn parse_expr<E: ErrorReporter>(f: &mut FileParser<E>, allow_struct_lit: bool) -> Option<ExprNode> {
    parse_binary_expr(f, &[TokenKind::Or], allow_struct_lit)
}

const BINOP_PRECEDENCE: &[&[TokenKind]] = &[
    &[TokenKind::Or],
    &[TokenKind::And],
    &[TokenKind::BitOr],
    &[TokenKind::BitXor],
    &[TokenKind::BitAnd],
    &[TokenKind::Eq, TokenKind::NEq],
    &[TokenKind::Lt, TokenKind::LEq, TokenKind::Gt, TokenKind::GEq],
    &[TokenKind::ShiftLeft, TokenKind::ShiftRight],
    &[TokenKind::Add, TokenKind::Sub],
    &[TokenKind::Mul, TokenKind::Div, TokenKind::Mod],
];

fn parse_binary_expr<E: ErrorReporter>(
    f: &mut FileParser<E>,
    op: &[TokenKind],
    allow_struct_lit: bool,
) -> Option<ExprNode> {
    let next_op = BINOP_PRECEDENCE.iter().skip_while(|p| *p != &op).nth(1);

    let a = if let Some(next_op) = next_op {
        parse_binary_expr(f, next_op, allow_struct_lit)?
    } else {
        parse_cast_expr(f, allow_struct_lit)?
    };

    let mut result = a;
    while op.contains(&f.kind()) {
        let op_token = f.pop();
        let b = if let Some(next_op) = next_op {
            parse_binary_expr(f, next_op, allow_struct_lit)?
        } else {
            parse_cast_expr(f, allow_struct_lit)?
        };
        result = ExprNode::Binary(BinaryExprNode {
            a: Box::new(result),
            op: op_token,
            b: Box::new(b),
        });
    }

    Some(result)
}

fn parse_cast_expr<E: ErrorReporter>(
    f: &mut FileParser<E>,
    allow_struct_lit: bool,
) -> Option<ExprNode> {
    let value = parse_unary_expr(f, allow_struct_lit)?;
    if f.take_if(TokenKind::As).is_some() {
        let target = match parse_type_expr(f) {
            Some(t) => t,
            None => {
                let pos = f.token().pos;
                f.skip_until_before(&[TokenKind::SemiColon]);
                f.errors.missing(pos, "target type");
                TypeExprNode::Invalid(pos)
            }
        };

        Some(ExprNode::Cast(CastExprNode {
            value: Box::new(value),
            target: Box::new(target),
        }))
    } else {
        Some(value)
    }
}

const UNARY_OP: &[TokenKind] = &[
    TokenKind::BitNot,
    TokenKind::Sub,
    TokenKind::Add,
    TokenKind::Not,
];

fn parse_unary_expr<E: ErrorReporter>(
    f: &mut FileParser<E>,
    allow_struct_lit: bool,
) -> Option<ExprNode> {
    let mut ops = vec![];
    while UNARY_OP.contains(&f.kind()) {
        let op = f.tokens.pop_front().unwrap();
        ops.push(op);
    }

    let mut value = parse_sequence_of_expr(f, allow_struct_lit)?;
    while let Some(op) = ops.pop() {
        value = ExprNode::Unary(UnaryExprNode {
            op,
            value: Box::new(value),
        })
    }

    Some(value)
}

fn parse_sequence_of_expr<E: ErrorReporter>(
    f: &mut FileParser<E>,
    allow_struct_lit: bool,
) -> Option<ExprNode> {
    let mut target = parse_primary_expr(f)?;
    let mut pos = target.pos();

    loop {
        let kind = f.kind();
        target = match kind {
            TokenKind::Dot => {
                f.take(TokenKind::Dot)?;
                match f.kind() {
                    TokenKind::Ident => {
                        let selection = f.take(TokenKind::Ident)?;
                        ExprNode::Selection(SelectionExprNode {
                            value: Box::new(target),
                            selection,
                        })
                    }
                    TokenKind::Mul => {
                        let tok = f.take(TokenKind::Mul)?;
                        ExprNode::Deref(DerefExprNode {
                            pos: tok.pos,
                            value: Box::new(target),
                        })
                    }
                    _ => break,
                }
            }
            TokenKind::OpenSquare => {
                f.take(TokenKind::OpenSquare)?;
                let index = parse_expr(f, true)?;
                f.take(TokenKind::CloseSquare)?;
                ExprNode::Index(IndexExprNode {
                    value: Box::new(target),
                    index: Box::new(index),
                })
            }
            TokenKind::OpenBrac => {
                let (_, arguments, _) = parse_sequence(
                    f,
                    TokenKind::OpenBrac,
                    TokenKind::Comma,
                    TokenKind::CloseBrac,
                    |this| parse_expr(this, true),
                )?;
                ExprNode::Call(CallExprNode {
                    pos,
                    callee: Box::new(target),
                    arguments,
                })
            }
            TokenKind::OpenBlock => {
                if !allow_struct_lit {
                    break;
                }

                let (_, elements, _) = parse_sequence(
                    f,
                    TokenKind::OpenBlock,
                    TokenKind::Comma,
                    TokenKind::CloseBlock,
                    |parser| {
                        let key = parser.take(TokenKind::Ident)?;
                        parser.take(TokenKind::Colon)?;
                        let value = parse_expr(parser, true)?;
                        Some(KeyValue {
                            pos: key.pos,
                            key,
                            value,
                        })
                    },
                )?;
                let target = convert_expr_to_type_expr(target);
                ExprNode::Struct(StructExprNode {
                    pos,
                    target,
                    elements,
                })
            }
            _ => {
                break;
            }
        };
        pos = target.pos();
    }

    Some(target)
}

fn convert_expr_to_type_expr(node: ExprNode) -> TypeExprNode {
    let pos = node.pos();
    match node {
        ExprNode::Path(path) => TypeExprNode::Path(path),
        ExprNode::Grouped(node) => {
            TypeExprNode::Grouped(Box::new(convert_expr_to_type_expr(*node)))
        }
        ExprNode::Number(tok)
        | ExprNode::Null(tok)
        | ExprNode::Bool(tok)
        | ExprNode::String(tok)
        | ExprNode::Unary(UnaryExprNode { op: tok, value: _ }) => TypeExprNode::Invalid(tok.pos),
        ExprNode::Selection(..) => TypeExprNode::Invalid(pos),
        ExprNode::Binary(node) => TypeExprNode::Invalid(node.a.pos()),
        ExprNode::Deref(node) => TypeExprNode::Invalid(node.value.pos()),
        ExprNode::Call(node) => TypeExprNode::Invalid(node.callee.pos()),
        ExprNode::Cast(node) => TypeExprNode::Invalid(node.value.pos()),
        ExprNode::Struct(node) => TypeExprNode::Invalid(node.target.pos()),
        ExprNode::Index(node) => TypeExprNode::Invalid(node.value.pos()),
    }
}

fn parse_primary_expr<E: ErrorReporter>(f: &mut FileParser<E>) -> Option<ExprNode> {
    match f.kind() {
        TokenKind::Ident => parse_path_for_expr(f).map(ExprNode::Path),
        TokenKind::NumberLit => f.take(TokenKind::NumberLit).map(ExprNode::Number),
        TokenKind::StringLit => f.take(TokenKind::StringLit).map(ExprNode::String),
        TokenKind::Null => f.take(TokenKind::Null).map(ExprNode::Null),
        TokenKind::True => f.take(TokenKind::True).map(ExprNode::Bool),
        TokenKind::False => f.take(TokenKind::False).map(ExprNode::Bool),
        TokenKind::OpenBrac => {
            let _ = f.take(TokenKind::OpenBrac).unwrap();
            let expr = parse_expr(f, true)?;
            let _ = f.take(TokenKind::CloseBrac)?;
            Some(ExprNode::Grouped(Box::new(expr)))
        }
        TokenKind::SemiColon => None,
        _ => {
            let tok = f.token();
            f.errors.unexpected_token(tok.pos, tok.kind);
            None
        }
    }
}

fn parse_path_for_expr<E: ErrorReporter>(f: &mut FileParser<E>) -> Option<PathNode> {
    let ident = f.take_if(TokenKind::Ident)?;

    let mut names = vec![ident];
    let mut args = Vec::default();
    while f.take_if(TokenKind::DoubleColon).is_some() {
        match f.kind() {
            TokenKind::Ident => {
                let ident = f.take(TokenKind::Ident).unwrap();
                names.push(ident);
            }
            TokenKind::Lt => {
                args = parse_sequence(
                    f,
                    TokenKind::Lt,
                    TokenKind::Comma,
                    TokenKind::Gt,
                    parse_type_expr,
                )
                .map(|(_, type_args, _)| type_args)
                .unwrap_or_default();
                break;
            }
            _ => {
                let tok = f.token();
                f.errors.missing(tok.pos, "ident or generic args");
                break;
            }
        };
    }

    Some(PathNode { names, args })
}

impl<'a, Error: ErrorReporter> FileParser<'a, Error> {
    fn new(errors: &'a Error, tokens: VecDeque<Token>, last_pos: Pos) -> Self {
        Self {
            errors,
            tokens,
            last_pos,
        }
    }

    fn unexpected(&mut self, expected: impl Display) {
        let token = self.token();
        self.errors
            .unexpected_parsing(token.pos, expected, token.kind);
    }

    fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    fn token(&mut self) -> Token {
        if let Some(tok) = self.tokens.front() {
            tok.clone()
        } else {
            Token {
                kind: TokenKind::Eof,
                value: "".into(),
                pos: self.last_pos,
            }
        }
    }

    fn kind(&mut self) -> TokenKind {
        self.tokens
            .front()
            .map(|tok| tok.kind)
            .unwrap_or(TokenKind::Eof)
    }

    fn pop(&mut self) -> Token {
        if let Some(tok) = self.tokens.pop_front() {
            tok
        } else {
            Token {
                kind: TokenKind::Eof,
                value: "".into(),
                pos: self.last_pos,
            }
        }
    }

    fn take(&mut self, kind: TokenKind) -> Option<Token> {
        if kind == TokenKind::Gt {
            self.split_shift_right();
        }

        let token = self.token();
        if token.kind == kind {
            Some(self.pop())
        } else {
            self.errors.unexpected_parsing(token.pos, kind, token.kind);
            None
        }
    }

    fn take_if(&mut self, kind: TokenKind) -> Option<Token> {
        if kind == TokenKind::Gt {
            self.split_shift_right();
        }

        let tok = self
            .tokens
            .front()
            .and_then(|tok| if tok.kind == kind { Some(tok) } else { None });
        if tok.is_some() {
            let tok = self.tokens.pop_front().unwrap();
            Some(tok)
        } else {
            None
        }
    }

    fn split_shift_right(&mut self) {
        let tok = self.tokens.front();
        if tok.map_or(false, |token| token.kind == TokenKind::ShiftRight) {
            let tok = self.tokens.pop_front().unwrap();
            let pos = tok.pos;
            self.tokens.push_front(Token {
                kind: TokenKind::Gt,
                value: ">".to_string(),
                pos: pos.with_offset(1),
            });
            self.tokens.push_front(Token {
                kind: TokenKind::Gt,
                value: ">".to_string(),
                pos,
            });
        }
    }

    fn skip_until_before(&mut self, kind: &[TokenKind]) {
        while let Some(tok) = self.tokens.front() {
            if !kind.contains(&tok.kind) {
                self.tokens.pop_front();
            } else {
                break;
            }
        }
    }
}

trait ParsingError: ErrorReporter {
    fn unexpected_parsing(&self, pos: Pos, expected: impl Display, found: impl Display) {
        self.report(pos, format!("Expected {expected}, but found {found}"));
    }

    fn missing(&self, pos: Pos, component: impl Display) {
        self.report(pos, format!("Missing {component}"));
    }

    fn unexpected_token(&self, pos: Pos, kind: TokenKind) {
        self.report(pos, format!("Unexpected token {kind}"));
    }

    fn dangling_annotations(&self, pos: Pos) {
        self.report(pos, String::from("There is no object to annotate"));
    }
}

impl<T> ParsingError for T where T: ErrorReporter {}
