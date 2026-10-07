use crate::ast::*;
use crate::error::ErrorManager;
use crate::scanner::scan;
use crate::token::{File, Pos, Token, TokenKind};
use std::collections::VecDeque;
use std::fmt::Display;

pub fn parse(errors: &ErrorManager, file: &File) -> PackageNode {
    let scan_result = scan(errors, file);

    let mut comments = Vec::default();
    let mut filtered_tokens = VecDeque::default();
    for tok in scan_result.into_iter() {
        if let TokenKind::Comment(value) = tok.kind {
            comments.push(Comment {
                value,
                pos: tok.pos,
            });
        } else {
            filtered_tokens.push_back(tok);
        }
    }

    let mut parser = FileParser::new(errors, filtered_tokens);

    let items = parse_root(&mut parser);
    PackageNode { items, comments }
}

struct FileParser<'a> {
    errors: &'a ErrorManager,
    tokens: VecDeque<Token>,
}

fn parse_root(f: &mut FileParser) -> Vec<ItemNode> {
    let mut items = Vec::<ItemNode>::default();

    while f.kind() != &TokenKind::Eof {
        if let Some(item) = parse_item_node(f) {
            items.push(item);
        }
    }

    items
}

fn parse_item_node(f: &mut FileParser) -> Option<ItemNode> {
    let annotations = parse_annotations(f);

    let tok = f.token();
    let item = match &tok.kind {
        TokenKind::Import => parse_import(f, annotations).map(ItemNode::Import),
        TokenKind::Struct => parse_struct(f, annotations).map(ItemNode::Struct),
        TokenKind::Let => parse_global(f, annotations).map(ItemNode::Global),
        TokenKind::Fn => parse_func(f, annotations).map(ItemNode::Function),
        TokenKind::Eof => {
            if let Some(annotation) = annotations.last() {
                report_dangling_annotations(f.errors, annotation.pos);
            }
            None
        }
        _ => {
            f.unexpected("top level definition");
            None
        }
    };

    if item.is_none() {
        f.skip_until_before(TOP_LEVEL_STOPPING_TOKEN);
        f.take_if(&TokenKind::SemiColon);
    }

    item
}

const TOP_LEVEL_STOPPING_TOKEN: &[TokenKind] = &[
    TokenKind::Let,
    TokenKind::Fn,
    TokenKind::Import,
    TokenKind::Struct,
    TokenKind::SemiColon,
];

fn parse_annotations(f: &mut FileParser) -> Vec<AnnotationNode> {
    let mut result = Vec::default();

    while let Some(at_sign) = f.take_if(&TokenKind::AtSign) {
        let pos = at_sign.pos;
        let Some(ident) = f.take_if_ident() else {
            f.unexpected("annotation identifier");
            f.skip_until_before(TOP_LEVEL_STOPPING_TOKEN);
            continue;
        };

        let args = parse_sequence(
            f,
            TokenKind::OpenBrac,
            TokenKind::Comma,
            TokenKind::CloseBrac,
            |this| this.take_if_string_lit(),
        );

        let Some((_, arguments, _)) = args else {
            f.unexpected("annotation arguments");
            f.skip_until_before(TOP_LEVEL_STOPPING_TOKEN);
            continue;
        };

        let arguments = arguments.into_iter().map(StringLit::from).collect();
        result.push(AnnotationNode {
            pos,
            name: ident,
            arguments,
        });
    }

    result
}

fn parse_sequence<T, F>(
    f: &mut FileParser,
    begin_tok: TokenKind,
    delim_tok: TokenKind,
    end_tok: TokenKind,
    parse_fn: F,
) -> Option<(Token, Vec<T>, Token)>
where
    F: Fn(&mut FileParser) -> Option<T>,
{
    let opening = f.take_if(&begin_tok)?;

    let mut items = Vec::<T>::default();
    let mut needs_delimiter = false;
    loop {
        if let Some(closing) = f.take_if(&end_tok) {
            return Some((opening, items, closing));
        }
        if f.kind() == &TokenKind::Eof {
            break;
        }
        if needs_delimiter && f.take_if(&delim_tok).is_some() {
            needs_delimiter = false;
            continue;
        }

        let token = f.token();
        if let Some(item) = parse_fn(f) {
            if needs_delimiter {
                report_unexpected_parsing(f.errors, token.pos, &delim_tok, token);
            }
            items.push(item);
            needs_delimiter = true;
        } else if f.kind() == &delim_tok {
            f.unexpected("list item");
            f.pop();
            needs_delimiter = false;
        } else {
            break;
        }
    }

    if let Some(closing) = f.take_if(&end_tok) {
        Some((opening, items, closing))
    } else {
        report_missing(f.errors, opening.pos, format!("closing {end_tok}"));
        None
    }
}

fn parse_import(f: &mut FileParser, annotations: Vec<AnnotationNode>) -> Option<ImportNode> {
    let import_tok = f.take_if(&TokenKind::Import)?;
    let pos = import_tok.pos;
    let name = f.take_ident()?;
    let path = f.take_string_lit()?;
    f.take(TokenKind::SemiColon)?;
    Some(ImportNode {
        pos,
        annotations,
        name,
        path,
    })
}

fn parse_global(f: &mut FileParser, annotations: Vec<AnnotationNode>) -> Option<GlobalNode> {
    let let_tok = f.take_if(&TokenKind::Let)?;
    let pos = let_tok.pos;

    let name = f.take_ident()?;

    f.take(TokenKind::Colon)?;
    let ty = if let Some(ty) = parse_type_expr(f) {
        ty
    } else {
        let pos = f.token().pos;
        report_missing(f.errors, pos, "type expression");
        TypeExprNode::Invalid(pos)
    };

    if matches!(&ty, TypeExprNode::Invalid(..)) {
        // when the type is invalid, it means the error during typecheck is
        // already reported, and thus we don't need to report anything anymore
        // and just skip until next checkpoint.
        f.skip_until_before(&[TokenKind::SemiColon, TokenKind::Equal]);
    }

    let value = if f.take_if(&TokenKind::Equal).is_some() {
        Some(parse_expr(f, true).unwrap_or_else(|| {
            let pos = f.token().pos;
            report_missing(f.errors, pos, "initializer value expression");
            ExprNode::Invalid(pos)
        }))
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

fn parse_type_expr(f: &mut FileParser) -> Option<TypeExprNode> {
    let tok = f.token();
    match &tok.kind {
        TokenKind::OpenSquare => {
            let tok = f.pop();
            if f.take(TokenKind::Mul).is_none() {
                return Some(TypeExprNode::Invalid(tok.pos));
            }
            let Some(close_tok) = f.take(TokenKind::CloseSquare) else {
                return Some(TypeExprNode::Invalid(tok.pos));
            };
            let ty = if let Some(ty) = parse_type_expr(f) {
                ty
            } else {
                report_missing(f.errors, close_tok.pos, "pointee type");
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
                report_missing(f.errors, tok.pos, "pointee type");
                TypeExprNode::Invalid(tok.pos)
            };
            Some(TypeExprNode::Ptr(PtrTypeNode {
                pos: tok.pos,
                ty: Box::new(ty),
            }))
        }
        TokenKind::Fn => {
            let tok = f.pop();

            let param_result = parse_sequence(
                f,
                TokenKind::OpenBrac,
                TokenKind::Comma,
                TokenKind::CloseBrac,
                parse_func_type_parameter,
            );
            if param_result.is_none() {
                report_missing(f.errors, tok.pos, "function parameter list");
            }

            let return_type = if let Some(colon_tok) = f.take_if(&TokenKind::Colon) {
                if let Some(expr) = parse_type_expr(f) {
                    Some(expr)
                } else {
                    report_missing(f.errors, colon_tok.pos, "return type");
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
            let inner_ty = parse_type_expr(f).unwrap_or_else(|| {
                let pos = f.token().pos;
                report_missing(f.errors, pos, "grouped type");
                TypeExprNode::Invalid(pos)
            });
            f.take(TokenKind::CloseBrac);
            Some(TypeExprNode::Grouped(Box::new(inner_ty)))
        }
        TokenKind::Ident { .. } => {
            let pos = tok.pos;
            Some(parse_named_type(f).unwrap_or(TypeExprNode::Invalid(pos)))
        }
        TokenKind::SemiColon => None,
        _ => {
            if tok.kind.is_keyword() {
                f.unexpected("type expression");
                let tok = f.pop();
                Some(TypeExprNode::Invalid(tok.pos))
            } else {
                None
            }
        }
    }
}

fn parse_func_type_parameter(f: &mut FileParser) -> Option<FuncTypeParam> {
    let name = if f.tokens.len() >= 2
        && matches!(f.tokens[0].kind, TokenKind::Ident(..))
        && matches!(f.tokens[1].kind, TokenKind::Colon)
    {
        let name = f.take_ident()?;
        f.take(TokenKind::Colon)?;
        Some(name)
    } else {
        None
    };

    let ty = if let Some(ty) = parse_type_expr(f) {
        ty
    } else if name.is_some() {
        let pos = f.token().pos;
        report_missing(f.errors, pos, "parameter type");
        TypeExprNode::Invalid(pos)
    } else {
        return None;
    };
    let pos = name.as_ref().map(|t| t.pos).unwrap_or_else(|| ty.pos());
    Some(FuncTypeParam { pos, name, ty })
}

fn parse_named_type(f: &mut FileParser) -> Option<TypeExprNode> {
    let ident = f.take_if_ident()?;
    let mut ty = TypeExprNode::Ident(ident);
    while f.take_if(&TokenKind::Dot).is_some() {
        ty = TypeExprNode::Selection(SelectionTypeNode {
            value: Box::new(ty),
            selection: f.take_ident()?,
        });
    }

    if f.kind() == &TokenKind::Lt {
        let (opening, args, _) = parse_sequence(
            f,
            TokenKind::Lt,
            TokenKind::Comma,
            TokenKind::Gt,
            parse_type_expr,
        )?;
        if args.is_empty() {
            report_missing(f.errors, opening.pos, "at least one type argument");
        }
        ty = TypeExprNode::Inst(InstTypeNode {
            value: Box::new(ty),
            args,
        });
    }

    Some(ty)
}

fn parse_struct(f: &mut FileParser, annotations: Vec<AnnotationNode>) -> Option<StructNode> {
    let struct_tok = f.take_if(&TokenKind::Struct)?;
    let pos = struct_tok.pos;

    let name = f.take_ident();

    let type_params = if f.kind() == &TokenKind::Lt {
        let result = parse_sequence(
            f,
            TokenKind::Lt,
            TokenKind::Comma,
            TokenKind::Gt,
            |parser| parser.take_ident(),
        );
        result.map(|(opening, type_params, _)| {
            if type_params.is_empty() {
                report_missing(f.errors, opening.pos, "at least one type parameter");
            }
            type_params
                .into_iter()
                .map(Identifier::from)
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
            let name = parser.take_ident()?;
            let ty = if parser.take(TokenKind::Colon).is_some() {
                parse_type_expr(parser).unwrap_or_else(|| {
                    let pos = parser.token().pos;
                    report_missing(parser.errors, pos, "struct field type");
                    TypeExprNode::Invalid(pos)
                })
            } else {
                TypeExprNode::Invalid(parser.token().pos)
            };
            Some(StructFieldNode { name, ty })
        },
    );
    if fields.is_none() {
        report_unexpected_parsing(f.errors, f.token().pos, "struct body", f.token().kind);
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

fn parse_func(f: &mut FileParser, annotations: Vec<AnnotationNode>) -> Option<FunctionNode> {
    let signature = parse_signature(f, annotations)?;
    let pos = signature.pos;
    if f.take_if(&TokenKind::SemiColon).is_some() {
        return Some(FunctionNode {
            pos,
            signature,
            body: None,
        });
    }

    let Some(body) = parse_block_stmt(f) else {
        report_missing(f.errors, signature.end_pos, "function body");
        return None;
    };
    Some(FunctionNode {
        pos,
        signature,
        body: Some(body),
    })
}

fn parse_signature(f: &mut FileParser, annotations: Vec<AnnotationNode>) -> Option<SignatureNode> {
    let func = f.take(TokenKind::Fn)?;
    let pos = func.pos;
    let name: Identifier = f.take_ident()?;

    let type_params = if f.kind() == &TokenKind::Lt {
        let (opening, type_parameters, _) = parse_sequence(
            f,
            TokenKind::Lt,
            TokenKind::Comma,
            TokenKind::Gt,
            |parser| parser.take_ident(),
        )?;
        if type_parameters.is_empty() {
            report_missing(f.errors, opening.pos, "at least one type parameter");
        }
        type_parameters
            .into_iter()
            .map(Identifier::from)
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
        report_missing(f.errors, name.pos, "function parameter list");
    }

    let return_type = if let Some(colon_tok) = f.take_if(&TokenKind::Colon) {
        if let Some(expr) = parse_type_expr(f) {
            Some(expr)
        } else {
            report_missing(f.errors, colon_tok.pos, "return type");
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

fn parse_parameter(f: &mut FileParser) -> Option<ParameterNode> {
    let name = f.take_if_ident()?;
    let pos = name.pos;
    let ty = if f.take(TokenKind::Colon).is_some() {
        parse_type_expr(f).unwrap_or_else(|| {
            let pos = f.token().pos;
            report_missing(f.errors, pos, "parameter type");
            TypeExprNode::Invalid(pos)
        })
    } else {
        TypeExprNode::Invalid(f.token().pos)
    };
    Some(ParameterNode { pos, name, ty })
}

fn parse_stmt(f: &mut FileParser) -> Option<StatementNode> {
    Some(match f.kind() {
        TokenKind::If => StatementNode::If(parse_if_stmt(f)?),
        TokenKind::While => StatementNode::While(parse_while_stmt(f)?),
        TokenKind::For => StatementNode::For(parse_for_stmt(f)?),
        TokenKind::Defer => StatementNode::Defer(parse_defer_stmt(f)?),
        TokenKind::OpenBlock => StatementNode::Block(parse_block_stmt(f)?),
        TokenKind::Continue => {
            let pos = f.take(TokenKind::Continue).unwrap().pos;
            f.take(TokenKind::SemiColon);
            StatementNode::Continue(pos)
        }
        TokenKind::Break => {
            let pos = f.take(TokenKind::Break).unwrap().pos;
            f.take(TokenKind::SemiColon);
            StatementNode::Break(pos)
        }
        TokenKind::Return => StatementNode::Return(parse_return_stmt(f)?),
        _ => {
            let stmt = parse_simple_stmt(f, true)?;
            f.take(TokenKind::SemiColon);
            stmt
        }
    })
}

// a simple statement is a let, an assignment or an expression statement. it is parsed without its
// ';' since it is also used as the initialization and the update of a for statement.
fn parse_simple_stmt(f: &mut FileParser, allow_struct_lit: bool) -> Option<StatementNode> {
    if f.kind() == &TokenKind::Let {
        return Some(StatementNode::Let(parse_let_stmt(f, allow_struct_lit)?));
    }

    let expr = parse_expr(f, allow_struct_lit)?;
    if matches!(expr, ExprNode::Invalid(..)) {
        f.skip_until_before_matching(|kind| {
            matches!(
                kind,
                TokenKind::SemiColon | TokenKind::Equal | TokenKind::AssignOp(_)
            )
        });
    }
    let pos = expr.pos();
    let op = match f.kind() {
        TokenKind::Equal => None,
        TokenKind::AssignOp(op) => Some(*op),
        _ => return Some(StatementNode::Expr(expr)),
    };
    f.pop();
    let value = parse_expr(f, allow_struct_lit).unwrap_or_else(|| {
        let pos = f.token().pos;
        report_missing(f.errors, pos, "right-hand operand");
        ExprNode::Invalid(pos)
    });
    Some(StatementNode::Assign(AssignStatementNode {
        pos,
        receiver: expr,
        op,
        value,
    }))
}

fn parse_let_stmt(f: &mut FileParser, allow_struct_lit: bool) -> Option<LetStatementNode> {
    let let_tok = f.take(TokenKind::Let)?;
    let pos = let_tok.pos;

    let name: Identifier = f.take_ident()?;

    if f.take_if(&TokenKind::Colon).is_some() {
        let ty = parse_type_expr(f).unwrap_or_else(|| {
            let pos = f.token().pos;
            report_missing(f.errors, pos, "local variable type");
            TypeExprNode::Invalid(pos)
        });
        if f.take_if(&TokenKind::Equal).is_some() {
            let value = parse_expr(f, allow_struct_lit).unwrap_or_else(|| {
                let pos = f.token().pos;
                report_missing(f.errors, pos, "initial value expression");
                ExprNode::Invalid(pos)
            });
            Some(LetStatementNode {
                pos,
                name,
                kind: LetKind::TypeValue { ty, value },
            })
        } else {
            Some(LetStatementNode {
                pos,
                name,
                kind: LetKind::TypeOnly { ty },
            })
        }
    } else if f.take(TokenKind::Equal).is_some() {
        let value = parse_expr(f, allow_struct_lit).unwrap_or_else(|| {
            let pos = f.token().pos;
            report_missing(f.errors, pos, "initial value expression");
            ExprNode::Invalid(pos)
        });
        Some(LetStatementNode {
            pos,
            name,
            kind: LetKind::ValueOnly { value },
        })
    } else {
        f.unexpected("type or value");
        f.skip_until_before(&[TokenKind::SemiColon]);
        Some(LetStatementNode {
            pos,
            name,
            kind: LetKind::Invalid,
        })
    }
}

fn parse_if_stmt(f: &mut FileParser) -> Option<IfStatementNode> {
    let if_tok = f.take(TokenKind::If)?;
    let pos = if_tok.pos;

    let Some(condition) = parse_expr(f, false) else {
        report_missing(f.errors, if_tok.pos, "if condition");
        return None;
    };

    let Some(body) = parse_block_stmt(f) else {
        report_missing(f.errors, if_tok.pos, "if body");
        return None;
    };

    let else_node = if let Some(else_tok) = f.take_if(&TokenKind::Else) {
        if f.kind() == &TokenKind::If {
            parse_if_stmt(f).map(StatementNode::If).map(Box::new)
        } else if f.kind() == &TokenKind::OpenBlock {
            parse_block_stmt(f).map(StatementNode::Block).map(Box::new)
        } else {
            report_missing(f.errors, else_tok.pos, "else body");
            None
        }
    } else {
        None
    };

    Some(IfStatementNode {
        pos,
        condition,
        body,
        else_node,
    })
}

fn parse_while_stmt(f: &mut FileParser) -> Option<WhileStatementNode> {
    let while_tok = f.take(TokenKind::While)?;
    let pos = while_tok.pos;

    let Some(condition) = parse_expr(f, false) else {
        report_missing(f.errors, while_tok.pos, "while condition");
        return None;
    };

    let Some(body) = parse_block_stmt(f) else {
        report_missing(f.errors, while_tok.pos, "while body");
        return None;
    };

    Some(WhileStatementNode {
        pos,
        condition,
        body,
    })
}

fn parse_for_stmt(f: &mut FileParser) -> Option<ForStatementNode> {
    let for_tok = f.take(TokenKind::For)?;
    let pos = for_tok.pos;
    let mut open_block = false;

    let init = if f.kind() == &TokenKind::SemiColon {
        None
    } else if let Some(init) = parse_simple_stmt(f, true) {
        Some(Box::new(init))
    } else {
        report_missing(f.errors, f.token().pos, "for init statement");
        f.skip_until_before(&[TokenKind::SemiColon, TokenKind::OpenBlock]);
        open_block = f.kind() == &TokenKind::OpenBlock;
        None
    };
    if !open_block {
        f.take(TokenKind::SemiColon)?;
    }

    let condition = if open_block || f.kind() == &TokenKind::SemiColon {
        None
    } else if let Some(condition) = parse_expr(f, false) {
        Some(condition)
    } else {
        report_missing(f.errors, f.token().pos, "for condition");
        f.skip_until_before(&[TokenKind::SemiColon, TokenKind::OpenBlock]);
        open_block = f.kind() == &TokenKind::OpenBlock;
        None
    };
    if !open_block {
        f.take(TokenKind::SemiColon)?;
    }

    let update = if open_block || f.kind() == &TokenKind::OpenBlock {
        None
    } else if let Some(update) = parse_simple_stmt(f, false) {
        Some(Box::new(update))
    } else {
        report_missing(f.errors, f.token().pos, "for update statement");
        f.skip_until_before(&[TokenKind::SemiColon, TokenKind::OpenBlock]);
        f.take_if(&TokenKind::SemiColon);
        None
    };

    let Some(body) = parse_block_stmt(f) else {
        report_missing(f.errors, pos, "for body");
        return None;
    };

    Some(ForStatementNode {
        pos,
        init,
        condition,
        update,
        body,
    })
}

fn parse_defer_stmt(f: &mut FileParser) -> Option<DeferStatementNode> {
    let defer_tok = f.take(TokenKind::Defer)?;
    let pos = defer_tok.pos;
    let body_token = f.token();
    let body = parse_stmt(f).unwrap_or_else(|| {
        if f.token() == body_token {
            f.unexpected("deferred statement");
        }
        StatementNode::Expr(ExprNode::Invalid(body_token.pos))
    });

    Some(DeferStatementNode {
        pos,
        body: Box::new(body),
    })
}

fn parse_block_stmt(f: &mut FileParser) -> Option<BlockStatementNode> {
    let open = f.take_if(&TokenKind::OpenBlock)?;
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
            f.take_if(&TokenKind::SemiColon);
        }
    }
    f.take(TokenKind::CloseBlock);
    Some(BlockStatementNode { pos, statements })
}

fn parse_return_stmt(f: &mut FileParser) -> Option<ReturnStatementNode> {
    let return_tok = f.take(TokenKind::Return)?;
    let pos = return_tok.pos;
    if f.take_if(&TokenKind::SemiColon).is_some() {
        return Some(ReturnStatementNode { pos, value: None });
    }

    let Some(value) = parse_expr(f, true) else {
        let token = f.token();
        report_unexpected_parsing(f.errors, token.pos, "return value expression", &token);
        return Some(ReturnStatementNode {
            pos,
            value: Some(ExprNode::Invalid(token.pos)),
        });
    };

    f.take(TokenKind::SemiColon)?;
    Some(ReturnStatementNode {
        pos,
        value: Some(value),
    })
}

// parse_expr returns None when there is no expression that can be parsed. Returning
// None means no token is consumed. parse_expr may parses incomplete expression, which
// in that case, it will return Some(ExprNode::Invalid) and report error internally.
fn parse_expr(f: &mut FileParser, allow_struct_lit: bool) -> Option<ExprNode> {
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

fn parse_binary_expr(
    f: &mut FileParser,
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
    while op.contains(f.kind()) {
        let op_token = f.pop();
        let b = if let Some(next_op) = next_op {
            parse_binary_expr(f, next_op, allow_struct_lit)
        } else {
            parse_cast_expr(f, allow_struct_lit)
        };

        let b = b.unwrap_or_else(|| {
            let pos = f.token().pos;
            report_missing(f.errors, pos, "second operand".to_string());
            ExprNode::Invalid(pos)
        });
        result = ExprNode::Binary(BinaryExprNode {
            a: Box::new(result),
            op: op_token.kind.into(),
            b: Box::new(b),
        });
    }

    Some(result)
}

fn parse_cast_expr(f: &mut FileParser, allow_struct_lit: bool) -> Option<ExprNode> {
    let value = parse_unary_expr(f, allow_struct_lit)?;
    if f.take_if(&TokenKind::As).is_some() {
        let target = match parse_type_expr(f) {
            Some(t) => t,
            None => {
                let pos = f.token().pos;
                f.skip_until_before(&[TokenKind::SemiColon]);
                report_missing(f.errors, pos, "target type");
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

fn parse_unary_expr(f: &mut FileParser, allow_struct_lit: bool) -> Option<ExprNode> {
    let mut ops = vec![];
    while UNARY_OP.contains(f.kind()) {
        let op = f.tokens.pop_front().unwrap();
        ops.push(op);
    }

    let mut value = match parse_sequence_of_expr(f, allow_struct_lit) {
        Some(value) => value,
        None if ops.is_empty() => return None,
        None => {
            let pos = f.token().pos;
            report_missing(f.errors, pos, "unary operand");
            return Some(ExprNode::Invalid(pos));
        }
    };
    while let Some(op) = ops.pop() {
        value = ExprNode::Unary(UnaryExprNode {
            pos: op.pos,
            op: op.kind.into(),
            value: Box::new(value),
        })
    }

    Some(value)
}

fn parse_sequence_of_expr(f: &mut FileParser, allow_struct_lit: bool) -> Option<ExprNode> {
    let mut target = parse_primary_expr(f)?;
    let mut pos = target.pos();

    loop {
        target = match f.kind() {
            TokenKind::Lt if matches!(&target, ExprNode::Ident(..) | ExprNode::Selection(..)) => {
                let Some(args) = f.try_take_generic_args(allow_struct_lit) else {
                    break;
                };
                ExprNode::Inst(InstExprNode {
                    value: Box::new(target),
                    args,
                })
            }
            TokenKind::Dot => {
                f.take(TokenKind::Dot)?;
                match f.kind() {
                    TokenKind::Ident { .. } => {
                        let selection: Identifier = f.take_ident()?;
                        ExprNode::Selection(SelectionExprNode {
                            value: Box::new(target),
                            selection,
                        })
                    }
                    TokenKind::Mul | TokenKind::AssignOp(BinaryOp::Mul) => {
                        let tok = f.take(TokenKind::Mul)?;
                        ExprNode::Deref(DerefExprNode {
                            pos: tok.pos,
                            value: Box::new(target),
                        })
                    }
                    _ => {
                        f.unexpected("ident or '*'");
                        f.skip_until_before(&[TokenKind::SemiColon]);
                        ExprNode::Invalid(pos)
                    }
                }
            }
            TokenKind::OpenSquare => {
                let open = f.take(TokenKind::OpenSquare).unwrap();
                let Some(index) = parse_expr(f, true) else {
                    report_missing(f.errors, f.token().pos, "index expression");
                    f.take_if(&TokenKind::CloseSquare);
                    return Some(ExprNode::Invalid(open.pos));
                };
                if f.take(TokenKind::CloseSquare).is_none() {
                    return Some(ExprNode::Invalid(open.pos));
                }
                ExprNode::Index(IndexExprNode {
                    value: Box::new(target),
                    index: Box::new(index),
                })
            }
            TokenKind::OpenBrac => {
                let Some((_, arguments, _)) = parse_sequence(
                    f,
                    TokenKind::OpenBrac,
                    TokenKind::Comma,
                    TokenKind::CloseBrac,
                    |this| match parse_expr(this, true) {
                        Some(argument) => Some(argument),
                        None if this.kind() == &TokenKind::Comma => {
                            let pos = this.token().pos;
                            report_missing(this.errors, pos, "function argument");
                            Some(ExprNode::Invalid(pos))
                        }
                        None => None,
                    },
                ) else {
                    return Some(ExprNode::Invalid(pos));
                };
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

                let Some((_, elements, _)) = parse_sequence(
                    f,
                    TokenKind::OpenBlock,
                    TokenKind::Comma,
                    TokenKind::CloseBlock,
                    |parser| {
                        let key: Identifier = parser.take_ident()?;
                        let value = if parser.take(TokenKind::Colon).is_some() {
                            parse_expr(parser, true).unwrap_or_else(|| {
                                let pos = parser.token().pos;
                                report_missing(parser.errors, pos, "struct field value");
                                ExprNode::Invalid(pos)
                            })
                        } else {
                            ExprNode::Invalid(parser.token().pos)
                        };
                        Some(KeyValue {
                            pos: key.pos,
                            key,
                            value,
                        })
                    },
                ) else {
                    return Some(ExprNode::Invalid(pos));
                };
                let target = convert_expr_to_type_expr(f.errors, target);
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

fn convert_expr_to_type_expr(errors: &ErrorManager, node: ExprNode) -> TypeExprNode {
    match node {
        ExprNode::Invalid(pos) => TypeExprNode::Invalid(pos),
        ExprNode::Ident(ident) => TypeExprNode::Ident(ident),
        ExprNode::Grouped(node) => {
            TypeExprNode::Grouped(Box::new(convert_expr_to_type_expr(errors, *node)))
        }
        ExprNode::Selection(selection) => TypeExprNode::Selection(SelectionTypeNode {
            value: Box::new(convert_expr_to_type_expr(errors, *selection.value)),
            selection: selection.selection,
        }),
        ExprNode::Inst(inst) => TypeExprNode::Inst(InstTypeNode {
            value: Box::new(convert_expr_to_type_expr(errors, *inst.value)),
            args: inst.args,
        }),

        ExprNode::Number(..)
        | ExprNode::String(..)
        | ExprNode::Null(..)
        | ExprNode::Bool(..)
        | ExprNode::Char(..)
        | ExprNode::Binary(..)
        | ExprNode::Unary(..)
        | ExprNode::Deref(..)
        | ExprNode::Call(..)
        | ExprNode::Cast(..)
        | ExprNode::Struct(..)
        | ExprNode::Index(..) => {
            report_invalid_struct_literal_target(errors, node.pos());
            TypeExprNode::Invalid(node.pos())
        }
    }
}

const EXPR_RECOVERY_TOKENS: &[TokenKind] = &[
    TokenKind::SemiColon,
    TokenKind::Comma,
    TokenKind::CloseBrac,
    TokenKind::CloseBlock,
    TokenKind::CloseSquare,
    TokenKind::OpenBlock,
    TokenKind::Else,
];

fn parse_primary_expr(f: &mut FileParser) -> Option<ExprNode> {
    match f.kind() {
        TokenKind::Ident { .. } => f.take_if_ident().map(ExprNode::Ident),
        TokenKind::NumberLit { .. } => f.take_number_lit().map(ExprNode::Number),
        TokenKind::CharLit { .. } => f.take_char_lit().map(ExprNode::Char),
        TokenKind::StringLit { .. } => f.take_string_lit().map(ExprNode::String),
        TokenKind::Null => f.take(TokenKind::Null).map(|t| ExprNode::Null(t.pos)),
        TokenKind::True => f
            .take(TokenKind::True)
            .map(BoolLiteral::from)
            .map(ExprNode::Bool),
        TokenKind::False => f
            .take(TokenKind::False)
            .map(BoolLiteral::from)
            .map(ExprNode::Bool),
        TokenKind::OpenBrac => {
            let open = f.take(TokenKind::OpenBrac).unwrap();
            let Some(expr) = parse_expr(f, true) else {
                report_missing(f.errors, f.token().pos, "grouped expression");
                f.take_if(&TokenKind::CloseBrac);
                return Some(ExprNode::Invalid(open.pos));
            };
            if f.take(TokenKind::CloseBrac).is_none() {
                return Some(ExprNode::Invalid(open.pos));
            }
            if matches!(expr, ExprNode::Invalid(..)) {
                Some(ExprNode::Invalid(open.pos))
            } else {
                Some(ExprNode::Grouped(Box::new(expr)))
            }
        }
        TokenKind::SemiColon
        | TokenKind::Comma
        | TokenKind::CloseBrac
        | TokenKind::CloseBlock
        | TokenKind::CloseSquare
        | TokenKind::OpenBlock
        | TokenKind::Else
        | TokenKind::Eof => None,
        _ => {
            let tok = f.pop();
            report_unexpected_token(f.errors, tok.pos, tok.kind);
            f.skip_until_before(EXPR_RECOVERY_TOKENS);
            Some(ExprNode::Invalid(tok.pos))
        }
    }
}

impl<'a> FileParser<'a> {
    fn new(errors: &'a ErrorManager, tokens: VecDeque<Token>) -> Self {
        debug_assert!(
            tokens
                .back()
                .is_some_and(|token| token.kind == TokenKind::Eof)
        );
        Self { errors, tokens }
    }

    fn try_take_generic_args(&mut self, allow_struct_lit: bool) -> Option<Vec<TypeExprNode>> {
        // currently, parsing a<T> is ambiguous because it can mean an instantiation or
        // binary expressions. To handle this, we assume it's an instantiation first and
        // fallback to binary expression if it doesn't result in valid AST. Because of
        // that, we need to be able to backtrack. To do that, we need to create a new
        // parser and scrap it if we want to backtrack.
        let errors = ErrorManager::default();
        let mut probe = FileParser::new(&errors, self.tokens.clone());
        let (_, args, _) = parse_sequence(
            &mut probe,
            TokenKind::Lt,
            TokenKind::Comma,
            TokenKind::Gt,
            parse_type_expr,
        )?;

        // these are some heuristic to decide whether this is a generic instantiation
        let follower = probe.kind().clone();
        let is_unambiguous_binary_operator =
            follower != TokenKind::Gt && BINOP_PRECEDENCE.iter().any(|ops| ops.contains(&follower));
        let starts_postfix_expression = matches!(
            follower,
            TokenKind::OpenBrac | TokenKind::Dot | TokenKind::OpenSquare
        );
        let starts_cast = follower == TokenKind::As;
        let starts_struct_literal = allow_struct_lit && follower == TokenKind::OpenBlock;
        let ends_expression = matches!(
            follower,
            TokenKind::SemiColon
                | TokenKind::Comma
                | TokenKind::CloseBrac
                | TokenKind::CloseSquare
                | TokenKind::CloseBlock
                | TokenKind::Eof
        );
        let has_valid_follower = is_unambiguous_binary_operator
            || starts_postfix_expression
            || starts_cast
            || starts_struct_literal
            || ends_expression;
        if args.is_empty() || !errors.is_empty() || !has_valid_follower {
            return None;
        }

        self.tokens = probe.tokens;
        Some(args)
    }

    fn unexpected(&mut self, expected: impl Display) {
        let token = self.token();
        report_unexpected_parsing(self.errors, token.pos, expected, token);
    }

    fn token(&self) -> Token {
        self.tokens[0].clone()
    }

    fn kind(&self) -> &TokenKind {
        &self.tokens[0].kind
    }

    fn pop(&mut self) -> Token {
        if self.kind() == &TokenKind::Eof {
            self.token()
        } else {
            self.tokens.pop_front().unwrap()
        }
    }

    fn take(&mut self, kind: TokenKind) -> Option<Token> {
        if kind == TokenKind::Gt {
            self.split_greater_than();
        }
        if kind == TokenKind::Mul {
            self.split_mul_assign();
        }

        let token = self.token();
        if token.kind == kind {
            Some(self.pop())
        } else {
            report_unexpected_parsing(self.errors, token.pos, kind, token);
            None
        }
    }

    fn take_ident(&mut self) -> Option<Identifier> {
        let token = self.token();
        if let TokenKind::Ident(..) = token.kind {
            let token = self.pop();
            let TokenKind::Ident(name) = token.kind else {
                unreachable!();
            };
            Some(Identifier {
                value: name,
                pos: token.pos,
            })
        } else {
            report_unexpected_parsing(self.errors, token.pos, "IDENT", token);
            None
        }
    }

    fn take_string_lit(&mut self) -> Option<StringLit> {
        let token = self.token();
        if let TokenKind::StringLit { .. } = token.kind {
            let token = self.pop();
            let TokenKind::StringLit { raw, value } = token.kind else {
                unreachable!();
            };
            Some(StringLit {
                raw,
                value,
                pos: token.pos,
            })
        } else {
            report_unexpected_parsing(self.errors, token.pos, "STRING_LIT", token);
            None
        }
    }

    fn take_number_lit(&mut self) -> Option<NumberLit> {
        let token = self.token();
        if let TokenKind::NumberLit { .. } = token.kind {
            let token = self.pop();
            let TokenKind::NumberLit { raw, value } = token.kind else {
                unreachable!();
            };
            Some(NumberLit {
                raw,
                value,
                pos: token.pos,
            })
        } else {
            report_unexpected_parsing(self.errors, token.pos, "NUMBER_LIT", token);
            None
        }
    }

    fn take_char_lit(&mut self) -> Option<CharLit> {
        let token = self.token();
        if let TokenKind::CharLit { .. } = token.kind {
            let token = self.pop();
            let TokenKind::CharLit { raw, value } = token.kind else {
                unreachable!();
            };
            Some(CharLit {
                raw,
                value,
                pos: token.pos,
            })
        } else {
            report_unexpected_parsing(self.errors, token.pos, "CHAR_LIT", token);
            None
        }
    }

    fn take_if(&mut self, kind: &TokenKind) -> Option<Token> {
        if kind == &TokenKind::Gt {
            self.split_greater_than();
        }
        if kind == &TokenKind::Mul {
            self.split_mul_assign();
        }

        if self.kind() == kind {
            Some(self.pop())
        } else {
            None
        }
    }

    fn take_if_ident(&mut self) -> Option<Identifier> {
        let tok = self.tokens.front()?;
        if let TokenKind::Ident(..) = tok.kind {
            let tok = self.tokens.pop_front().unwrap();
            let TokenKind::Ident(value) = tok.kind else {
                unreachable!();
            };
            Some(Identifier {
                value,
                pos: tok.pos,
            })
        } else {
            None
        }
    }

    fn take_if_string_lit(&mut self) -> Option<StringLit> {
        let tok = self.tokens.front()?;
        if let TokenKind::StringLit { .. } = tok.kind {
            let tok = self.tokens.pop_front().unwrap();
            let TokenKind::StringLit { raw, value } = tok.kind else {
                unreachable!();
            };
            Some(StringLit {
                raw,
                value,
                pos: tok.pos,
            })
        } else {
            None
        }
    }

    fn split_greater_than(&mut self) {
        let tok = self.tokens.front();
        if tok.map_or(false, |token| token.kind == TokenKind::ShiftRight) {
            let tok = self.tokens.pop_front().unwrap();
            let pos = tok.pos;

            let remaining_kind = match self.tokens.front() {
                Some(token) if token.spacing && token.kind == TokenKind::Gt => {
                    Some(TokenKind::ShiftRight)
                }
                Some(token) if token.spacing && token.kind == TokenKind::GEq => {
                    Some(TokenKind::AssignOp(BinaryOp::ShiftRight))
                }
                _ => None,
            };
            let remaining_kind = if let Some(kind) = remaining_kind {
                self.tokens.pop_front();
                kind
            } else {
                TokenKind::Gt
            };
            self.tokens.push_front(Token {
                kind: remaining_kind,
                pos: Pos {
                    col: pos.col + 1,
                    ..pos
                },
                spacing: true,
            });
            self.tokens.push_front(Token {
                kind: TokenKind::Gt,
                pos,
                spacing: tok.spacing,
            });
        } else if tok.is_some_and(|token| token.kind == TokenKind::AssignOp(BinaryOp::ShiftRight)) {
            let tok = self.tokens.pop_front().unwrap();
            let pos = tok.pos;
            self.tokens.push_front(Token {
                kind: TokenKind::GEq,
                pos: Pos {
                    col: pos.col + 1,
                    ..pos
                },
                spacing: true,
            });
            self.tokens.push_front(Token {
                kind: TokenKind::Gt,
                pos,
                spacing: tok.spacing,
            });
        } else if tok.is_some_and(|token| token.kind == TokenKind::GEq) {
            let tok = self.tokens.pop_front().unwrap();
            let pos = tok.pos;
            let equal_pos = Pos {
                col: pos.col + 1,
                ..pos
            };

            let joins_next_equal = self
                .tokens
                .front()
                .is_some_and(|token| token.kind == TokenKind::Equal && token.spacing);
            if joins_next_equal {
                self.tokens.pop_front();
                self.tokens.push_front(Token {
                    kind: TokenKind::Eq,
                    pos: equal_pos,
                    spacing: true,
                });
            } else {
                self.tokens.push_front(Token {
                    kind: TokenKind::Equal,
                    pos: equal_pos,
                    spacing: true,
                });
            }
            self.tokens.push_front(Token {
                kind: TokenKind::Gt,
                pos,
                spacing: tok.spacing,
            });
        }
    }

    fn split_mul_assign(&mut self) {
        let tok = self.tokens.front();
        if !tok.is_some_and(|token| token.kind == TokenKind::AssignOp(BinaryOp::Mul)) {
            return;
        }
        let tok = self.tokens.pop_front().unwrap();
        let pos = tok.pos;
        let equal_pos = Pos {
            col: pos.col + 1,
            ..pos
        };
        let joins_next_equal = self
            .tokens
            .front()
            .is_some_and(|token| token.kind == TokenKind::Equal && token.spacing);
        if joins_next_equal {
            self.tokens.pop_front();
            self.tokens.push_front(Token {
                kind: TokenKind::Eq,
                pos: equal_pos,
                spacing: true,
            });
        } else {
            self.tokens.push_front(Token {
                kind: TokenKind::Equal,
                pos: equal_pos,
                spacing: true,
            });
        }
        self.tokens.push_front(Token {
            kind: TokenKind::Mul,
            pos,
            spacing: tok.spacing,
        });
    }

    fn skip_until_before(&mut self, kinds: &[TokenKind]) {
        self.skip_until_before_matching(|kind| kinds.contains(kind));
    }

    fn skip_until_before_matching(&mut self, matches: impl Fn(&TokenKind) -> bool) {
        while self.kind() != &TokenKind::Eof && !matches(self.kind()) {
            self.tokens.pop_front();
        }
    }
}

fn report_unexpected_parsing(
    errors: &ErrorManager,
    pos: Pos,
    expected: impl Display,
    found: impl Display,
) {
    errors.report(pos, format!("Expected {expected}, but found {found}"));
}

fn report_missing(errors: &ErrorManager, pos: Pos, component: impl Display) {
    errors.report(pos, format!("Missing {component}"));
}

fn report_unexpected_token(errors: &ErrorManager, pos: Pos, kind: TokenKind) {
    errors.report(pos, format!("Unexpected token {kind}"));
}

fn report_dangling_annotations(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("There is no object to annotate"));
}

fn report_invalid_struct_literal_target(errors: &ErrorManager, pos: Pos) {
    errors.report(
        pos,
        String::from("Struct literal target must be a type expression"),
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::FileManager;

    #[test]
    fn consuming_or_skipping_at_eof_preserves_the_token() {
        let mut files = FileManager::default();
        let file = files
            .add_file("eof.mg".into(), "let value = 1;\n  ".into())
            .unwrap();
        let errors = ErrorManager::default();
        let tokens = scan(&errors, &file);
        let eof = tokens.last().unwrap().clone();
        let mut parser = FileParser::new(&errors, tokens.into());
        parser.skip_until_before(&[TokenKind::SemiColon]);
        assert!(parser.take_if(&TokenKind::SemiColon).is_some());

        for _ in 0..2 {
            assert_eq!(parser.kind(), &TokenKind::Eof);
            assert_eq!(parser.pop(), eof);
            assert_eq!(parser.take(TokenKind::Eof), Some(eof.clone()));
            assert_eq!(parser.take_if(&TokenKind::Eof), Some(eof.clone()));
            parser.skip_until_before(&[TokenKind::SemiColon]);
            parser.skip_until_before_matching(|_| false);
            assert_eq!(parser.tokens.len(), 1);
            assert_eq!(parser.token(), eof);
        }
        assert!(errors.is_empty());
    }

    #[test]
    fn recovery_leaves_eof_available_for_diagnostics() {
        let mut files = FileManager::default();
        for source in [
            "unexpected",
            "fn f() { if",
            "fn f() { let",
            "fn f() { ()",
            "fn f() { value as",
            "let x: Pair<i32",
            "let x: i32 = f<i32",
        ] {
            let file = files
                .add_file("recovery.mg".into(), format!("{source}\n  "))
                .unwrap();
            let errors = ErrorManager::default();
            let tokens = scan(&errors, &file);
            let eof = tokens.last().unwrap().clone();
            let mut parser = FileParser::new(&errors, tokens.into());
            parse_root(&mut parser);
            assert!(!errors.is_empty(), "{source}");
            assert_eq!(parser.tokens.len(), 1, "{source}");
            assert_eq!(parser.token(), eof, "{source}");
        }
    }

    #[test]
    fn split_operators_preserve_file_line_and_code_point_column() {
        let mut files = FileManager::default();
        for (source, kinds, columns) in [
            (">>", vec![TokenKind::Gt, TokenKind::Gt], vec![3, 4]),
            (
                ">>=",
                vec![TokenKind::Gt, TokenKind::Gt, TokenKind::Equal],
                vec![3, 4, 5],
            ),
            (
                ">>>",
                vec![TokenKind::Gt, TokenKind::ShiftRight],
                vec![3, 4],
            ),
            (
                ">>>=",
                vec![TokenKind::Gt, TokenKind::AssignOp(BinaryOp::ShiftRight)],
                vec![3, 4],
            ),
            (">==", vec![TokenKind::Gt, TokenKind::Eq], vec![3, 4]),
            (
                ">>==",
                vec![TokenKind::Gt, TokenKind::Gt, TokenKind::Eq],
                vec![3, 4, 5],
            ),
            ("*=", vec![TokenKind::Mul, TokenKind::Equal], vec![3, 4]),
            ("*==", vec![TokenKind::Mul, TokenKind::Eq], vec![3, 4]),
        ] {
            let file = files
                .add_file("operators.mg".into(), format!("\nα {source}"))
                .unwrap();
            let errors = ErrorManager::default();
            let tokens = scan(&errors, &file);
            let eof = tokens.last().unwrap().clone();
            let mut parser = FileParser::new(&errors, tokens.into());
            assert_eq!(parser.take_ident().unwrap().value, "α");
            for (kind, col) in kinds.into_iter().zip(columns) {
                let token = parser.take(kind).expect(source);
                assert_eq!(
                    token.pos,
                    Pos {
                        file: file.id,
                        line: 2,
                        col
                    }
                );
            }
            assert_eq!(parser.kind(), &TokenKind::Eof);
            assert_eq!(parser.token(), eof);
            assert!(errors.is_empty());
        }
    }
}
