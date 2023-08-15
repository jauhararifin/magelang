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
        f.parse_expr(true)
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
                TypeExprNode::Invalid
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
                TypeExprNode::Invalid
            };
            Some(TypeExprNode::Ptr(PtrTypeNode {
                pos: tok.pos,
                ty: Box::new(ty),
            }))
        }
        TokenKind::OpenBrac => {
            f.pop();
            let inner_ty = parse_type_expr(f);
            f.take(TokenKind::CloseBrac);
            inner_ty.map(Box::new).map(TypeExprNode::Grouped)
        }
        TokenKind::Ident => {
            let name = f.take(TokenKind::Ident).unwrap();
            let named_type = if f.take_if(TokenKind::Dot).is_some() {
                let package = name;
                let name = f.take(TokenKind::Ident);
                if let Some(name) = name {
                    Some(NamedTypeNode::Selection(package, name))
                } else {
                    None
                }
            } else {
                Some(NamedTypeNode::Ident(name))
            };

            let type_args = if f.kind() == TokenKind::Lt {
                parse_sequence(
                    f,
                    TokenKind::Lt,
                    TokenKind::Comma,
                    TokenKind::Gt,
                    parse_type_expr,
                )
                .map(|(_, type_args, _)| type_args)
            } else {
                None
            };

            let Some(ty) = named_type else {
                return Some(TypeExprNode::Invalid);
            };
            let Some(type_args) = type_args else {
                return Some(TypeExprNode::Invalid);
            };

            if type_args.is_empty() {
                Some(TypeExprNode::Named(ty))
            } else {
                Some(TypeExprNode::Instance(TypeInstanceNode {
                    ty,
                    args: type_args,
                }))
            }
        }
        _ => None,
    }
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

    let (_, fields, _) = parse_sequence(
        f,
        TokenKind::OpenBlock,
        TokenKind::Comma,
        TokenKind::CloseBlock,
        |parser| {
            let name = parser.take(TokenKind::Ident)?;
            parser.take(TokenKind::Colon)?;
            let ty = parser.parse_expr(true)?;
            Some(StructFieldNode {
                pos: name.pos,
                name,
                ty,
            })
        },
    )?;

    Some(StructNode {
        pos,
        annotations,
        name: name?,
        type_params: type_params?,
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

    let type_params = if f.kind() == TokenKind::OpenSquare {
        let (_, type_parameters, _) = parse_sequence(
            f,
            TokenKind::OpenSquare,
            TokenKind::Comma,
            TokenKind::CloseSquare,
            |parser| parser.take(TokenKind::Ident),
        )?;
        type_parameters
            .into_iter()
            .map(TypeParameterNode::from)
            .collect()
    } else {
        Vec::default()
    };

    let (_, parameters, close_brac) = parse_sequence(
        f,
        TokenKind::OpenBrac,
        TokenKind::Comma,
        TokenKind::CloseBrac,
        parse_parameter,
    )?;
    let return_type = if f.take_if(TokenKind::Colon).is_some() {
        let Some(expr) = f.parse_expr(false) else {
            f.errors
                .unexpected_parsing(pos, "type expression", "nothing");
            return None;
        };
        Some(expr)
    } else {
        None
    };

    let end_pos = return_type
        .as_ref()
        .map(|expr| expr.pos())
        .unwrap_or(close_brac.pos);

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
    let _ = f.take(TokenKind::Colon)?;
    let ty = f.parse_expr(false)?;
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
            let expr = f.parse_expr(true)?;
            let pos = expr.pos();
            if f.take_if(TokenKind::Equal).is_some() {
                let value = f.parse_expr(true)?;
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
        let ty = f.parse_expr(true)?;
        if f.take_if(TokenKind::Equal).is_some() {
            let value = f.parse_expr(true)?;
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
        let value = f.parse_expr(true)?;
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

    let condition = f.parse_expr(false)?;
    let body = parse_block_stmt(f)?;

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

    let condition = f.parse_expr(false)?;
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

    let Some(value) = f.parse_expr(true) else {
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

impl<'a, Error: ErrorReporter> FileParser<'a, Error> {
    fn new(errors: &'a Error, tokens: VecDeque<Token>, last_pos: Pos) -> Self {
        Self {
            errors,
            tokens,
            last_pos,
        }
    }

    fn parse_sequence<T, F>(
        &mut self,
        begin_tok: TokenKind,
        delim_tok: TokenKind,
        end_tok: TokenKind,
        parse_fn: F,
    ) -> Option<(Token, Vec<T>, Token)>
    where
        F: Fn(&mut Self) -> Option<T>,
    {
        let opening = self.take(begin_tok)?;

        let mut items = Vec::<T>::default();
        while self.kind() != end_tok && self.kind() != TokenKind::Eof {
            if let Some(item) = parse_fn(self) {
                items.push(item);
            }
            self.take_if(delim_tok);
        }

        let closing = self.take(end_tok)?;

        Some((opening, items, closing))
    }

    fn parse_expr(&mut self, allow_struct_lit: bool) -> Option<ExprNode> {
        self.parse_binary_expr(TokenKind::Or, allow_struct_lit)
    }

    const BINOP_PRECEDENCE: &[TokenKind] = &[
        TokenKind::Or,
        TokenKind::And,
        TokenKind::BitOr,
        TokenKind::BitXor,
        TokenKind::BitAnd,
        TokenKind::Eq,
        TokenKind::NEq,
        TokenKind::Lt,
        TokenKind::LEq,
        TokenKind::Gt,
        TokenKind::GEq,
        TokenKind::ShiftLeft,
        TokenKind::ShiftRight,
        TokenKind::Add,
        TokenKind::Sub,
        TokenKind::Mul,
        TokenKind::Div,
        TokenKind::Mod,
    ];

    fn parse_binary_expr(&mut self, op: TokenKind, allow_struct_lit: bool) -> Option<ExprNode> {
        let next_op = Self::BINOP_PRECEDENCE
            .iter()
            .skip_while(|p| *p != &op)
            .nth(1);

        let a = if let Some(next_op) = next_op {
            self.parse_binary_expr(*next_op, allow_struct_lit)?
        } else {
            self.parse_cast_expr(allow_struct_lit)?
        };

        let mut result = a;
        while let Some(op_token) = self.take_if(op) {
            let b = if let Some(next_op) = next_op {
                self.parse_binary_expr(*next_op, allow_struct_lit)?
            } else {
                self.parse_cast_expr(allow_struct_lit)?
            };
            result = ExprNode::Binary(BinaryExprNode {
                a: Box::new(result),
                op: op_token,
                b: Box::new(b),
            });
        }

        Some(result)
    }

    fn parse_cast_expr(&mut self, allow_struct_lit: bool) -> Option<ExprNode> {
        let value = self.parse_unary_expr(allow_struct_lit)?;
        if self.take_if(TokenKind::As).is_some() {
            let target = self.parse_unary_expr(allow_struct_lit)?;
            Some(ExprNode::Cast(CastExprNode {
                value: Box::new(value),
                target: Box::new(target),
            }))
        } else {
            Some(value)
        }
    }

    const UNARY_OP: &[TokenKind] = &[
        TokenKind::Mul,
        TokenKind::BitNot,
        TokenKind::Sub,
        TokenKind::Add,
        TokenKind::Not,
    ];

    fn parse_unary_expr(&mut self, allow_struct_lit: bool) -> Option<ExprNode> {
        let mut ops = vec![];
        while Self::UNARY_OP.contains(&self.kind()) {
            let op = self.tokens.pop_front().unwrap();
            ops.push(op);
        }

        let mut value = self.parse_call_or_selection_expr(allow_struct_lit)?;
        while let Some(op) = ops.pop() {
            if op.kind == TokenKind::Mul {
                value = ExprNode::Deref(DerefExprNode {
                    pos: op.pos,
                    value: Box::new(value),
                })
            } else {
                value = ExprNode::Unary(UnaryExprNode {
                    op,
                    value: Box::new(value),
                })
            }
        }

        Some(value)
    }

    fn parse_call_or_selection_expr(&mut self, allow_struct_lit: bool) -> Option<ExprNode> {
        let mut target = self.parse_primary_expr()?;
        let mut pos = target.pos();

        loop {
            let kind = self.kind();
            target = match kind {
                TokenKind::Dot => {
                    self.take(TokenKind::Dot)?;
                    let selection = self.take(TokenKind::Ident)?;
                    ExprNode::Selection(SelectionExprNode {
                        value: Box::new(target),
                        selection,
                    })
                }
                TokenKind::OpenSquare => {
                    let (_, arguments, _) = self.parse_sequence(
                        TokenKind::OpenSquare,
                        TokenKind::Comma,
                        TokenKind::CloseSquare,
                        |this| this.parse_expr(true),
                    )?;
                    ExprNode::Index(IndexExprNode {
                        value: Box::new(target),
                        indexes: arguments,
                    })
                }
                TokenKind::OpenBrac => {
                    let (_, arguments, _) = self.parse_sequence(
                        TokenKind::OpenBrac,
                        TokenKind::Comma,
                        TokenKind::CloseBrac,
                        |this| this.parse_expr(true),
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

                    let (_, elements, _) = self.parse_sequence(
                        TokenKind::OpenBlock,
                        TokenKind::Comma,
                        TokenKind::CloseBlock,
                        |parser| {
                            let key = parser.take(TokenKind::Ident)?;
                            parser.take(TokenKind::Colon)?;
                            let value = parser.parse_expr(true)?;
                            Some(KeyValue {
                                pos: key.pos,
                                key,
                                value,
                            })
                        },
                    )?;
                    ExprNode::Struct(StructExprNode {
                        pos,
                        target: Box::new(target),
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

    fn parse_primary_expr(&mut self) -> Option<ExprNode> {
        match self.kind() {
            TokenKind::Ident => self.take(TokenKind::Ident).map(ExprNode::Ident),
            TokenKind::IntegerLit => self.take(TokenKind::IntegerLit).map(ExprNode::Integer),
            TokenKind::RealLit => self.take(TokenKind::RealLit).map(ExprNode::Frac),
            TokenKind::StringLit => self.take(TokenKind::StringLit).map(ExprNode::String),
            TokenKind::True => self.take(TokenKind::True).map(ExprNode::Bool),
            TokenKind::False => self.take(TokenKind::False).map(ExprNode::Bool),
            TokenKind::OpenBrac => {
                let _ = self.take(TokenKind::OpenBrac).unwrap();
                let expr = self.parse_expr(true)?;
                let _ = self.take(TokenKind::CloseBrac)?;
                Some(ExprNode::Grouped(Box::new(expr)))
            }
            TokenKind::SemiColon => None,
            _ => {
                let tok = self.token();
                self.errors.unexpected_token(tok.pos, tok.kind);
                None
            }
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
        let token = self.token();
        if token.kind == kind {
            Some(self.pop())
        } else {
            self.errors.unexpected_parsing(token.pos, kind, token.kind);
            None
        }
    }

    fn take_if(&mut self, kind: TokenKind) -> Option<Token> {
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
        self.report(pos, format!("Expected token {kind}"));
    }

    fn dangling_annotations(&self, pos: Pos) {
        self.report(pos, String::from("There is no object to annotate"));
    }
}

impl<T> ParsingError for T where T: ErrorReporter {}
