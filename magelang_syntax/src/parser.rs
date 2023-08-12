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

    let items = parser.parse_root();
    PackageNode { items, comments }
}

struct FileParser<'a, Error> {
    errors: &'a Error,
    tokens: VecDeque<Token>,
    last_pos: Pos,
}

impl<'a, Error: ErrorReporter> FileParser<'a, Error> {
    fn new(errors: &'a Error, tokens: VecDeque<Token>, last_pos: Pos) -> Self {
        Self {
            errors,
            tokens,
            last_pos,
        }
    }

    fn parse_root(&mut self) -> Vec<ItemNode> {
        let mut items = Vec::<ItemNode>::default();

        while !self.tokens.is_empty() {
            if let Some(item) = self.parse_item_node() {
                items.push(item);
            }
        }

        items
    }

    fn parse_item_node(&mut self) -> Option<ItemNode> {
        let mut annotations = Vec::default();
        while let Some(annotation) = self.parse_annotation() {
            annotations.push(annotation);
        }

        let tok = self.token();
        if tok.kind == TokenKind::Eof {
            return None;
        }

        let item = match &tok.kind {
            TokenKind::Let => self.parse_global(),
            TokenKind::Fn => self.parse_func(annotations),
            TokenKind::Import => self.parse_import().map(ItemNode::Import),
            TokenKind::Struct => self.parse_struct(),
            _ => None,
        };

        if item.is_none() {
            let stopping_token = &[
                TokenKind::Fn,
                TokenKind::SemiColon,
                TokenKind::Import,
                TokenKind::Let,
            ];
            self.skip_until_before(stopping_token);
            self.take_if(TokenKind::SemiColon);
            self.errors
                .unexpected_parsing(tok.pos, "declaration", "invalid syntax");
        }

        item
    }

    fn parse_annotation(&mut self) -> Option<AnnotationNode> {
        let at_sign = self.take_if(TokenKind::AtSign)?;
        let pos = at_sign.pos;
        let name = self.take(TokenKind::Ident)?;
        let (_, arguments, _) = self.parse_sequence(
            TokenKind::OpenBrac,
            TokenKind::Comma,
            TokenKind::CloseBrac,
            |this| this.take_if(TokenKind::StringLit),
        )?;
        Some(AnnotationNode {
            pos,
            name,
            arguments,
        })
    }

    fn parse_import(&mut self) -> Option<ImportNode> {
        let import_tok = self.take(TokenKind::Import)?;
        let pos = import_tok.pos;
        let name = self.take(TokenKind::Ident)?;
        let path = self.take(TokenKind::StringLit)?;
        self.take(TokenKind::SemiColon)?;
        Some(ImportNode { pos, name, path })
    }

    fn parse_global(&mut self) -> Option<ItemNode> {
        let let_tok = self.take(TokenKind::Let)?;
        let pos = let_tok.pos;

        let name = self.take(TokenKind::Ident)?;
        self.take(TokenKind::Colon)?;
        let ty = self.parse_expr(true)?;
        let value = if self.take_if(TokenKind::Equal).is_some() {
            self.parse_expr(true)
        } else {
            None
        };
        self.take(TokenKind::SemiColon)?;

        Some(ItemNode::Global(GlobalNode {
            pos,
            name,
            ty,
            value,
        }))
    }

    fn parse_struct(&mut self) -> Option<ItemNode> {
        let struct_tok = self.take(TokenKind::Struct)?;
        let pos = struct_tok.pos;

        let name = self.take(TokenKind::Ident)?;

        let type_params = if self.kind() == TokenKind::OpenSquare {
            let (_, type_params, _) = self.parse_sequence(
                TokenKind::OpenSquare,
                TokenKind::Comma,
                TokenKind::CloseSquare,
                |parser| parser.take(TokenKind::Ident),
            )?;
            type_params
                .into_iter()
                .map(TypeParameterNode::from)
                .collect()
        } else {
            Vec::default()
        };

        let (_, fields, _) = self.parse_sequence(
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

        Some(ItemNode::Struct(StructNode {
            pos,
            name,
            type_params,
            fields,
        }))
    }

    fn parse_func(&mut self, annotations: Vec<AnnotationNode>) -> Option<ItemNode> {
        let signature = self.parse_signature(annotations)?;
        let pos = signature.pos;
        if self.take_if(TokenKind::SemiColon).is_some() {
            return Some(ItemNode::Function(FunctionNode {
                pos,
                signature,
                body: None,
            }));
        }

        let body = self.parse_block_stmt()?;
        Some(ItemNode::Function(FunctionNode {
            pos,
            signature,
            body: Some(body),
        }))
    }

    fn parse_signature(&mut self, annotations: Vec<AnnotationNode>) -> Option<SignatureNode> {
        let func = self.take(TokenKind::Fn)?;
        let pos = func.pos;
        let name = self.take(TokenKind::Ident)?;

        let type_params = if self.kind() == TokenKind::OpenSquare {
            let (_, type_parameters, _) = self.parse_sequence(
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

        let (_, parameters, _) = self.parse_sequence(
            TokenKind::OpenBrac,
            TokenKind::Comma,
            TokenKind::CloseBrac,
            Self::parse_parameter,
        )?;
        let return_type = if self.take_if(TokenKind::Colon).is_some() {
            let Some(expr) = self.parse_expr(false) else {
                self.errors
                    .unexpected_parsing(pos, "type expression", "nothing");
                return None;
            };
            Some(expr)
        } else {
            None
        };

        Some(SignatureNode {
            pos,
            annotations,
            name,
            type_params,
            parameters,
            return_type,
        })
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

    fn parse_parameter(&mut self) -> Option<ParameterNode> {
        let name = self.take(TokenKind::Ident)?;
        let pos = name.pos;
        let _ = self.take(TokenKind::Colon)?;
        let Some(ty) = self.parse_expr(false) else {
            self.errors
                .unexpected_parsing(pos, "type expression", "nothing");
            return None;
        };
        Some(ParameterNode { pos, name, ty })
    }

    fn parse_stmt(&mut self) -> Option<StatementNode> {
        Some(match self.kind() {
            TokenKind::Let => StatementNode::Let(self.parse_let_stmt()?),
            TokenKind::If => StatementNode::If(self.parse_if_stmt()?),
            TokenKind::While => StatementNode::While(self.parse_while_stmt()?),
            TokenKind::OpenBlock => StatementNode::Block(self.parse_block_stmt()?),
            TokenKind::Continue => StatementNode::Continue(self.take(TokenKind::Continue).unwrap()),
            TokenKind::Break => StatementNode::Break(self.take(TokenKind::Break).unwrap()),
            TokenKind::Return => StatementNode::Return(self.parse_return_stmt()?),
            _ => {
                let expr = self.parse_expr(true)?;
                let pos = expr.pos();
                if self.take_if(TokenKind::Equal).is_some() {
                    let value = self.parse_expr(true)?;
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

    fn parse_let_stmt(&mut self) -> Option<LetStatementNode> {
        let let_tok = self.take(TokenKind::Let)?;
        let pos = let_tok.pos;

        let name = self.take(TokenKind::Ident)?;

        if self.take_if(TokenKind::Colon).is_some() {
            let ty = self.parse_expr(true)?;
            if self.take_if(TokenKind::Equal).is_some() {
                let value = self.parse_expr(true)?;
                self.take(TokenKind::SemiColon)?;
                Some(LetStatementNode {
                    pos,
                    name,
                    kind: LetKind::TypeValue { ty, value },
                })
            } else {
                self.take(TokenKind::SemiColon)?;
                Some(LetStatementNode {
                    pos,
                    name,
                    kind: LetKind::TypeOnly { ty },
                })
            }
        } else {
            self.take(TokenKind::Equal)?;
            let value = self.parse_expr(true)?;
            self.take(TokenKind::SemiColon)?;
            Some(LetStatementNode {
                pos,
                name,
                kind: LetKind::ValueOnly { value },
            })
        }
    }

    fn parse_if_stmt(&mut self) -> Option<IfStatementNode> {
        let if_tok = self.take(TokenKind::If)?;
        let pos = if_tok.pos;

        let condition = self.parse_expr(false)?;
        let body = self.parse_block_stmt()?;

        let else_node = self.take_if(TokenKind::Else).and_then(|_| {
            let stmt = if self.kind() == TokenKind::If {
                let else_if = self.parse_if_stmt()?;
                StatementNode::If(else_if)
            } else {
                let else_body = self.parse_block_stmt()?;
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

    fn parse_while_stmt(&mut self) -> Option<WhileStatementNode> {
        let while_tok = self.take(TokenKind::While)?;
        let pos = while_tok.pos;

        let condition = self.parse_expr(false)?;
        let body = self.parse_block_stmt()?;

        Some(WhileStatementNode {
            pos,
            condition,
            body,
        })
    }

    fn parse_block_stmt(&mut self) -> Option<BlockStatementNode> {
        let open = self.take(TokenKind::OpenBlock)?;
        let pos = open.pos;
        let mut statements = vec![];
        loop {
            let tok = self.token();
            if tok.kind == TokenKind::Eof || tok.kind == TokenKind::CloseBlock {
                break;
            }

            if let Some(stmt) = self.parse_stmt() {
                statements.push(stmt);
            } else {
                self.skip_until_before(&[TokenKind::SemiColon]);
                self.tokens.pop_front();
            }
        }
        self.take(TokenKind::CloseBlock)?;
        Some(BlockStatementNode { pos, statements })
    }

    fn parse_return_stmt(&mut self) -> Option<ReturnStatementNode> {
        let return_tok = self.take(TokenKind::Return)?;
        let pos = return_tok.pos;
        if self.take_if(TokenKind::SemiColon).is_some() {
            return Some(ReturnStatementNode { pos, value: None });
        }

        let Some(value) = self.parse_expr(true) else {
            self.errors
                .unexpected_parsing(pos, "type expression", "nothing");
            return None;
        };

        self.take(TokenKind::SemiColon)?;
        Some(ReturnStatementNode {
            pos,
            value: Some(value),
        })
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

    fn take(&mut self, kind: TokenKind) -> Option<Token> {
        let token = self.tokens.pop_front()?;
        if token.kind == kind {
            Some(token)
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

    fn unexpected_token(&self, pos: Pos, kind: TokenKind) {
        self.report(pos, format!("Expected token {kind}"));
    }
}

impl<T> ParsingError for T where T: ErrorReporter {}
