use crate::ast::{
    AssignStatementNode, AstNode, BinaryExprNode, BlockStatementNode, CallExprNode, CastExprNode, ElseIfStatementNode,
    ExprNode, FunctionNode, GroupedExprNode, IfStatementNode, ImportNode, IndexExprNode, ItemNode, LetKind,
    LetStatementNode, PackageNode, ParameterNode, ReturnStatementNode, SelectionExprNode, SignatureNode, SliceNode,
    StatementNode, TagNode, UnaryExprNode, WhileStatementNode,
};
use crate::errors::{unexpected_parsing, unexpected_token};
use crate::scanner::scan;
use crate::tokens::{Token, TokenKind};
use indexmap::IndexMap;
use magelang_common::{ErrorAccumulator, FileId, FileInfo, Pos, SymbolId, SymbolLoader};
use std::collections::VecDeque;
use std::rc::Rc;

pub(crate) fn parse(
    err_channel: &ErrorAccumulator,
    symbol_loader: &SymbolLoader,
    file_info: &FileInfo,
) -> Rc<PackageNode> {
    let tokens = scan(err_channel, file_info);
    let mut comments = vec![];
    let mut filtered_tokens = vec![];
    for tok in tokens {
        if tok.kind == TokenKind::Comment {
            comments.push(tok);
        } else {
            filtered_tokens.push(tok);
        }
    }

    let tokens = filtered_tokens
        .into_iter()
        .filter(|t| t.kind != TokenKind::Comment)
        .peekable();
    let parser = FileParser::new(err_channel, symbol_loader, file_info.id, tokens);
    let root = parser.parse_root();
    Rc::new(root)
}

struct FileParser<'err, 'sym> {
    err_channel: &'err ErrorAccumulator,
    symbol_loader: &'sym SymbolLoader,

    tokens: VecDeque<Token>,
    comments: Vec<Token>,
    file_id: FileId,
    last_offset: u32,
}

impl<'err, 'sym> FileParser<'err, 'sym> {
    fn new(
        err_channel: &'err ErrorAccumulator,
        symbol_loader: &'sym SymbolLoader,
        file_id: FileId,
        tokens: impl Iterator<Item = Token>,
    ) -> Self {
        let mut comments = vec![];
        let mut filtered_tokens = VecDeque::new();
        for tok in tokens {
            if tok.kind == TokenKind::Comment {
                comments.push(tok.clone());
            } else {
                filtered_tokens.push_back(tok.clone());
            }
        }

        Self {
            err_channel,
            symbol_loader,
            tokens: filtered_tokens,
            comments,
            file_id,
            last_offset: 0,
        }
    }

    fn parse_root(mut self) -> PackageNode {
        let mut items = IndexMap::<SymbolId, Vec<ItemNode>>::new();
        let pos = Pos::new(self.file_id, 0);

        loop {
            if self.kind() == TokenKind::Eof {
                break;
            }
            if let Some(item) = self.parse_item_node() {
                let name = self.symbol_loader.declare_symbol(item.name());
                items.entry(name).or_default().push(item);
            }
        }

        PackageNode {
            pos,
            items,
            comments: self.comments,
        }
    }

    fn parse_item_node(&mut self) -> Option<ItemNode> {
        let mut tags = vec![];
        while let Some(tag) = self.parse_tag() {
            tags.push(tag);
        }

        let tok = self.token();
        if tok.kind == TokenKind::Eof {
            return None;
        }

        let pos = tok.pos;

        let item = match &tok.kind {
            TokenKind::Fn => self.parse_func(tags),
            TokenKind::Import => self.parse_import().map(ItemNode::Import),
            _ => None,
        };

        if item.is_none() {
            let stopping_token = &[TokenKind::Fn, TokenKind::SemiColon, TokenKind::Import];
            let mut tokens = self.skip_until_before(stopping_token);
            if let Some(tok) = self.take_if(TokenKind::SemiColon) {
                tokens.push(tok);
            }
            self.err_channel
                .push(unexpected_parsing(pos, "declaration", "invalid syntax"));
        }

        item
    }

    fn parse_tag(&mut self) -> Option<TagNode> {
        let pound_tag = self.take_if(TokenKind::Pound)?;
        let pos = pound_tag.pos;
        let name = self.take(TokenKind::Ident)?;
        let (_, arguments, _) =
            self.parse_sequence(TokenKind::OpenBrac, TokenKind::Comma, TokenKind::CloseBrac, |this| {
                this.take_if(TokenKind::StringLit)
            })?;
        Some(TagNode { pos, name, arguments })
    }

    fn parse_import(&mut self) -> Option<ImportNode> {
        let import_tok = self.take(TokenKind::Import)?;
        let pos = import_tok.pos;
        let name = self.take(TokenKind::Ident)?;
        let path = self.take(TokenKind::StringLit)?;
        self.take(TokenKind::SemiColon)?;
        Some(ImportNode { pos, name, path })
    }

    fn parse_func(&mut self, tags: Vec<TagNode>) -> Option<ItemNode> {
        let signature = self.parse_signature(tags)?;
        let pos = signature.get_pos();
        if self.take_if(TokenKind::SemiColon).is_some() {
            return Some(ItemNode::NativeFunction(signature));
        }

        let body = self.parse_block_stmt()?;
        Some(ItemNode::Function(FunctionNode { pos, signature, body }))
    }

    fn parse_signature(&mut self, tags: Vec<TagNode>) -> Option<SignatureNode> {
        let func = self.take(TokenKind::Fn)?;
        let pos = if let Some(tag) = tags.first() {
            tag.pos
        } else {
            func.pos
        };
        let name = self.take(TokenKind::Ident)?;
        let (_, parameters, _) = self.parse_sequence(
            TokenKind::OpenBrac,
            TokenKind::Comma,
            TokenKind::CloseBrac,
            Self::parse_parameter,
        )?;
        let return_type = if self.take_if(TokenKind::Colon).is_some() {
            let Some(expr) = self.parse_expr() else {
                self.err_channel.push(unexpected_parsing(
                    pos,
                    "type expression",
                    "nothing",
                ));
                return None;
            };
            Some(expr)
        } else {
            None
        };

        Some(SignatureNode {
            pos,
            tags,
            name,
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

        let mut items = Vec::<T>::new();
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
        let pos = name.pos.clone();
        let _ = self.take(TokenKind::Colon)?;
        let Some(type_expr) = self.parse_expr() else {
            self.err_channel.push(unexpected_parsing(
                pos,
                "type expression",
                "nothing",
            ));
            return None;
        };
        Some(ParameterNode { pos, name, type_expr })
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
                let expr = self.parse_expr()?;
                let pos = expr.get_pos();
                if self.take_if(TokenKind::Equal).is_some() {
                    let value = self.parse_expr()?;
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
            let ty = self.parse_expr()?;
            if self.take_if(TokenKind::Equal).is_some() {
                let value = self.parse_expr()?;
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
            let value = self.parse_expr()?;
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

        let condition = self.parse_expr()?;
        let body = self.parse_block_stmt()?;

        let mut else_ifs = vec![];
        let mut else_body = None;

        while let Some(else_tok) = self.take_if(TokenKind::Else) {
            let else_pos = else_tok.pos.clone();
            if self.take_if(TokenKind::If).is_some() {
                let condition = self.parse_expr()?;
                let body = self.parse_block_stmt()?;
                else_ifs.push(ElseIfStatementNode {
                    pos: else_pos,
                    condition,
                    body,
                });
            } else {
                let body = self.parse_block_stmt()?;
                else_body = Some(body);
                break;
            }
        }

        Some(IfStatementNode {
            pos,
            condition,
            body,
            else_ifs,
            else_body,
        })
    }

    fn parse_while_stmt(&mut self) -> Option<WhileStatementNode> {
        let while_tok = self.take(TokenKind::While)?;
        let pos = while_tok.pos;

        let condition = self.parse_expr()?;
        let body = self.parse_block_stmt()?;

        Some(WhileStatementNode { pos, condition, body })
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

        let Some(value) = self.parse_expr() else {
            self.err_channel.push(unexpected_parsing(
                pos,
                "type expression",
                "nothing",
            ));
            return None;
        };

        self.take(TokenKind::SemiColon)?;
        Some(ReturnStatementNode {
            pos,
            value: Some(value),
        })
    }

    fn parse_expr(&mut self) -> Option<ExprNode> {
        self.parse_binary_expr(TokenKind::Or)
    }

    fn parse_binary_expr(&mut self, op: TokenKind) -> Option<ExprNode> {
        let next_op = BINOP_PRECEDENCE.iter().skip_while(|p| *p != &op).nth(1);

        let a = if let Some(next_op) = next_op {
            self.parse_binary_expr(*next_op)?
        } else {
            self.parse_cast_expr()?
        };

        let mut result = a;
        while let Some(op_token) = self.take_if(op) {
            let b = if let Some(next_op) = next_op {
                self.parse_binary_expr(*next_op)?
            } else {
                self.parse_cast_expr()?
            };
            result = ExprNode::Binary(BinaryExprNode {
                a: Box::new(result),
                op: op_token,
                b: Box::new(b),
            });
        }

        Some(result)
    }

    fn parse_cast_expr(&mut self) -> Option<ExprNode> {
        let value = self.parse_unary_expr()?;
        if self.take_if(TokenKind::As).is_some() {
            let target = self.parse_expr()?;
            Some(ExprNode::Cast(CastExprNode {
                value: Box::new(value),
                target: Box::new(target),
            }))
        } else {
            Some(value)
        }
    }

    fn parse_unary_expr(&mut self) -> Option<ExprNode> {
        if UNARY_OP.contains(&self.kind()) {
            let op = self.tokens.pop_front().unwrap();
            let value = self.parse_unary_expr()?;
            Some(ExprNode::Unary(UnaryExprNode {
                op,
                value: Box::new(value),
            }))
        } else {
            self.parse_index_expr()
        }
    }

    fn parse_index_expr(&mut self) -> Option<ExprNode> {
        let value = self.parse_call_expr()?;
        if self.take_if(TokenKind::OpenSquare).is_none() {
            return Some(value);
        };
        let index = self.parse_expr()?;
        self.take(TokenKind::CloseSquare)?;
        Some(ExprNode::Index(IndexExprNode {
            value: Box::new(value),
            index: Box::new(index),
        }))
    }

    fn parse_call_expr(&mut self) -> Option<ExprNode> {
        let target = self.parse_slice_expr()?;

        if self.kind() != TokenKind::OpenBrac {
            return Some(target);
        }

        let (_, arguments, _) =
            self.parse_sequence(TokenKind::OpenBrac, TokenKind::Comma, TokenKind::CloseBrac, |this| {
                this.parse_expr()
            })?;

        let pos = target.get_pos();

        Some(ExprNode::Call(CallExprNode {
            pos,
            target: Box::new(target),
            arguments,
        }))
    }

    fn parse_slice_expr(&mut self) -> Option<ExprNode> {
        let Some(open_square_tok) = self.take_if(TokenKind::OpenSquare) else {
            return self.parse_selection_expr();
        };
        let pos = open_square_tok.pos;
        self.take(TokenKind::CloseSquare);
        let element = self.parse_selection_expr()?;
        Some(ExprNode::Slice(SliceNode {
            pos,
            element: Box::new(element),
        }))
    }

    fn parse_selection_expr(&mut self) -> Option<ExprNode> {
        let value = self.parse_primary_expr()?;
        if self.take_if(TokenKind::Dot).is_none() {
            return Some(value);
        };
        let selection = self.take_if(TokenKind::IntegerLit);
        if let Some(selection) = selection {
            return Some(ExprNode::Selection(SelectionExprNode {
                value: Box::new(value),
                selection,
            }));
        }

        let selection = self.take(TokenKind::Ident)?;
        Some(ExprNode::Selection(SelectionExprNode {
            value: Box::new(value),
            selection,
        }))
    }

    fn parse_primary_expr(&mut self) -> Option<ExprNode> {
        match self.kind() {
            TokenKind::Ident => self.take(TokenKind::Ident).map(ExprNode::Ident),
            TokenKind::IntegerLit => self.take(TokenKind::IntegerLit).map(ExprNode::IntegerLiteral),
            TokenKind::RealLit => self.take(TokenKind::RealLit).map(ExprNode::RealLiteral),
            TokenKind::StringLit => self.take(TokenKind::StringLit).map(ExprNode::StringLit),
            TokenKind::True => self.take(TokenKind::True).map(ExprNode::BooleanLit),
            TokenKind::False => self.take(TokenKind::False).map(ExprNode::BooleanLit),
            TokenKind::OpenBrac => {
                let _ = self.take(TokenKind::OpenBrac).unwrap();
                let expr = self.parse_expr()?;
                let _ = self.take(TokenKind::CloseBrac)?;
                Some(ExprNode::Grouped(GroupedExprNode { value: Box::new(expr) }))
            }
            TokenKind::SemiColon => None,
            _ => {
                self.err_channel.push(unexpected_token(&self.token()));
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
                pos: Pos::new(self.file_id, self.last_offset),
            }
        }
    }

    fn kind(&mut self) -> TokenKind {
        self.tokens.front().map(|tok| tok.kind).unwrap_or(TokenKind::Eof)
    }

    fn take(&mut self, kind: TokenKind) -> Option<Token> {
        let token = self.tokens.pop_front()?;
        self.last_offset = token.pos.offset + token.value.len() as u32;
        if token.kind == kind {
            Some(token)
        } else {
            self.err_channel
                .push(unexpected_parsing(token.pos.clone(), kind, token.kind));
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
            self.last_offset = tok.pos.offset + tok.value.len() as u32;
            Some(tok)
        } else {
            None
        }
    }

    fn skip_until_before(&mut self, kind: &[TokenKind]) -> Vec<Token> {
        let mut tokens = vec![];
        while let Some(tok) = self.tokens.front() {
            if !kind.contains(&tok.kind) {
                let tok = self.tokens.pop_front().unwrap();
                self.last_offset = tok.pos.offset + tok.value.len() as u32;
                tokens.push(tok);
            } else {
                break;
            }
        }
        tokens
    }
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

const UNARY_OP: &[TokenKind] = &[TokenKind::BitNot, TokenKind::Sub, TokenKind::Add, TokenKind::Not];
