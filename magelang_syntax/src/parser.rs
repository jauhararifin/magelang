use crate::ast::{
    AstNode, BlockStatementNode, CallExprNode, ExprNode, FunctionNode, ImportNode, ItemNode, PackageNode,
    ParameterNode, ReturnStatementNode, SignatureNode, StatementNode,
};
use crate::scanner::scan;
use crate::errors::unexpected_parsing;
use crate::tokens::{Token, TokenKind};
use indexmap::IndexMap;
use magelang_common::{ErrorAccumulator, FileId, FileInfo, Span, SymbolId, SymbolLoader};
use std::collections::VecDeque;
use std::rc::Rc;

pub(crate) fn parse(
    err_channel: &ErrorAccumulator,
    symbol_loader: &SymbolLoader,
    file_info: &FileInfo,
) -> Rc<PackageNode> {
    let tokens = scan(err_channel, &file_info);
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
    last_offset: usize,
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
        let mut span = Span::new(self.file_id, 0, 0);

        loop {
            if self.kind() == TokenKind::Eof {
                break;
            }
            if let Some(item) = self.parse_item_node() {
                span.union(&item.get_span());
                let name = item.name();
                let name = self.symbol_loader.declare_symbol(name);
                items.entry(name).or_default().push(item);
            }
        }

        PackageNode {
            span,
            items,
            comments: self.comments,
        }
    }

    fn parse_item_node(&mut self) -> Option<ItemNode> {
        let tok = self.token();
        if tok.kind == TokenKind::Eof {
            return None;
        }

        let mut span = tok.span.clone();

        let item = match &tok.kind {
            TokenKind::Fn => self.parse_func(),
            TokenKind::Import => self.parse_import().map(ItemNode::Import),
            _ => None,
        };

        if item.is_none() {
            let stopping_token = &[TokenKind::Fn, TokenKind::SemiColon, TokenKind::Import];
            let mut tokens = self.skip_until_before(stopping_token);
            if let Some(tok) = self.take_if(TokenKind::SemiColon) {
                tokens.push(tok);
            }
            for tok in &tokens {
                span.union(&tok.span);
            }
            self.err_channel
                .push(unexpected_parsing(span, "declaration", "invalid syntax"));
        }

        item
    }

    fn parse_import(&mut self) -> Option<ImportNode> {
        let import_tok = self.take(TokenKind::Import)?;
        let mut span = import_tok.span;
        let name = self.take(TokenKind::Ident)?;
        let path = self.take(TokenKind::StringLit)?;
        let semicolon = self.take(TokenKind::SemiColon)?;
        span.union(&semicolon.span);
        Some(ImportNode { span, name, path })
    }

    fn parse_func(&mut self) -> Option<ItemNode> {
        let signature = self.parse_signature()?;
        let mut span = signature.get_span();
        if let Some(_) = self.take_if(TokenKind::SemiColon) {
            return Some(ItemNode::NativeFunction(signature));
        }

        let body = self.parse_block_stmt()?;
        span.union(&body.get_span());
        Some(ItemNode::Function(FunctionNode { span, signature, body }))
    }

    fn parse_signature(&mut self) -> Option<SignatureNode> {
        let func = self.take(TokenKind::Fn)?;
        let mut span = func.span;
        let name = self.take(TokenKind::Ident)?;
        let (_, parameters, close_brac) = self.parse_sequence(
            TokenKind::OpenBrac,
            TokenKind::Comma,
            TokenKind::CloseBrac,
            Self::parse_parameter,
        )?;
        span.union(&close_brac.span);
        let return_type = if let Some(_) = self.take_if(TokenKind::Colon) {
            let Some(expr) = self.parse_expr() else {
                self.err_channel.push(unexpected_parsing(
                    span,
                    "type expression",
                    "nothing",
                ));
                return None;
            };
            span.union(&expr.get_span());
            Some(expr)
        } else {
            None
        };

        Some(SignatureNode {
            span,
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
        let mut span = name.span.clone();
        let _ = self.take(TokenKind::Colon)?;
        let Some(type_expr) = self.parse_expr() else {
            self.err_channel.push(unexpected_parsing(
                span,
                "type expression",
                "nothing",
            ));
            return None;
        };
        span.union(&type_expr.get_span());
        Some(ParameterNode { span, name, type_expr })
    }

    fn parse_stmt(&mut self) -> Option<StatementNode> {
        Some(match self.kind() {
            TokenKind::OpenBlock => StatementNode::Block(self.parse_block_stmt()?),
            TokenKind::Return => StatementNode::Return(self.parse_return_stmt()?),
            _ => StatementNode::Expr(self.parse_expr()?),
        })
    }

    fn parse_block_stmt(&mut self) -> Option<BlockStatementNode> {
        let open = self.take(TokenKind::OpenBlock)?;
        let mut span = open.span;
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
        let close = self.take(TokenKind::CloseBlock)?;
        span.union(&close.span);
        Some(BlockStatementNode { span, statements })
    }

    fn parse_return_stmt(&mut self) -> Option<ReturnStatementNode> {
        let return_tok = self.take(TokenKind::Return)?;
        let mut span = return_tok.span;
        if let Some(semicolon_tok) = self.take_if(TokenKind::SemiColon) {
            span.union(&semicolon_tok.span);
            return Some(ReturnStatementNode { span, value: None });
        }

        let Some(value) = self.parse_expr() else {
            self.err_channel.push(unexpected_parsing(
                span,
                "type expression",
                "nothing",
            ));
            return None;
        };

        let semicolon_tok = self.take(TokenKind::SemiColon)?;
        span.union(&semicolon_tok.span);
        Some(ReturnStatementNode {
            span,
            value: Some(value),
        })
    }

    fn parse_expr(&mut self) -> Option<ExprNode> {
        self.parse_call_expr()
    }

    fn parse_call_expr(&mut self) -> Option<ExprNode> {
        let target = self.parse_primary_expr()?;

        if self.kind() != TokenKind::OpenBrac {
            return Some(target);
        }

        let (_, arguments, close_brac) =
            self.parse_sequence(TokenKind::OpenBrac, TokenKind::Comma, TokenKind::CloseBrac, |this| {
                this.parse_expr()
            })?;

        let mut span = target.get_span();
        span.union(&close_brac.span);

        Some(ExprNode::Call(CallExprNode {
            span,
            target: Box::new(target),
            arguments,
        }))
    }

    fn parse_primary_expr(&mut self) -> Option<ExprNode> {
        if let Some(ident) = self.take_if(TokenKind::Ident) {
            Some(ExprNode::Ident(ident))
        } else if let Some(expr) = self.take_if(TokenKind::IntegerLit) {
            Some(ExprNode::IntegerLiteral(expr))
        } else {
            None
        }
    }

    fn token(&mut self) -> Token {
        if let Some(tok) = self.tokens.front() {
            tok.clone()
        } else {
            Token {
                kind: TokenKind::Eof,
                value: "".into(),
                span: Span::new(self.file_id, self.last_offset, 0),
            }
        }
    }

    fn kind(&mut self) -> TokenKind {
        self.tokens.front().map(|tok| tok.kind).unwrap_or(TokenKind::Eof)
    }

    fn take(&mut self, kind: TokenKind) -> Option<Token> {
        let token = self.tokens.pop_front()?;
        self.last_offset = token.span.start + token.span.len;
        if token.kind == kind {
            Some(token)
        } else {
            self.err_channel
                .push(unexpected_parsing(token.span.clone(), kind, token.kind));
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
            self.last_offset = tok.span.start + tok.span.len;
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
                self.last_offset = tok.span.start + tok.span.len;
                tokens.push(tok);
            } else {
                break;
            }
        }
        tokens
    }
}

pub fn parse_string_lit(s: &str) -> &str {
    s[1..s.len() - 1].into()
}
