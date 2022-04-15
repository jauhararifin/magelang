use crate::ast::*;
use crate::token::{Error as LexerError, Lexer, Token, TokenKind};
use std::mem::discriminant;

#[derive(Debug)]
pub enum Error {
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: Token,
    },
    Lexer(LexerError),
}

impl From<LexerError> for Error {
    fn from(err: LexerError) -> Self {
        Error::Lexer(err)
    }
}

trait Parser<T: Lexer> {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<ParseResult<T>, Error>;

    fn expect(&self, ctx: &mut Context<T>, kind: TokenKind) -> Result<Token, Error> {
        let token = ctx.lexer.next()?;
        if discriminant(&token.kind) != discriminant(&kind) {
            return Err(Error::UnexpectedToken {
                expected: vec![kind],
                found: token,
            });
        }

        Ok(token)
    }

    fn check(&self, ctx: &mut Context<T>, kind: TokenKind) -> Result<Option<Token>, Error> {
        let token = ctx.lexer.next()?;
        if discriminant(&token.kind) != discriminant(&kind) {
            return Ok(None);
        }

        Ok(Some(token))
    }

    fn expect_one_of(&self, ctx: &mut Context<T>, kind: Vec<TokenKind>) -> Result<Token, Error> {
        let token = ctx.lexer.next()?;

        for k in kind.iter() {
            if discriminant(&token.kind) == discriminant(k) {
                return Ok(token);
            }
        }

        Err(Error::UnexpectedToken {
            expected: kind,
            found: token,
        })
    }
}

struct Context<T: Lexer> {
    lexer: T,
}

enum ParseResult<T: Lexer> {
    Push(Box<dyn Parser<T>>),
    AST(AST),
}

#[derive(Debug)]
enum AST {
    Root(Root),
    Declaration(Declaration),
    Param(Param),
    Type(Type),
    Statement(Statement),
    Expr(Expr),
    Empty,
}

impl AST {
    fn as_param(self) -> Param {
        if let AST::Param(param) = self {
            return param;
        }
        panic!("as_param called but the underlying variant is not AST::Param");
    }

    fn as_type(self) -> Type {
        if let AST::Type(typ) = self {
            return typ;
        }
        panic!("as_type called but the underlying variant is not AST::Type");
    }

    fn as_block_statement(self) -> BlockStatement {
        if let AST::Statement(typ) = self {
            if let Statement::Block(block) = typ {
                return block;
            }
        }
        panic!(
            "as_block called but the underlying variant is not AST::Statement(Statement::Block)"
        );
    }
}

pub struct SimpleParser<T: Lexer> {
    context: Context<T>,
}

impl<T: Lexer> SimpleParser<T> {
    pub fn new(lexer: T) -> Self {
        Self {
            context: Context { lexer: lexer },
        }
    }

    pub fn parse(&mut self) -> Result<Root, Error> {
        let mut stack: Vec<Box<dyn Parser<T>>> = vec![Box::new(RootParser::new())];

        let mut data = AST::Empty;
        while let Some(mut parser) = stack.pop() {
            let result = parser.parse(&mut self.context, data)?;
            match result {
                ParseResult::Push(new_parser) => {
                    stack.push(parser);
                    stack.push(new_parser);
                    data = AST::Empty;
                }
                ParseResult::AST(ast) => {
                    data = ast;
                }
            }
        }

        if let AST::Root(result) = data {
            return Ok(result);
        }

        panic!("invalid parsing result");
    }
}

struct RootParser {
    declarations: Vec<Declaration>,
}

impl RootParser {
    fn new() -> Self {
        Self {
            declarations: Vec::new(),
        }
    }
}

impl<T: Lexer> Parser<T> for RootParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<ParseResult<T>, Error> {
        match data {
            AST::Declaration(decl) => self.declarations.push(decl),
            AST::Empty => (),
            _ => panic!("got {:?} instead of declaration", data),
        }

        loop {
            let token = ctx.lexer.peek()?;
            match token.kind {
                TokenKind::EOI => {
                    return Ok(ParseResult::AST(AST::Root(Root {
                        declarations: std::mem::replace(&mut self.declarations, vec![]),
                    })));
                }
                TokenKind::Endl => {
                    ctx.lexer.next()?;
                }
                TokenKind::Fn => {
                    return Ok(ParseResult::Push(Box::new(FnParser::new())));
                }
                TokenKind::Var => {
                    unimplemented!("global variable is not implemented yet");
                }
                _ => {
                    let token = ctx.lexer.next()?;
                    return Err(Error::UnexpectedToken {
                        expected: vec![TokenKind::Fn, TokenKind::Var],
                        found: token,
                    });
                }
            }
        }
    }
}

struct FnParser {
    state: FnParserState,

    name: Option<Token>,
    params: Vec<Param>,
    return_type: Option<Type>,
    body: Option<BlockStatement>,
}

enum FnParserState {
    Name,
    Param,
    Return,
    Body,
}

impl FnParser {
    fn new() -> Self {
        Self {
            state: FnParserState::Name,
            name: None,
            params: vec![],
            return_type: None,
            body: None,
        }
    }

    fn parse_fn_name<T: Lexer>(&mut self, ctx: &mut Context<T>) -> Result<ParseResult<T>, Error> {
        self.expect(ctx, TokenKind::Fn)?;

        let name = self.expect(ctx, TokenKind::Ident("".to_string()))?;

        self.expect(ctx, TokenKind::OpenBrace)?;
        self.name = Some(name);

        if self.check(ctx, TokenKind::CloseBrace)?.is_some() {
            return self.parse_after_close_brace(ctx);
        }

        self.state = FnParserState::Param;
        return Ok(ParseResult::Push(Box::new(ParamParser::new())));
    }

    fn parse_after_close_brace<T: Lexer>(
        &mut self,
        ctx: &mut Context<T>,
    ) -> Result<ParseResult<T>, Error> {
        if self.check(ctx, TokenKind::Colon)?.is_none() {
            self.state = FnParserState::Body;
            return Ok(ParseResult::Push(Box::new(BlockStatementParser::new())));
        }

        self.state = FnParserState::Return;
        return Ok(ParseResult::Push(Box::new(TypeParser::new())));
    }

    fn parse_fn_param<T: Lexer>(
        &mut self,
        ctx: &mut Context<T>,
        data: AST,
    ) -> Result<ParseResult<T>, Error> {
        let param = data.as_param();
        self.params.push(param);

        let t = self.expect_one_of(ctx, vec![TokenKind::Comma, TokenKind::CloseBrace])?;
        if t.kind == TokenKind::Comma {
            return Ok(ParseResult::Push(Box::new(ParamParser::new())));
        }

        return self.parse_after_close_brace(ctx);
    }

    fn parse_fn_return<T: Lexer>(
        &mut self,
        ctx: &mut Context<T>,
        data: AST,
    ) -> Result<ParseResult<T>, Error> {
        let typ = data.as_type();
        self.return_type = Some(typ);

        self.state = FnParserState::Body;
        return Ok(ParseResult::Push(Box::new(BlockStatementParser::new())));
    }

    fn parse_fn_body<T: Lexer>(
        &mut self,
        ctx: &mut Context<T>,
        data: AST,
    ) -> Result<ParseResult<T>, Error> {
        let body = data.as_block_statement();
        self.body = Some(body);

        return Ok(ParseResult::AST(AST::Declaration(Declaration::Fn {
            name: self.name.take().unwrap(),
            param: std::mem::replace(&mut self.params, vec![]),
            ret_type: self.return_type.take().unwrap(),
            body: self.body.take().unwrap(),
        })));
    }
}

impl<T: Lexer> Parser<T> for FnParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<ParseResult<T>, Error> {
        match &self.state {
            FnParserState::Name => self.parse_fn_name(ctx),
            FnParserState::Param => self.parse_fn_param(ctx, data),
            FnParserState::Return => self.parse_fn_return(ctx, data),
            FnParserState::Body => self.parse_fn_body(ctx, data),
        }
    }
}

struct ParamParser {}

impl ParamParser {
    fn new() -> Self {
        Self {}
    }
}

impl<T: Lexer> Parser<T> for ParamParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<ParseResult<T>, Error> {
        unimplemented!("param parser is not implemented yet");
    }
}

struct TypeParser {}

impl TypeParser {
    fn new() -> Self {
        Self {}
    }
}

impl<T: Lexer> Parser<T> for TypeParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<ParseResult<T>, Error> {
        unimplemented!("type parser is not implemented yet");
    }
}

struct BlockStatementParser {}

impl BlockStatementParser {
    fn new() -> Self {
        Self {}
    }
}

impl<T: Lexer> Parser<T> for BlockStatementParser {
    fn parse(&mut self, ctx: &mut Context<T>, data: AST) -> Result<ParseResult<T>, Error> {
        unimplemented!("block parser is not implemented yet");
    }
}
