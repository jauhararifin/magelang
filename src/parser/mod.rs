use crate::ast::{
    AssignNode, BlockStatementNode, DeclNode, ExprNode, ExprNodeKind, FnDeclNode, FnHeaderNode, IfNode, ParamNode,
    ReturnNode, RootNode, StatementNode, VarNode, WhileNode,
};
use crate::errors::Error;
use crate::lexer::{ILexer, LexerHelper};
use crate::token::{Token, TokenKind};

use self::expr::{ExprParserHelper, IExprParserHelper};

mod expr;

pub trait IParser {
    fn parse(&mut self) -> std::result::Result<RootNode, Error>;
}

pub struct Parser<T: LexerHelper> {
    lexer: T,
    stack: Vec<State>,
}

#[derive(Debug)]
enum State {
    Root,
    RootDecl {
        declarations: Vec<DeclNode>,
    },
    Var,
    FnName,
    FnParam {
        fn_token: Token,
        name: Token,
        native_token: Option<Token>,
        params: Vec<ParamNode>,
    },
    FnReturn {
        fn_token: Token,
        name: Token,
        native_token: Option<Token>,
        params: Vec<ParamNode>,
    },
    FnBody {
        fn_token: Token,
        name: Token,
        params: Vec<ParamNode>,
        ret_type: Option<ExprNode>,
    },
    ParamName,
    Statement,
    BlockStatement {
        body: Vec<StatementNode>,
    },
    AssignStatement,
    IfStatement,
    IfStatementBody {
        if_token: Token,
        cond: ExprNode,
    },
    WhileStatement,
    WhileStatementBody {
        while_token: Token,
        cond: ExprNode,
    },
    ReturnStatement,
}

#[derive(Debug)]
enum Ast {
    Root(RootNode),
    FnDecl(FnDeclNode),
    Var(VarNode),
    Param(ParamNode),
    Statement(StatementNode),
    Assign(AssignNode),
    If(IfNode),
    While(WhileNode),
    Return(ReturnNode),
    Expr(ExprNode),
    BlockStatement(BlockStatementNode),
    Empty,
}

impl<T: ILexer> Parser<T> {
    pub fn new(lexer: T) -> Self {
        Self {
            lexer,
            stack: Vec::new(),
        }
    }

    fn parse_state(&mut self, state: State, data: Ast) -> Result<Ast, Error> {
        match state {
            State::Root => self.parse_root(),
            State::RootDecl { declarations } => self.parse_root_decl(declarations, data),
            State::Var => self.parse_var(),
            State::FnName => self.parse_fn_name(),
            State::FnParam {
                fn_token,
                name,
                native_token,
                params,
            } => self.parse_fn_param(fn_token, name, native_token, params, data),
            State::FnReturn {
                fn_token,
                name,
                native_token,
                params,
            } => self.parse_fn_return(fn_token, name, native_token, params),
            State::FnBody {
                fn_token,
                name,
                params,
                ret_type,
            } => self.parse_fn_body(fn_token, name, params, ret_type, data),
            State::ParamName => self.parse_param_name(),
            State::Statement => self.parse_statement(data),
            State::BlockStatement { body } => self.parse_block_statement(body, data),
            State::AssignStatement => self.parse_assign_statement(),
            State::IfStatement => self.parse_if_statement(),
            State::IfStatementBody { if_token, cond } => self.parse_if_statement_body(if_token, cond, data),
            State::WhileStatement => self.parse_while_statement(),
            State::WhileStatementBody { while_token, cond } => self.parse_while_statement_body(while_token, cond, data),
            State::ReturnStatement => self.parse_return_statement(),
        }
    }

    fn parse_root(&mut self) -> Result<Ast, Error> {
        self.consume_endl_and_comment()?;
        self.stack.push(State::RootDecl { declarations: vec![] });
        Ok(Ast::Empty)
    }

    fn parse_root_decl(&mut self, mut declarations: Vec<DeclNode>, data: Ast) -> Result<Ast, Error> {
        match data {
            Ast::FnDecl(decl) => declarations.push(DeclNode::Fn(decl)),
            Ast::Empty => (),
            _ => unreachable!("got {:?} instead of declaration", data),
        }

        self.consume_endl_and_comment()?;

        let token = self.lexer.peek()?;
        match token.kind {
            TokenKind::Eoi => Ok(Ast::Root(RootNode { declarations })),
            TokenKind::Fn => {
                self.stack.push(State::RootDecl { declarations });
                self.stack.push(State::FnName);
                Ok(Ast::Empty)
            }
            _ => {
                let token = self.lexer.next()?;
                Err(Error::UnexpectedToken {
                    expected: vec![TokenKind::Fn, TokenKind::Var],
                    found: token,
                })
            }
        }
    }

    fn parse_var(&mut self) -> Result<Ast, Error> {
        self.expect(TokenKind::Var)?;
        let name = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        let typ = ExprParserHelper::new(&mut self.lexer).parse()?;
        Ok(if self.check(&TokenKind::Assign)?.is_some() {
            let value = ExprParserHelper::new(&mut self.lexer).parse()?;
            Ast::Var(VarNode {
                name,
                typ,
                value: Some(value),
            })
        } else {
            Ast::Var(VarNode { name, typ, value: None })
        })
    }

    fn parse_fn_name(&mut self) -> Result<Ast, Error> {
        let fn_token = self.expect(TokenKind::Fn)?;
        let native_token = self.check(&TokenKind::Native)?;
        let name = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::OpenBrace)?;

        self.stack.push(State::FnParam {
            fn_token,
            name,
            native_token,
            params: Vec::new(),
        });
        Ok(Ast::Empty)
    }

    fn parse_fn_param(
        &mut self,
        fn_token: Token,
        name: Token,
        native_token: Option<Token>,
        mut params: Vec<ParamNode>,
        data: Ast,
    ) -> Result<Ast, Error> {
        let mut first_param = true;
        if let Ast::Param(param) = data {
            params.push(param);
            first_param = false;
        }

        if self.check(&TokenKind::CloseBrace)?.is_some() {
            if self.check(&TokenKind::Colon)?.is_none() {
                if native_token.is_none() {
                    self.stack.push(State::FnBody {
                        fn_token,
                        name,
                        params,
                        ret_type: None,
                    });
                    return Ok(Ast::Empty);
                }
                return Ok(Ast::FnDecl(FnDeclNode {
                    fn_token,
                    name,
                    header: FnHeaderNode {
                        native_token,
                        params,
                        ret_type: None,
                    },
                    body: None,
                }));
            }

            self.stack.push(State::FnReturn {
                fn_token,
                name,
                native_token,
                params,
            });
            return Ok(Ast::Empty);
        }

        if !first_param {
            self.expect(TokenKind::Comma)?;
        }

        self.stack.push(State::FnParam {
            fn_token,
            name,
            native_token,
            params,
        });
        self.stack.push(State::ParamName);

        Ok(Ast::Empty)
    }

    fn parse_fn_return(
        &mut self,
        fn_token: Token,
        name: Token,
        native_token: Option<Token>,
        params: Vec<ParamNode>,
    ) -> Result<Ast, Error> {
        let return_type = ExprParserHelper::new(&mut self.lexer).parse()?;

        if native_token.is_none() {
            self.stack.push(State::FnBody {
                fn_token,
                name,
                params,
                ret_type: Some(return_type),
            });
            return Ok(Ast::Empty);
        }

        return Ok(Ast::FnDecl(FnDeclNode {
            fn_token,
            name,
            header: FnHeaderNode {
                native_token,
                params,
                ret_type: Some(return_type),
            },
            body: None,
        }));
    }

    fn parse_fn_body(
        &mut self,
        fn_token: Token,
        name: Token,
        params: Vec<ParamNode>,
        ret_type: Option<ExprNode>,
        data: Ast,
    ) -> Result<Ast, Error> {
        if let Ast::BlockStatement(body) = data {
            return Ok(Ast::FnDecl(FnDeclNode {
                fn_token,
                name,
                header: FnHeaderNode {
                    native_token: None,
                    params,
                    ret_type,
                },
                body: Some(body),
            }));
        }

        self.stack.push(State::FnBody {
            fn_token,
            name,
            params,
            ret_type,
        });
        self.stack.push(State::BlockStatement { body: vec![] });

        Ok(Ast::Empty)
    }

    fn parse_param_name(&mut self) -> Result<Ast, Error> {
        let name = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        let typ = ExprParserHelper::new(&mut self.lexer).parse()?;
        Ok(Ast::Param(ParamNode { name, typ }))
    }

    fn parse_statement(&mut self, data: Ast) -> Result<Ast, Error> {
        match data {
            Ast::Var(stmt) => return Ok(Ast::Statement(StatementNode::Var(stmt))),
            Ast::Assign(stmt) => return Ok(Ast::Statement(StatementNode::Assign(stmt))),
            Ast::Return(stmt) => return Ok(Ast::Statement(StatementNode::Return(stmt))),
            Ast::If(stmt) => return Ok(Ast::Statement(StatementNode::If(stmt))),
            Ast::While(stmt) => return Ok(Ast::Statement(StatementNode::While(stmt))),
            Ast::BlockStatement(stmt) => return Ok(Ast::Statement(StatementNode::Block(stmt))),
            Ast::Expr(stmt) => return Ok(Ast::Statement(StatementNode::Expr(stmt))),
            Ast::Statement(stmt) => return Ok(Ast::Statement(stmt)),
            Ast::Empty => (),
            _ => unreachable!("invalid data when parsing statement: {:?}", data),
        }

        self.consume_endl_and_comment()?;

        let next_parser = match &self.lexer.peek()?.kind {
            TokenKind::Var => State::Var,
            TokenKind::While => State::WhileStatement,
            TokenKind::If => State::IfStatement,
            TokenKind::Return => State::ReturnStatement,
            TokenKind::OpenBlock => State::BlockStatement { body: vec![] },
            _ => State::AssignStatement,
        };

        self.stack.push(State::Statement);
        self.stack.push(next_parser);

        Ok(Ast::Empty)
    }

    fn parse_block_statement(&mut self, mut body: Vec<StatementNode>, data: Ast) -> Result<Ast, Error> {
        self.consume_endl_and_comment()?;

        match data {
            Ast::Empty => {
                self.expect(TokenKind::OpenBlock)?;
            }
            Ast::Statement(statement) => {
                body.push(statement);
            }
            _ => unreachable!("invalid data when parsing block statement: {:?}", data),
        }

        self.consume_endl_and_comment()?;

        let result = if self.check(&TokenKind::CloseBlock)?.is_some() {
            Ast::BlockStatement(BlockStatementNode { body })
        } else {
            self.stack.push(State::BlockStatement { body });
            self.stack.push(State::Statement);
            Ast::Empty
        };

        Ok(result)
    }

    fn parse_assign_statement(&mut self) -> Result<Ast, Error> {
        let receiver = ExprParserHelper::new(&mut self.lexer).parse()?;

        let op = self.check_one_of(&vec![
            TokenKind::Assign,
            TokenKind::BitAndAssign,
            TokenKind::BitOrAssign,
            TokenKind::BitXorAssign,
            TokenKind::PlusAssign,
            TokenKind::MinusAssign,
            TokenKind::MulAssign,
            TokenKind::DivAssign,
            TokenKind::ModAssign,
            TokenKind::ShrAssign,
            TokenKind::ShlAssign,
        ])?;

        if let Some(op) = op {
            let value = ExprParserHelper::new(&mut self.lexer).parse()?;
            Ok(Ast::Assign(AssignNode { receiver, op, value }))
        } else {
            Ok(Ast::Expr(receiver))
        }
    }

    fn parse_if_statement(&mut self) -> Result<Ast, Error> {
        let if_token = self.expect(TokenKind::If)?;
        let cond = ExprParserHelper::new(&mut self.lexer).parse()?;
        self.stack.push(State::IfStatementBody { if_token, cond });
        Ok(Ast::Empty)
    }

    fn parse_if_statement_body(&mut self, if_token: Token, cond: ExprNode, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.stack.push(State::IfStatementBody { if_token, cond });
                self.stack.push(State::BlockStatement { body: vec![] });
                Ast::Empty
            }
            Ast::BlockStatement(body) => Ast::If(IfNode { if_token, cond, body }),
            _ => unreachable!("invalid data when parsing if statement body: {:?}", data),
        })
    }

    fn parse_while_statement(&mut self) -> Result<Ast, Error> {
        let while_token = self.expect(TokenKind::While)?;
        let cond = ExprParserHelper::new(&mut self.lexer).parse()?;
        self.stack.push(State::WhileStatementBody { while_token, cond });
        Ok(Ast::Empty)
    }

    fn parse_while_statement_body(&mut self, while_token: Token, cond: ExprNode, data: Ast) -> Result<Ast, Error> {
        Ok(match data {
            Ast::Empty => {
                self.stack.push(State::WhileStatementBody { while_token, cond });
                self.stack.push(State::BlockStatement { body: vec![] });
                Ast::Empty
            }
            Ast::BlockStatement(body) => Ast::While(WhileNode {
                while_token,
                cond,
                body,
            }),
            _ => unreachable!("invalid data when parsing while statement body: {:?}", data),
        })
    }

    fn parse_return_statement(&mut self) -> Result<Ast, Error> {
        let return_token = self.expect(TokenKind::Return)?;
        let value = ExprParserHelper::new(&mut self.lexer).parse()?;
        let value = if let ExprNodeKind::Empty = value.kind {
            None
        } else {
            Some(value)
        };
        Ok(Ast::Return(ReturnNode { return_token, value }))
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, Error> {
        let token = self.lexer.next()?;
        if &token.kind != &kind {
            return Err(Error::UnexpectedToken {
                expected: vec![kind],
                found: token,
            });
        }
        Ok(token)
    }

    fn check(&mut self, kind: &TokenKind) -> Result<Option<Token>, Error> {
        let token = self.lexer.peek()?;
        if &token.kind != kind {
            return Ok(None);
        }

        let token = self.lexer.next()?;
        Ok(Some(token))
    }

    fn check_one_of(&mut self, kind: &Vec<TokenKind>) -> Result<Option<Token>, Error> {
        let token = self.lexer.peek()?;

        for k in kind.iter() {
            if &token.kind == k {
                return Ok(Some(self.lexer.next()?));
            }
        }

        Ok(None)
    }

    fn consume_endl_and_comment(&mut self) -> Result<(), Error> {
        while matches!(self.lexer.peek()?.kind, TokenKind::Endl | TokenKind::Comment) {
            self.lexer.next()?;
        }
        Ok(())
    }
}

impl<T: ILexer> IParser for Parser<T> {
    fn parse(&mut self) -> std::result::Result<RootNode, Error> {
        self.stack.push(State::Root);

        let mut data = Ast::Empty;
        while let Some(state) = self.stack.pop() {
            data = self.parse_state(state, data)?;
        }

        if let Ast::Root(result) = data {
            return Ok(result);
        }

        unreachable!("invalid parsing result");
    }
}
