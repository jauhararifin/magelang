use crate::{
    errors::Error,
    lexer::ILexer,
    token::{Token, TokenKind},
};

pub trait LexerHelper: ILexer {
    fn expect(&mut self, kind: TokenKind) -> Result<Token, Error>;
    fn next_is(&mut self, kind: &TokenKind) -> Result<Option<Token>, Error>;
    fn next_in(&mut self, kind: &[TokenKind]) -> Result<Option<Token>, Error>;
    fn skip_endls(&mut self) -> Result<(), Error>;
    fn skip_comments(&mut self) -> Result<(), Error>;
}

impl<T> LexerHelper for T
where
    T: ILexer,
{
    fn expect(&mut self, kind: TokenKind) -> Result<Token, Error> {
        self.skip_comments()?;

        let token = self.next()?;
        if &token.kind != &kind {
            return Err(Error::UnexpectedToken {
                expected: vec![kind],
                found: token,
            });
        }
        Ok(token)
    }

    fn next_is(&mut self, kind: &TokenKind) -> Result<Option<Token>, Error> {
        self.skip_comments()?;

        let token = self.peek()?;
        if &token.kind != kind {
            return Ok(None);
        }

        let token = self.next()?;
        Ok(Some(token))
    }

    fn next_in(&mut self, kind: &[TokenKind]) -> Result<Option<Token>, Error> {
        self.skip_comments()?;

        let token = self.peek()?;
        for k in kind.iter() {
            if &token.kind == k {
                return Ok(Some(self.next()?));
            }
        }

        Ok(None)
    }

    fn skip_endls(&mut self) -> Result<(), Error> {
        self.skip_comments()?;

        while matches!(self.peek()?.kind, TokenKind::Endl) {
            self.next()?;
        }
        Ok(())
    }

    fn skip_comments(&mut self) -> Result<(), Error> {
        while matches!(self.peek()?.kind, TokenKind::Comment) {
            self.next()?;
        }
        Ok(())
    }
}
