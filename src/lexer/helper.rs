use crate::{
    errors::Error,
    lexer::ILexer,
    token::{Token, TokenKind},
};

pub trait LexerHelper: ILexer {
    fn expect(&mut self, kind: TokenKind) -> Result<Token, Error>;
    fn next_is(&mut self, kind: &TokenKind) -> Result<Option<Token>, Error>;
    fn next_in(&mut self, kind: &[TokenKind]) -> Result<Option<Token>, Error>;
    fn consume_endl(&mut self) -> Result<(), Error>;
    fn consume_comment(&mut self) -> Result<(), Error>;
}

impl<T> LexerHelper for T
where
    T: ILexer,
{
    fn expect(&mut self, kind: TokenKind) -> Result<Token, Error> {
        self.consume_comment()?;

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
        self.consume_comment()?;

        let token = self.peek()?;
        if &token.kind != kind {
            return Ok(None);
        }

        let token = self.next()?;
        Ok(Some(token))
    }

    fn next_in(&mut self, kind: &[TokenKind]) -> Result<Option<Token>, Error> {
        self.consume_comment()?;

        let token = self.peek()?;
        for k in kind.iter() {
            if &token.kind == k {
                return Ok(Some(self.next()?));
            }
        }

        Ok(None)
    }

    fn consume_endl(&mut self) -> Result<(), Error> {
        self.consume_comment()?;

        while matches!(self.peek()?.kind, TokenKind::Endl) {
            self.next()?;
        }
        Ok(())
    }

    fn consume_comment(&mut self) -> Result<(), Error> {
        while matches!(self.peek()?.kind, TokenKind::Comment) {
            self.next()?;
        }
        Ok(())
    }
}
