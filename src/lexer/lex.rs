use crate::chars::{CharReader, ICharReader};
use crate::errors::Error;
use crate::pos::Pos;
use crate::token::{Token, TokenKind};
use std::collections::{HashMap, VecDeque};
use std::io::Read;
use std::rc::Rc;

type Result<T> = std::result::Result<T, Error>;

pub trait ILexer {
    fn next(&mut self) -> Result<Token>;
    fn peek(&mut self) -> Result<&Token>;
}

pub struct Lexer<R: Read> {
    reader: CharReader<R>,
    file_name: Rc<String>,

    is_started: bool,
    ch: Option<char>,
    pos: Pos,

    temp_pos: Pos,
    temp_val: String,

    tokens: VecDeque<Token>,
    eoi: Option<Token>,
}

impl<R: Read> Lexer<R> {
    pub fn new(reader: R, file_name: &str) -> Self {
        Self {
            reader: CharReader::new(reader, Rc::new(String::from(file_name))),
            file_name: Rc::new(String::from(file_name)),

            is_started: false,
            ch: None,
            pos: Pos {
                line: 1,
                col: 0,
                file_name: Rc::new(String::from(file_name)),
            },

            temp_pos: Pos {
                line: 0,
                col: 0,
                file_name: Rc::new(String::from(file_name)),
            },
            temp_val: String::new(),

            tokens: VecDeque::new(),
            eoi: None,
        }
    }
}

impl<T: Read> ILexer for Lexer<T> {
    fn next(&mut self) -> Result<Token> {
        self.advance()?;
        if let Some(token) = self.tokens.pop_front() {
            return Ok(token);
        }
        Ok(Token {
            kind: TokenKind::Eoi,
            value: Rc::new(String::new()),
            pos: self.temp_pos.clone(),
        })
    }

    fn peek(&mut self) -> Result<&Token> {
        self.advance()?;
        if let Some(token) = self.tokens.front() {
            return Ok(token);
        }
        Ok(self.eoi.as_ref().unwrap())
    }
}

impl<R: Read> Lexer<R> {
    fn advance(&mut self) -> Result<()> {
        self.advance_char()?;
        let ch = if let Some(ch) = self.ch {
            ch
        } else {
            self.eoi = Some(Token {
                kind: TokenKind::Eoi,
                value: Rc::new(String::new()),
                pos: self.temp_pos.clone(),
            });
            return Ok(());
        };

        self.temp_val = String::from(ch);
        self.temp_pos = self.pos.clone();

        match ch {
            '\n' => self.consume_and_emit_token(TokenKind::Endl),
            '_' | 'a'..='z' | 'A'..='Z' => self.parse_word(),
            '"' | '`' => self.parse_string(),
            '0'..='9' => self.parse_number(),
            '!' | '%' | '&' | '|' | '^' | '~' | '(' | ')' | '{' | '}' | '[' | ']' | '*' | '+' | '-' | ':' | '<'
            | '>' | '=' | ',' => self.parse_operator(),
            '/' => self.parse_slash(),
            _ => Err(Error::UnexpectedChar {
                char: ch.into(),
                pos: self.pos.clone(),
            }),
        }
    }

    fn parse_word(&mut self) -> Result<()> {
        self.temp_val.push_str(
            self.reader
                .consume_while(|ch| matches!(ch, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'))?
                .as_str(),
        );

        let kind = match self.temp_val.as_str() {
            "package" => TokenKind::Package,
            "import" => TokenKind::Import,
            "if" => TokenKind::If,
            "native" => TokenKind::Native,
            "var" => TokenKind::Var,
            "while" => TokenKind::While,
            "fn" => TokenKind::Fn,
            "as" => TokenKind::As,
            "return" => TokenKind::Return,
            "bool" => TokenKind::Bool,
            "i8" => TokenKind::I8,
            "i16" => TokenKind::I16,
            "i32" => TokenKind::I32,
            "i64" => TokenKind::I64,
            "u8" => TokenKind::U8,
            "u16" => TokenKind::U16,
            "u32" => TokenKind::U32,
            "u64" => TokenKind::U64,
            "f32" => TokenKind::F32,
            "f64" => TokenKind::F64,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Ident,
        };
        self.emit_token(kind)?;
        self.next_char()
    }

    fn parse_string(&mut self) -> Result<()> {
        let opening_quote = self.temp_val.chars().next().unwrap();

        let backslash_chars = HashMap::from([
            ('n', '\n'),
            ('r', '\r'),
            ('t', '\t'),
            ('\\', '\\'),
            ('0', '\0'),
            ('"', '"'),
            ('\'', '\''),
            ('`', '`'),
            // \xAB = 0xAB
            // \u{abcd} = 0xabcd
        ]);

        let mut after_backslash = false;
        while let Some(ch) = self.reader.next()? {
            let c = ch.value;

            if c == '\n' {
                return Err(Error::UnexpectedSymbol {
                    symbol: "\n".to_string(),
                    pos: ch.pos,
                });
            }

            if after_backslash {
                if let Some(v) = backslash_chars.get(&c) {
                    self.temp_val.push(*v);
                } else {
                    return Err(Error::UnexpectedSymbol {
                        symbol: String::from(c),
                        pos: ch.pos,
                    });
                }
                after_backslash = false;
            } else if c == '\\' {
                after_backslash = true;
            } else if c == opening_quote {
                break;
            } else {
                self.temp_val.push(c);
            }
        }

        self.emit_token(TokenKind::StringLit)
    }

    fn parse_number(&mut self) -> Result<()> {
        let mut base = 10;
        if let Some('0') = self.ch {
            base = match self.reader.peek_char()? {
                Some('b' | 'B') => 2,
                Some('o' | 'O') => 8,
                Some('x' | 'X') => 16,
                Some('0'..='7') => 8,
                _ => {
                    self.emit_token(TokenKind::IntegerLit)?;
                    return self.next_char();
                }
            };
            self.temp_val.push(self.reader.next()?.unwrap().value);
        }

        let digit_matcher = match base {
            2 => |c| c == '0' || c == '1' || c == '_',
            8 => |c| (c >= '0' && c <= '7') || c == '_',
            10 => |c| (c >= '0' && c <= '9') || c == '_',
            16 => |c| (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || c == '_',
            _ => unreachable!(),
        };

        self.temp_val
            .push_str(self.reader.consume_while(digit_matcher)?.as_str());

        let mut kind = TokenKind::IntegerLit;
        if let Some('.') = self.reader.peek_char()? {
            let ch = self.reader.next()?.unwrap();
            self.temp_val.push(ch.value);
            self.temp_val
                .push_str(self.reader.consume_while(digit_matcher)?.as_str());
            kind = TokenKind::FloatLit;
        }

        if let Some('e' | 'E') = self.reader.peek_char()? {
            let ch = self.reader.next()?.unwrap();
            self.temp_val.push(ch.value);
            self.temp_val
                .push_str(self.reader.consume_while(digit_matcher)?.as_str());
            kind = TokenKind::FloatLit;
        }

        self.emit_token(kind)?;
        self.next_char()
    }

    fn parse_operator(&mut self) -> Result<()> {
        let first_char = self.temp_val.chars().next().unwrap();
        self.next_char()?;
        let second_char = self.ch;

        match (first_char, second_char) {
            ('!', Some('=')) => self.consume_and_emit_token(TokenKind::NotEq),
            ('!', _) => self.emit_token(TokenKind::Not),
            ('%', Some('=')) => self.consume_and_emit_token(TokenKind::ModAssign),
            ('%', _) => self.emit_token(TokenKind::Mod),
            ('&', Some('&')) => self.consume_and_emit_token(TokenKind::And),
            ('&', Some('=')) => self.consume_and_emit_token(TokenKind::BitAndAssign),
            ('&', _) => self.emit_token(TokenKind::BitAnd),
            ('|', Some('|')) => self.consume_and_emit_token(TokenKind::Or),
            ('|', Some('=')) => self.consume_and_emit_token(TokenKind::BitOrAssign),
            ('|', _) => self.emit_token(TokenKind::BitOr),
            ('^', Some('=')) => self.consume_and_emit_token(TokenKind::BitXorAssign),
            ('^', _) => self.emit_token(TokenKind::BitXor),
            ('~', _) => self.emit_token(TokenKind::BitNot),
            ('(', _) => self.emit_token(TokenKind::OpenBrace),
            (')', _) => self.emit_token(TokenKind::CloseBrace),
            ('{', _) => self.emit_token(TokenKind::OpenBlock),
            ('}', _) => self.emit_token(TokenKind::CloseBlock),
            ('[', _) => self.emit_token(TokenKind::OpenBrack),
            (']', _) => self.emit_token(TokenKind::CloseBrack),
            ('*', Some('=')) => self.consume_and_emit_token(TokenKind::MulAssign),
            ('*', _) => self.emit_token(TokenKind::Mul),
            ('+', Some('=')) => self.consume_and_emit_token(TokenKind::PlusAssign),
            ('+', _) => self.emit_token(TokenKind::Plus),
            ('-', Some('=')) => self.consume_and_emit_token(TokenKind::MinusAssign),
            ('-', _) => self.emit_token(TokenKind::Minus),
            ('/', Some('=')) => self.consume_and_emit_token(TokenKind::DivAssign),
            ('/', _) => self.emit_token(TokenKind::DivAssign),
            (':', _) => self.emit_token(TokenKind::Colon),
            ('=', Some('=')) => self.consume_and_emit_token(TokenKind::Eq),
            ('=', _) => self.emit_token(TokenKind::Assign),
            (',', _) => self.emit_token(TokenKind::Comma),
            ('<', Some('=')) => self.consume_and_emit_token(TokenKind::LTEq),
            ('<', Some('<')) => {
                self.next_char()?;
                match self.ch {
                    Some('=') => self.consume_and_emit_token(TokenKind::ShlAssign),
                    _ => self.emit_token(TokenKind::ShlAssign),
                }
            }
            ('<', _) => self.emit_token(TokenKind::LT),
            ('>', Some('=')) => self.consume_and_emit_token(TokenKind::GTEq),
            ('>', Some('>')) => {
                self.next_char()?;
                match self.ch {
                    Some('=') => self.consume_and_emit_token(TokenKind::ShrAssign),
                    _ => self.emit_token(TokenKind::ShrAssign),
                }
            }
            ('>', _) => self.emit_token(TokenKind::GT),
            _ => unreachable!(),
        }
    }

    fn parse_slash(&mut self) -> Result<()> {
        self.temp_pos = self.pos.clone();
        self.temp_val = String::from(self.ch.unwrap());
        match self.reader.peek_char()? {
            Some('/') => self.parse_comment(),
            _ => self.parse_operator(),
        }
    }

    fn parse_comment(&mut self) -> Result<()> {
        loop {
            if let Some('\n') | None = self.ch {
                break;
            }
            self.temp_val.push(self.ch.unwrap());
            self.next_char()?;
        }

        self.emit_token(TokenKind::Comment)
    }

    fn advance_char(&mut self) -> Result<()> {
        if !self.is_started {
            self.is_started = true;
            self.next_char()?;
        }

        while let Some(' ' | '\t' | '\r') = self.ch {
            self.next_char()?;
        }

        Ok(())
    }

    fn next_char(&mut self) -> Result<()> {
        if let Some(ch) = self.reader.next()? {
            self.ch = Some(ch.value);
            self.pos = ch.pos;
        } else {
            self.ch = None;
            self.pos.col += 1;
        }
        Ok(())
    }

    fn consume_and_emit_token(&mut self, kind: TokenKind) -> Result<()> {
        self.temp_val.push(self.ch.unwrap());
        self.next_char()?;
        self.emit_token(kind)
    }

    fn emit_token(&mut self, kind: TokenKind) -> Result<()> {
        let val = std::mem::replace(&mut self.temp_val, String::new());
        let pos = std::mem::replace(
            &mut self.temp_pos,
            Pos {
                line: 0,
                col: 0,
                file_name: Rc::clone(&self.file_name),
            },
        );

        self.tokens.push_back(Token {
            kind,
            pos,
            value: Rc::new(val),
        });

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_fn() {
        let simple_fn = r#"fn gcd(a: i32, b: i64): i64 {
                while b != 0 {
                    var t = b;
                    b = a % b;
                    b = t;
                }
                return a;
            }
            "#
        .as_bytes();
        let mut lexer = Lexer::new(simple_fn, "anonymous");

        let result = lexer.next().unwrap();
        assert_eq!(
            result,
            Token {
                kind: TokenKind::Fn,
                value: Rc::new("fn".to_string()),
                pos: Pos {
                    file_name: Rc::new("anonymous".to_string()),
                    line: 1,
                    col: 1
                }
            },
        );
    }
}
