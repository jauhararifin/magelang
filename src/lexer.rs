use crate::token::{Error, Lexer, Pos, Token, TokenKind};
use std::io::Read;

pub struct SimpleLexer<T: Read> {
    reader: T,

    is_loaded: bool,
    char_posts: Vec<CharPos>,
    char_offset: usize,
    tokens: Vec<Token>,
}

#[derive(Copy, Clone)]
struct CharPos {
    val: char,
    pos: Pos,
}

impl<T: Read> SimpleLexer<T> {
    pub fn new(reader: T) -> Self {
        Self {
            reader: reader,
            is_loaded: false,
            char_posts: Vec::new(),
            char_offset: 0,
            tokens: Vec::new(),
        }
    }

    fn load_tokens(&mut self) -> Result<(), Error> {
        if self.is_loaded {
            return Ok(());
        }

        self.load_char_posts()?;

        let size = self.char_posts.len();
        while self.char_offset < size {
            let c = self.char_posts[self.char_offset].val;
            if self.next_is("//") {
                self.consume_comment();
            } else if self.next_in("0123456789") {
                self.consume_number_literal();
            } else if self.next_is("\"") {
            } else if self.next_is("\n") {
            } else if self.next_is("\n") {
            } else if c.is_alphabetic() || c == '_' {
            } else {
            }
        }

        self.is_loaded = true;
        return Ok(());
    }

    fn load_char_posts(&mut self) -> Result<(), Error> {
        let mut buff = String::new();
        self.reader.read_to_string(&mut buff)?;

        let mut line = 1;
        let mut col = 0;

        for c in buff.chars() {
            col += 1;
            self.char_posts.push(CharPos {
                val: c,
                pos: Pos {
                    line: line,
                    col: col,
                },
            });
            if c == '\n' {
                line += 1;
                col = 0;
            }
        }

        Ok(())
    }

    fn next_is(&self, s: &str) -> bool {
        if self.char_offset + s.len() > self.char_posts.len() {
            return false;
        }

        for (i, c) in s.chars().enumerate() {
            if self.char_posts[self.char_offset + i].val != c {
                return false;
            }
        }

        true
    }

    fn next_in(&self, s: &str) -> bool {
        if self.char_offset >= self.char_posts.len() {
            return false;
        }

        for c in s.chars() {
            if self.char_posts[self.char_offset].val == c {
                return true;
            }
        }

        false
    }

    fn consume_comment(&mut self) {
        let (value, pos) = self.consume_while_match(|c| c != '\n');
        self.emit_token(TokenKind::Comment(value), pos);
    }

    fn consume_number_literal(&mut self) {
        let (value, pos) = self.consume_while_match(|c| c.is_digit(10) || c == '_');
        self.emit_token(TokenKind::NumberLit(value), pos);
    }

    fn consume_while_match<F: Fn(char) -> bool>(&mut self, matcher: F) -> (String, Pos) {
        let mut result = String::new();
        let mut pos = Pos { line: 0, col: 0 };
        while self.char_offset < self.char_posts.len() {
            let char_pos = &self.char_posts[self.char_offset];
            let c = char_pos.val;
            if matcher(c) {
                result.push(c);
                if pos.line == 0 {
                    pos = char_pos.pos;
                }
                self.char_offset += 1;
            } else {
                break;
            }
        }

        return (result, pos);
    }

    fn emit_token(&mut self, token_kind: TokenKind, pos: Pos) {
        self.tokens.push(Token {
            kind: token_kind,
            pos: pos,
        })
    }
}

impl<T: Read> Lexer for SimpleLexer<T> {
    fn next(&mut self) -> Result<Token, Error> {
        Ok(Token {
            kind: TokenKind::EOI,
            pos: Pos { line: 1, col: 1 },
        })
    }

    fn peek(&self) -> Result<Token, Error> {
        Ok(Token {
            kind: TokenKind::EOI,
            pos: Pos { line: 1, col: 1 },
        })
    }
}
