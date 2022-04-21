use crate::token::{Error, Lexer, Pos, Token, TokenKind};
use std::collections::{HashMap, VecDeque};
use std::io::Read;

pub struct SimpleLexer<T: Read> {
    reader: T,

    is_loaded: bool,
    char_posts: Vec<CharPos>,
    char_offset: usize,

    tokens: VecDeque<Token>,
    token_eoi: Token,
}

#[derive(Copy, Clone)]
struct CharPos {
    val: char,
    pos: Pos,
}

impl<T: Read> SimpleLexer<T> {
    pub fn new(reader: T) -> Self {
        Self {
            reader,

            is_loaded: false,
            char_posts: Vec::new(),
            char_offset: 0,

            tokens: VecDeque::new(),
            token_eoi: Token {
                kind: TokenKind::EOI,
                value: None,
                pos: Pos { line: 0, col: 0 },
            },
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

            self.token_eoi.pos = self.char_posts[self.char_offset].pos;

            if self.next_is("//") {
                self.consume_comment();
            } else if self.next_in("0123456789") {
                self.consume_number_literal();
            } else if self.next_is("\"") {
                self.consume_string_literal()?;
            } else if self.next_is("\n") {
                self.emit_token(TokenKind::Endl, None, self.char_posts[self.char_offset].pos);
                self.char_offset += 1;
            } else if c.is_alphabetic() || c == '_' {
                self.consume_word();
            } else if self.next_in("'*|-,&~)}<:./{=]^%[!(+>'") {
                self.consume_operator()?;
            } else {
                self.char_offset += 1;
            }
        }

        self.is_loaded = true;
        Ok(())
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
                pos: Pos { line, col },
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
        self.emit_token(TokenKind::Comment, Some(value), pos);
    }

    fn consume_number_literal(&mut self) {
        let (value, pos) = self.consume_while_match(|c| c.is_digit(10) || c == '_' || c == '.');
        if value.contains('.') {
            self.emit_token(TokenKind::FloatLit, Some(value), pos);
        } else {
            self.emit_token(TokenKind::IntegerLit, Some(value), pos);
        }
    }

    fn consume_string_literal(&mut self) -> Result<(), Error> {
        let opening_quote = self.char_posts[self.char_offset].val;

        let backslash_chars = HashMap::from([
            ('n', '\n'),
            ('r', '\r'),
            ('"', '"'),
            ('`', '`'),
            ('\\', '\\'),
            ('t', '\t'),
        ]);

        let mut value = String::new();
        let pos = self.char_posts[self.char_offset].pos;
        let mut after_backslash = false;

        self.char_offset += 1;
        while self.char_offset < self.char_posts.len() {
            let char_pos = self.char_posts[self.char_offset];
            let c = char_pos.val;

            if c == '\n' {
                return Err(Error::UnexpectedSymbol {
                    symbol: '\n',
                    pos: char_pos.pos,
                });
            }

            if after_backslash {
                if let Some(v) = backslash_chars.get(&c) {
                    value.push(*v);
                } else {
                    return Err(Error::UnexpectedSymbol {
                        symbol: c,
                        pos: char_pos.pos,
                    });
                }
                after_backslash = false;
            } else {
                if c == '\\' {
                    after_backslash = true;
                } else if c == opening_quote {
                    self.char_offset += 1;
                    break;
                } else {
                    value.push(c);
                }
            }

            self.char_offset += 1;
        }

        self.emit_token(TokenKind::StringLit, Some(value), pos);
        Ok(())
    }

    fn consume_word(&mut self) {
        let (name, pos) =
            self.consume_while_match(|c| c.is_alphabetic() || c == '_' || c.is_digit(10));

        match name.as_str() {
            "if" => self.emit_token(TokenKind::If, None, pos),
            "var" => self.emit_token(TokenKind::Var, None, pos),
            "type" => self.emit_token(TokenKind::Type, None, pos),
            "struct" => self.emit_token(TokenKind::Struct, None, pos),
            "tuple" => self.emit_token(TokenKind::Tuple, None, pos),
            "while" => self.emit_token(TokenKind::While, None, pos),
            "fn" => self.emit_token(TokenKind::Fn, None, pos),
            "as" => self.emit_token(TokenKind::As, None, pos),
            "return" => self.emit_token(TokenKind::Return, None, pos),
            "bool" => self.emit_token(TokenKind::Bool, None, pos),
            "i8" => self.emit_token(TokenKind::I8, None, pos),
            "i16" => self.emit_token(TokenKind::I16, None, pos),
            "i32" => self.emit_token(TokenKind::I32, None, pos),
            "i64" => self.emit_token(TokenKind::I64, None, pos),
            "u8" => self.emit_token(TokenKind::U8, None, pos),
            "u16" => self.emit_token(TokenKind::U16, None, pos),
            "u32" => self.emit_token(TokenKind::U32, None, pos),
            "u64" => self.emit_token(TokenKind::U64, None, pos),
            "f32" => self.emit_token(TokenKind::F32, None, pos),
            "f64" => self.emit_token(TokenKind::F64, None, pos),
            "true" => self.emit_token(TokenKind::True, None, pos),
            "false" => self.emit_token(TokenKind::False, None, pos),
            _ => self.emit_token(TokenKind::Ident, Some(name), pos),
        }
    }

    fn consume_operator(&mut self) -> Result<(), Error> {
        let found = self.consume_exact_operator("!=", TokenKind::NotEq)
            || self.consume_exact_operator("!", TokenKind::Not)
            || self.consume_exact_operator("%=", TokenKind::ModAssign)
            || self.consume_exact_operator("%", TokenKind::Mod)
            || self.consume_exact_operator("&&", TokenKind::And)
            || self.consume_exact_operator("&=", TokenKind::BitAndAssign)
            || self.consume_exact_operator("&", TokenKind::BitAnd)
            || self.consume_exact_operator("||", TokenKind::Or)
            || self.consume_exact_operator("|=", TokenKind::BitOrAssign)
            || self.consume_exact_operator("|", TokenKind::BitOr)
            || self.consume_exact_operator("^=", TokenKind::BitXorAssign)
            || self.consume_exact_operator("^", TokenKind::BitXor)
            || self.consume_exact_operator("~", TokenKind::BitNot)
            || self.consume_exact_operator("(", TokenKind::OpenBrace)
            || self.consume_exact_operator(")", TokenKind::CloseBrace)
            || self.consume_exact_operator("{", TokenKind::OpenBlock)
            || self.consume_exact_operator("}", TokenKind::CloseBlock)
            || self.consume_exact_operator("*=", TokenKind::MulAssign)
            || self.consume_exact_operator("*", TokenKind::Mul)
            || self.consume_exact_operator("+=", TokenKind::PlusAssign)
            || self.consume_exact_operator("+", TokenKind::Plus)
            || self.consume_exact_operator("-=", TokenKind::MinusAssign)
            || self.consume_exact_operator("-", TokenKind::Minus)
            || self.consume_exact_operator("/=", TokenKind::DivAssign)
            || self.consume_exact_operator("/", TokenKind::Div)
            || self.consume_exact_operator(":", TokenKind::Colon)
            || self.consume_exact_operator("<<=", TokenKind::SHLAssign)
            || self.consume_exact_operator("<<", TokenKind::SHL)
            || self.consume_exact_operator("<=", TokenKind::LTEq)
            || self.consume_exact_operator("<", TokenKind::LT)
            || self.consume_exact_operator(">>=", TokenKind::SHRAssign)
            || self.consume_exact_operator(">>", TokenKind::SHR)
            || self.consume_exact_operator(">=", TokenKind::GTEq)
            || self.consume_exact_operator(">", TokenKind::GT)
            || self.consume_exact_operator("==", TokenKind::Eq)
            || self.consume_exact_operator("=", TokenKind::Assign)
            || self.consume_exact_operator(",", TokenKind::Comma)
            || self.consume_exact_operator(".", TokenKind::Dot);

        if !found {
            let char_pos = self.char_posts[self.char_offset];
            return Err(Error::UnexpectedSymbol {
                symbol: char_pos.val,
                pos: char_pos.pos,
            });
        }
        Ok(())
    }

    fn consume_exact_operator(&mut self, op: &str, token_kind: TokenKind) -> bool {
        if self.next_is(op) {
            self.emit_token(token_kind, None, self.char_posts[self.char_offset].pos);
            self.char_offset += op.len();
            return true;
        }
        false
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

    fn emit_token(&mut self, kind: TokenKind, value: Option<String>, pos: Pos) {
        self.tokens.push_back(Token { kind, value, pos })
    }
}

impl<T: Read> Lexer for SimpleLexer<T> {
    fn next(&mut self) -> Result<Token, Error> {
        self.load_tokens()?;

        if let Some(token) = self.tokens.pop_front() {
            return Ok(token);
        }

        return Ok(self.token_eoi.clone());
    }

    fn peek(&mut self) -> Result<&Token, Error> {
        self.load_tokens()?;

        if let Some(token) = self.tokens.front() {
            return Ok(token);
        }

        return Ok(&self.token_eoi);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_fn() {
        let simple_fn = r#"
            fn gcd(a: i32, b: i64): i64 {
                while b != 0 {
                    var t = b;
                    b = a % b;
                    b = t;
                }
                return a;
            }
            "#
        .as_bytes();
        let mut lexer = SimpleLexer::new(simple_fn);

        let result = lexer.next().unwrap();
        assert_eq!(
            result,
            Token {
                kind: TokenKind::Fn,
                value: None,
                pos: Pos { line: 1, col: 1 }
            },
        );
    }
}
