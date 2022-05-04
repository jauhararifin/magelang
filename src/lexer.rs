use crate::errors::Error;
use crate::pos::Pos;
use crate::token::{Token, TokenKind};
use std::collections::{HashMap, VecDeque};
use std::io::{Bytes, Read};
use unicode_reader::CodePoints;

type Result<T> = std::result::Result<T, Error>;

pub trait Lexer {
    fn next(&mut self) -> Result<Token>;
    fn peek(&mut self) -> Result<&Token>;
}

pub struct SimpleLexer<R: Read> {
    reader: CodePoints<Bytes<R>>,

    current_line: usize,
    current_col: usize,
    next_char_pos: Option<CharPos>,

    tokens: VecDeque<Token>,
    token_eoi: Option<Token>,
}

#[derive(Copy, Clone, Debug)]
struct CharPos {
    val: char,
    pos: Pos,
}

impl<R: Read> SimpleLexer<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader: CodePoints::from(reader),

            current_line: 1,
            current_col: 1,
            next_char_pos: None,

            tokens: VecDeque::new(),
            token_eoi: None,
        }
    }

    fn advance(&mut self) -> Result<()> {
        if let Some(_) = self.token_eoi {
            return Ok(());
        }

        while self.tokens.len() == 0 {
            let mut char_pos = self.next_char_pos;
            if char_pos.is_none() {
                char_pos = self.next_char()?;
            }

            if char_pos.is_none() {
                self.put_eoi();
                return Ok(());
            }

            let char_pos = char_pos.unwrap();
            self.next_char_pos = self.process(char_pos)?;
        }

        Ok(())
    }

    fn next_char(&mut self) -> Result<Option<CharPos>> {
        let next_char = self.reader.next();
        if next_char.is_none() {
            return Ok(None);
        }

        let next_char = next_char.unwrap()?;
        let char_pos = CharPos {
            val: next_char,
            pos: Pos {
                line: self.current_line,
                col: self.current_col,
            },
        };

        if next_char == '\n' {
            self.current_col = 1;
            self.current_line += 1;
        } else {
            self.current_col += 1;
        }

        Ok(Some(char_pos))
    }

    fn put_eoi(&mut self) {
        self.token_eoi = Some(Token {
            kind: TokenKind::Eoi,
            value: None,
            pos: Pos {
                line: self.current_line,
                col: self.current_col,
            },
        });
    }

    fn process(&mut self, char_pos: CharPos) -> Result<Option<CharPos>> {
        Ok(match char_pos.val {
            '#' => self.consume_comment(char_pos)?,
            '\n' => self.consume_newline(char_pos)?,
            '\"' => self.consume_string_literal(char_pos)?,
            '0'..='9' => self.consume_number_literal(char_pos)?,
            '_' | 'a'..='z' | 'A'..='Z' => self.consume_word(char_pos)?,
            ' ' | '\r' | '\t' => self.next_char()?,
            _ => self.process_op(char_pos)?,
        })
    }

    fn consume_comment(&mut self, char_pos: CharPos) -> Result<Option<CharPos>> {
        let (value, _) = self.consume_while_match(|c| c != '\n')?;
        self.emit_token(TokenKind::Comment, Some(value), char_pos.pos);
        Ok(None)
    }

    fn consume_newline(&mut self, char_pos: CharPos) -> Result<Option<CharPos>> {
        self.emit_token(TokenKind::Endl, None, char_pos.pos);
        Ok(None)
    }

    fn consume_number_literal(&mut self, char_pos: CharPos) -> Result<Option<CharPos>> {
        let mut lit = char_pos.val.to_string();
        let (value, next) = self.consume_while_match(|c| c.is_digit(10) || c == '_' || c == '.')?;
        lit.push_str(value.as_str());

        if value.contains('.') {
            self.emit_token(TokenKind::FloatLit, Some(lit), char_pos.pos);
        } else {
            self.emit_token(TokenKind::IntegerLit, Some(lit), char_pos.pos);
        }
        Ok(next)
    }

    fn consume_string_literal(&mut self, char_pos: CharPos) -> Result<Option<CharPos>> {
        let opening_quote = char_pos.val;

        let backslash_chars = HashMap::from([
            ('n', '\n'),
            ('r', '\r'),
            ('"', '"'),
            ('`', '`'),
            ('\\', '\\'),
            ('t', '\t'),
        ]);

        let mut value = String::new();
        let pos = char_pos.pos;
        let mut after_backslash = false;

        while let Some(char_pos) = self.next_char()? {
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
            } else if c == '\\' {
                after_backslash = true;
            } else if c == opening_quote {
                if after_backslash {
                    return Err(Error::UnexpectedSymbol {
                        symbol: c,
                        pos: char_pos.pos,
                    });
                }
                break;
            } else {
                value.push(c);
            }
        }

        self.emit_token(TokenKind::StringLit, Some(value), pos);
        Ok(None)
    }

    fn consume_word(&mut self, char_pos: CharPos) -> Result<Option<CharPos>> {
        let mut word = char_pos.val.to_string();
        let (name, next) = self.consume_while_match(|c| c.is_alphabetic() || c == '_' || c.is_digit(10))?;
        word.push_str(name.as_str());

        match word.as_str() {
            "if" => self.emit_token(TokenKind::If, None, char_pos.pos),
            "native" => self.emit_token(TokenKind::Native, None, char_pos.pos),
            "var" => self.emit_token(TokenKind::Var, None, char_pos.pos),
            "while" => self.emit_token(TokenKind::While, None, char_pos.pos),
            "fn" => self.emit_token(TokenKind::Fn, None, char_pos.pos),
            "as" => self.emit_token(TokenKind::As, None, char_pos.pos),
            "return" => self.emit_token(TokenKind::Return, None, char_pos.pos),
            "bool" => self.emit_token(TokenKind::Bool, None, char_pos.pos),
            "i8" => self.emit_token(TokenKind::I8, None, char_pos.pos),
            "i16" => self.emit_token(TokenKind::I16, None, char_pos.pos),
            "i32" => self.emit_token(TokenKind::I32, None, char_pos.pos),
            "i64" => self.emit_token(TokenKind::I64, None, char_pos.pos),
            "u8" => self.emit_token(TokenKind::U8, None, char_pos.pos),
            "u16" => self.emit_token(TokenKind::U16, None, char_pos.pos),
            "u32" => self.emit_token(TokenKind::U32, None, char_pos.pos),
            "u64" => self.emit_token(TokenKind::U64, None, char_pos.pos),
            "f32" => self.emit_token(TokenKind::F32, None, char_pos.pos),
            "f64" => self.emit_token(TokenKind::F64, None, char_pos.pos),
            "true" => self.emit_token(TokenKind::True, None, char_pos.pos),
            "false" => self.emit_token(TokenKind::False, None, char_pos.pos),
            _ => self.emit_token(TokenKind::Ident, Some(word), char_pos.pos),
        }

        Ok(next)
    }

    fn process_op(&mut self, char_pos: CharPos) -> Result<Option<CharPos>> {
        let operators: HashMap<&'static str, TokenKind> = HashMap::from([
            ("!=", TokenKind::NotEq),
            ("!", TokenKind::Not),
            ("%=", TokenKind::ModAssign),
            ("%", TokenKind::Mod),
            ("&&", TokenKind::And),
            ("&=", TokenKind::BitAndAssign),
            ("&", TokenKind::BitAnd),
            ("||", TokenKind::Or),
            ("|=", TokenKind::BitOrAssign),
            ("|", TokenKind::BitOr),
            ("^=", TokenKind::BitXorAssign),
            ("^", TokenKind::BitXor),
            ("~", TokenKind::BitNot),
            ("(", TokenKind::OpenBrace),
            (")", TokenKind::CloseBrace),
            ("{", TokenKind::OpenBlock),
            ("}", TokenKind::CloseBlock),
            ("*=", TokenKind::MulAssign),
            ("*", TokenKind::Mul),
            ("+=", TokenKind::PlusAssign),
            ("+", TokenKind::Plus),
            ("-=", TokenKind::MinusAssign),
            ("-", TokenKind::Minus),
            ("/=", TokenKind::DivAssign),
            ("/", TokenKind::Div),
            (":", TokenKind::Colon),
            ("<<=", TokenKind::ShlAssign),
            ("<<", TokenKind::Shl),
            ("<=", TokenKind::LTEq),
            ("<", TokenKind::LT),
            (">>=", TokenKind::ShrAssign),
            (">>", TokenKind::Shr),
            (">=", TokenKind::GTEq),
            (">", TokenKind::GT),
            ("==", TokenKind::Eq),
            ("=", TokenKind::Assign),
            (",", TokenKind::Comma),
        ]);

        let mut char_pos = char_pos;
        let pos = char_pos.pos;
        let mut matched_str = String::new();
        let mut current_str = char_pos.val.to_string();

        let next = loop {
            let matches = operators
                .iter()
                .filter(|op| op.0.starts_with(current_str.as_str()))
                .count();

            if matches == 0 {
                break Some(char_pos);
            } else if matches == 1 {
                matched_str.push(char_pos.val);
                break None;
            } else if let Some(next) = self.next_char()? {
                matched_str = current_str.clone();
                char_pos = next;
                current_str.push(char_pos.val);
            } else {
                break None;
            }
        };

        if let Some(kind) = operators.get(matched_str.as_str()) {
            self.emit_token(kind.clone(), None, pos);
        } else {
            return Err(Error::UnexpectedSymbol {
                symbol: char_pos.val,
                pos: char_pos.pos,
            });
        }

        Ok(next)
    }

    fn consume_while_match<F: Fn(char) -> bool>(&mut self, matcher: F) -> Result<(String, Option<CharPos>)> {
        let mut result = String::new();
        while let Some(char_pos) = self.next_char()? {
            let c = char_pos.val;
            if matcher(c) {
                result.push(c);
            } else {
                return Ok((result, Some(char_pos)));
            }
        }

        Ok((result, None))
    }

    fn emit_token(&mut self, kind: TokenKind, value: Option<String>, pos: Pos) {
        self.tokens.push_back(Token { kind, value, pos })
    }
}

impl<T: Read> Lexer for SimpleLexer<T> {
    fn next(&mut self) -> Result<Token> {
        self.advance()?;
        if let Some(token) = self.tokens.pop_front() {
            return Ok(token);
        }
        Ok(self.token_eoi.as_ref().unwrap().clone())
    }

    fn peek(&mut self) -> Result<&Token> {
        self.advance()?;
        if let Some(token) = self.tokens.front() {
            return Ok(token);
        }
        Ok(self.token_eoi.as_ref().unwrap())
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
