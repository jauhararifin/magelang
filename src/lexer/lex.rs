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

trait Consumer<R: Read> {
    fn consume(&self, reader: &mut CharReader<R>) -> Result<Option<Token>>;
}

pub struct Lexer<R: Read> {
    reader: CharReader<R>,
    file_name: Rc<String>,

    rules: Vec<(String, Box<dyn Consumer<R>>)>,

    tokens: VecDeque<Token>,
    token_eoi: Option<Token>,
}

impl<R: Read> Lexer<R> {
    pub fn new(reader: R, file_name: &str) -> Self {
        Self {
            reader: CharReader::new(reader, Rc::new(String::from(file_name))),
            file_name: Rc::new(String::from(file_name)),

            rules: Self::generate_rules(),

            tokens: VecDeque::new(),
            token_eoi: None,
        }
    }

    fn generate_rules() -> Vec<(String, Box<dyn Consumer<R>>)> {
        let mut rules = Vec::<(String, Box<dyn Consumer<R>>)>::new();

        for i in [' ', '\r', '\t'] {
            rules.push((String::from(i), Box::new(WhitespaceConsumer())));
        }

        rules.push(("//".to_string(), Box::new(CommentConsumer())));
        rules.push(("\n".to_string(), Box::new(NewlineConsumer())));

        for i in "0123456789".chars() {
            rules.push((String::from(i), Box::new(NumberLitConsumer())));
        }

        for i in ['\"', '`'] {
            rules.push((String::from(i), Box::new(StringLitConsumer())));
        }

        Self::generate_keyword_rule(&mut rules, "!=", TokenKind::NotEq);
        Self::generate_keyword_rule(&mut rules, "!", TokenKind::Not);
        Self::generate_keyword_rule(&mut rules, "%=", TokenKind::ModAssign);
        Self::generate_keyword_rule(&mut rules, "%", TokenKind::Mod);
        Self::generate_keyword_rule(&mut rules, "&&", TokenKind::And);
        Self::generate_keyword_rule(&mut rules, "&=", TokenKind::BitAndAssign);
        Self::generate_keyword_rule(&mut rules, "&", TokenKind::BitAnd);
        Self::generate_keyword_rule(&mut rules, "||", TokenKind::Or);
        Self::generate_keyword_rule(&mut rules, "|=", TokenKind::BitOrAssign);
        Self::generate_keyword_rule(&mut rules, "|", TokenKind::BitOr);
        Self::generate_keyword_rule(&mut rules, "^=", TokenKind::BitXorAssign);
        Self::generate_keyword_rule(&mut rules, "^", TokenKind::BitXor);
        Self::generate_keyword_rule(&mut rules, "~", TokenKind::BitNot);
        Self::generate_keyword_rule(&mut rules, "(", TokenKind::OpenBrace);
        Self::generate_keyword_rule(&mut rules, ")", TokenKind::CloseBrace);
        Self::generate_keyword_rule(&mut rules, "{", TokenKind::OpenBlock);
        Self::generate_keyword_rule(&mut rules, "}", TokenKind::CloseBlock);
        Self::generate_keyword_rule(&mut rules, "[", TokenKind::OpenBrack);
        Self::generate_keyword_rule(&mut rules, "]", TokenKind::CloseBrack);
        Self::generate_keyword_rule(&mut rules, "*=", TokenKind::MulAssign);
        Self::generate_keyword_rule(&mut rules, "*", TokenKind::Mul);
        Self::generate_keyword_rule(&mut rules, "+=", TokenKind::PlusAssign);
        Self::generate_keyword_rule(&mut rules, "+", TokenKind::Plus);
        Self::generate_keyword_rule(&mut rules, "-=", TokenKind::MinusAssign);
        Self::generate_keyword_rule(&mut rules, "-", TokenKind::Minus);
        Self::generate_keyword_rule(&mut rules, "/=", TokenKind::DivAssign);
        Self::generate_keyword_rule(&mut rules, "/", TokenKind::Div);
        Self::generate_keyword_rule(&mut rules, ":", TokenKind::Colon);
        Self::generate_keyword_rule(&mut rules, "<<=", TokenKind::ShlAssign);
        Self::generate_keyword_rule(&mut rules, "<<", TokenKind::Shl);
        Self::generate_keyword_rule(&mut rules, "<=", TokenKind::LTEq);
        Self::generate_keyword_rule(&mut rules, "<", TokenKind::LT);
        Self::generate_keyword_rule(&mut rules, ">>=", TokenKind::ShrAssign);
        Self::generate_keyword_rule(&mut rules, ">>", TokenKind::Shr);
        Self::generate_keyword_rule(&mut rules, ">=", TokenKind::GTEq);
        Self::generate_keyword_rule(&mut rules, ">", TokenKind::GT);
        Self::generate_keyword_rule(&mut rules, "==", TokenKind::Eq);
        Self::generate_keyword_rule(&mut rules, "=", TokenKind::Assign);
        Self::generate_keyword_rule(&mut rules, ",", TokenKind::Comma);

        Self::generate_keyword_rule(&mut rules, "if", TokenKind::If);
        Self::generate_keyword_rule(&mut rules, "native", TokenKind::Native);
        Self::generate_keyword_rule(&mut rules, "var", TokenKind::Var);
        Self::generate_keyword_rule(&mut rules, "while", TokenKind::While);
        Self::generate_keyword_rule(&mut rules, "fn", TokenKind::Fn);
        Self::generate_keyword_rule(&mut rules, "as", TokenKind::As);
        Self::generate_keyword_rule(&mut rules, "return", TokenKind::Return);
        Self::generate_keyword_rule(&mut rules, "bool", TokenKind::Bool);
        Self::generate_keyword_rule(&mut rules, "i8", TokenKind::I8);
        Self::generate_keyword_rule(&mut rules, "i16", TokenKind::I16);
        Self::generate_keyword_rule(&mut rules, "i32", TokenKind::I32);
        Self::generate_keyword_rule(&mut rules, "i64", TokenKind::I64);
        Self::generate_keyword_rule(&mut rules, "u8", TokenKind::U8);
        Self::generate_keyword_rule(&mut rules, "u16", TokenKind::U16);
        Self::generate_keyword_rule(&mut rules, "u32", TokenKind::U32);
        Self::generate_keyword_rule(&mut rules, "u64", TokenKind::U64);
        Self::generate_keyword_rule(&mut rules, "f32", TokenKind::F32);
        Self::generate_keyword_rule(&mut rules, "f64", TokenKind::F64);
        Self::generate_keyword_rule(&mut rules, "true", TokenKind::True);
        Self::generate_keyword_rule(&mut rules, "false", TokenKind::False);

        for i in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_".chars() {
            rules.push((String::from(i), Box::new(IdentConsumer())));
        }

        rules
    }

    fn generate_keyword_rule(rules: &mut Vec<(String, Box<dyn Consumer<R>>)>, keyword: &'static str, kind: TokenKind) {
        rules.push((keyword.to_string(), Box::new(KeywordConsumer::new(keyword, kind))));
    }
}

impl<T: Read> ILexer for Lexer<T> {
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

impl<R: Read> Lexer<R> {
    fn advance(&mut self) -> Result<()> {
        if let Some(_) = self.token_eoi {
            return Ok(());
        }

        while self.tokens.len() == 0 {
            let mut advanced = false;
            for rule in self.rules.iter() {
                let (pattern, consumer) = rule;
                if self.reader.starts_with(pattern)? {
                    if let Some(token) = consumer.consume(&mut self.reader)? {
                        self.tokens.push_back(token);
                        advanced = true;
                        break;
                    }
                }
            }

            if !advanced {
                if let Some(ch) = self.reader.next()? {
                    return Err(Error::UnexpectedChar {
                        char: ch.value as u32,
                        pos: ch.pos,
                    });
                } else {
                    self.put_eoi();
                    break;
                }
            }
        }

        Ok(())
    }

    fn put_eoi(&mut self) {
        self.token_eoi = Some(Token {
            kind: TokenKind::Eoi,
            value: None,
            pos: Pos {
                file_name: self.file_name.clone(),
                line: 0,
                col: 0,
            },
        });
    }
}

struct CommentConsumer();

impl<R: Read> Consumer<R> for CommentConsumer {
    fn consume(&self, reader: &mut CharReader<R>) -> Result<Option<Token>> {
        let c = reader.next()?.unwrap();
        reader.next()?;

        let value = reader.consume_while(|c| c != '\n')?;
        Ok(Some(Token {
            kind: TokenKind::Comment,
            value: Some(Rc::new(value)),
            pos: c.pos,
        }))
    }
}

struct NewlineConsumer();

impl<R: Read> Consumer<R> for NewlineConsumer {
    fn consume(&self, reader: &mut CharReader<R>) -> Result<Option<Token>> {
        let c = reader.next()?.unwrap();
        Ok(Some(Token {
            kind: TokenKind::Endl,
            value: None,
            pos: c.pos,
        }))
    }
}

struct WhitespaceConsumer();

impl<R: Read> Consumer<R> for WhitespaceConsumer {
    fn consume(&self, reader: &mut CharReader<R>) -> Result<Option<Token>> {
        reader.consume_while(|c| c == ' ' || c == '\t' || c == '\r')?;
        Ok(None)
    }
}

struct NumberLitConsumer();

impl<R: Read> Consumer<R> for NumberLitConsumer {
    fn consume(&self, reader: &mut CharReader<R>) -> Result<Option<Token>> {
        let mut value = String::new();

        let ch = reader.next()?.unwrap();
        let pos = ch.pos;
        value.push(ch.value);

        let mut base = 10;
        if ch.value == '0' {
            base = match reader.peek_char()? {
                Some('b' | 'B') => 2,
                Some('o' | 'O') => 8,
                Some('x' | 'X') => 16,
                Some('0'..='7') => 8,
                _ => {
                    return Ok(Some(Token {
                        kind: TokenKind::IntegerLit,
                        value: Some(Rc::new("0".to_string())),
                        pos,
                    }))
                }
            };
            let ch = reader.next()?.unwrap();
            value.push(ch.value);
        }

        let digit_matcher = match base {
            2 => |c| c == '0' || c == '1' || c == '_',
            8 => |c| (c >= '0' && c <= '7') || c == '_',
            10 => |c| (c >= '0' && c <= '9') || c == '_',
            16 => |c| (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || c == '_',
            _ => unreachable!(),
        };
        value.push_str(reader.consume_while(digit_matcher)?.as_str());

        let mut kind = TokenKind::IntegerLit;
        if let Some('.') = reader.peek_char()? {
            let ch = reader.next()?.unwrap();
            value.push(ch.value);
            value.push_str(reader.consume_while(digit_matcher)?.as_str());
            kind = TokenKind::FloatLit;
        }

        if let Some('e' | 'E') = reader.peek_char()? {
            let ch = reader.next()?.unwrap();
            value.push(ch.value);
            value.push_str(reader.consume_while(digit_matcher)?.as_str());
            kind = TokenKind::FloatLit;
        }

        Ok(Some(Token {
            kind,
            value: Some(Rc::new(value)),
            pos,
        }))
    }
}

struct StringLitConsumer();

impl<R: Read> Consumer<R> for StringLitConsumer {
    fn consume(&self, reader: &mut CharReader<R>) -> Result<Option<Token>> {
        let opening_quote = reader.next()?.unwrap();

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

        let mut value = String::new();
        let pos = opening_quote.pos;

        let mut after_backslash = false;
        while let Some(ch) = reader.next()? {
            let c = ch.value;

            if c == '\n' {
                return Err(Error::UnexpectedSymbol {
                    symbol: "\n".to_string(),
                    pos: ch.pos,
                });
            }

            if after_backslash {
                if let Some(v) = backslash_chars.get(&c) {
                    value.push(*v);
                } else {
                    return Err(Error::UnexpectedSymbol {
                        symbol: String::from(c),
                        pos: ch.pos,
                    });
                }
                after_backslash = false;
            } else if c == '\\' {
                after_backslash = true;
            } else if c == opening_quote.value {
                break;
            } else {
                value.push(c);
            }
        }

        Ok(Some(Token {
            kind: TokenKind::StringLit,
            value: Some(Rc::new(value)),
            pos,
        }))
    }
}
struct KeywordConsumer {
    keyword: &'static str,
    kind: TokenKind,
}

impl KeywordConsumer {
    fn new(keyword: &'static str, kind: TokenKind) -> Self {
        Self { keyword, kind }
    }
}

impl<R: Read> Consumer<R> for KeywordConsumer {
    fn consume(&self, reader: &mut CharReader<R>) -> Result<Option<Token>> {
        let ch = reader.next()?.unwrap();
        for _ in 0..(self.keyword.len() - 1) {
            reader.next()?;
        }
        Ok(Some(Token {
            kind: self.kind.clone(),
            value: None,
            pos: ch.pos,
        }))
    }
}

struct IdentConsumer();

impl<R: Read> Consumer<R> for IdentConsumer {
    fn consume(&self, reader: &mut CharReader<R>) -> Result<Option<Token>> {
        let ch = reader.next()?.unwrap();
        let mut value = String::from(ch.value);
        let pos = ch.pos;

        value.push_str(
            reader
                .consume_while(|c| matches!(c, '_' | '0'..='9' | 'a'..='z' | 'A'..='Z'))?
                .as_str(),
        );

        Ok(Some(Token {
            kind: TokenKind::Ident,
            value: Some(Rc::new(value)),
            pos,
        }))
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
                value: None,
                pos: Pos {
                    file_name: Rc::new("anonymous".to_string()),
                    line: 1,
                    col: 1
                }
            },
        );
    }
}
