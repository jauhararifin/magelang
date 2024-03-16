use crate::error::ErrorReporter;
use crate::number::Number;
use crate::token::{File, Pos, Token, TokenKind};
use num::BigInt;

pub(crate) fn scan(errors: &impl ErrorReporter, file: &File) -> Vec<Token> {
    let mut scanner = Scanner::new(errors, file);
    let mut tokens = Vec::default();
    while let Some(token) = scanner.scan() {
        tokens.push(token);
    }
    tokens
}

struct Scanner<'a, Error> {
    errors: &'a Error,
    file_offset: Pos,
    text: &'a str,
    offset: usize,
}

impl<'a, Error: ErrorReporter> Scanner<'a, Error> {
    fn new(errors: &'a Error, file: &'a File) -> Self {
        Self {
            errors,
            file_offset: file.offset,
            text: &file.text,
            offset: 0,
        }
    }

    fn scan(&mut self) -> Option<Token> {
        self.skip_whitespace();
        self.scan_word()
            .or_else(|| self.scan_char_lit())
            .or_else(|| self.scan_string_lit())
            .or_else(|| self.scan_number_lit())
            .or_else(|| self.scan_comments())
            .or_else(|| self.scan_symbols())
            .or_else(|| self.scan_invalid())
    }

    fn skip_whitespace(&mut self) {
        while let Some((ch, _)) = self.peek() {
            if ch.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }

    fn scan_word(&mut self) -> Option<Token> {
        let initial = |c: char| c.is_alphabetic() || c == '_';
        let (c, pos) = self.next_if(initial)?;

        let mut value = String::from(c);
        let valid_char = |c: char| c.is_alphabetic() || c.is_ascii_digit() || c == '_';
        while let Some((c, _)) = self.next_if(valid_char) {
            value.push(c);
        }

        let kind = match value.as_str() {
            "let" => TokenKind::Let,
            "struct" => TokenKind::Struct,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "fn" => TokenKind::Fn,
            "return" => TokenKind::Return,
            "import" => TokenKind::Import,
            "as" => TokenKind::As,
            "null" => TokenKind::Null,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "continue" => TokenKind::Continue,
            "break" => TokenKind::Break,
            _ => TokenKind::Ident,
        };
        let value_str = if kind == TokenKind::Ident {
            value
        } else {
            String::default()
        };

        Some(Token {
            kind,
            value_str,
            value_bytes: Vec::default(),
            char_value: '\0',
            value_number: Number::default(),
            pos,
        })
    }

    fn scan_char_lit(&mut self) -> Option<Token> {
        let (_, pos) = self.next_if(|c| c == '\'')?;
        let raw = String::from("\'");

        if let Some((c, _)) = self.peek() {
            match c {
                '\\' => self.scan_char_after_backslash(pos, raw),
                '\'' => self.scan_char_closing(pos, raw, 0 as char),
                _ => {
                    self.next();
                    self.scan_char_closing(pos, raw, c)
                }
            }
        } else {
            self.scan_char_closing(pos, raw, 0 as char)
        }
    }

    fn scan_char_after_backslash(&mut self, pos: Pos, mut raw: String) -> Option<Token> {
        self.next();
        raw.push('\\');

        let Some((c, p)) = self.next() else {
            return self.scan_char_closing(pos, raw, 0 as char);
        };

        raw.push(c);
        match c {
            'n' => self.scan_char_closing(pos, raw, '\n'),
            'r' => self.scan_char_closing(pos, raw, '\r'),
            't' => self.scan_char_closing(pos, raw, '\t'),
            '\\' => self.scan_char_closing(pos, raw, '\\'),
            '0' => self.scan_char_closing(pos, raw, 0 as char),
            '\'' => self.scan_char_closing(pos, raw, '\''),
            'x' => self.scan_char_hex(pos, raw),
            _ => {
                self.errors.unexpected_char(p, c);
                self.scan_char_closing(pos, raw, 0 as char)
            }
        }
    }

    fn scan_char_hex(&mut self, pos: Pos, mut raw: String) -> Option<Token> {
        let Some((c, p)) = self.next() else {
            return self.scan_char_closing(pos, raw, 0 as char);
        };
        raw.push(c);

        match c {
            '0'..='9' | 'a'..='f' | 'A'..='F' => {
                let value = Self::char_to_int(c);

                let Some((c, p)) = self.next() else {
                    return self.scan_char_closing(pos, raw, 0 as char);
                };
                raw.push(c);

                match c {
                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        let value = value << 4 | Self::char_to_int(c);
                        self.scan_char_closing(pos, raw, value as char)
                    }
                    _ => {
                        self.errors.unexpected_char(p, c);
                        self.scan_char_closing(pos, raw, 0 as char)
                    }
                }
            }
            _ => {
                self.errors.unexpected_char(p, c);
                self.scan_char_closing(pos, raw, 0 as char)
            }
        }
    }

    fn char_to_int(c: char) -> u8 {
        match c {
            '0'..='9' => c as u8 - b'0',
            'a'..='f' => c as u8 - b'a' + 0xa,
            'A'..='F' => c as u8 - b'A' + 0xa,
            _ => 0,
        }
    }

    fn scan_char_closing(&mut self, pos: Pos, mut raw: String, value: char) -> Option<Token> {
        let mut found_multichar = false;
        loop {
            let Some((c, p)) = self.next() else {
                self.errors.missing_closing_quote(self.get_pos());
                return Some(Token {
                    kind: TokenKind::CharLit,
                    value_str: raw,
                    value_bytes: Vec::default(),
                    char_value: value,
                    value_number: Number::default(),
                    pos,
                });
            };
            raw.push(c);

            if c == '\'' {
                return Some(Token {
                    kind: TokenKind::CharLit,
                    value_str: raw,
                    value_bytes: Vec::default(),
                    char_value: value,
                    value_number: Number::default(),
                    pos,
                });
            }

            if !found_multichar {
                self.errors.multiple_char_in_literal(p);
                found_multichar = true;
            }
        }
    }

    fn scan_string_lit(&mut self) -> Option<Token> {
        let (_, pos) = self.next_if(|c| c == '"')?;
        let raw = String::from("\"");
        let value = Vec::default();

        self.scan_string_internal(pos, raw, value)
    }

    fn scan_string_internal(
        &mut self,
        pos: Pos,
        mut raw: String,
        mut value: Vec<u8>,
    ) -> Option<Token> {
        while let Some((c, _)) = self.peek() {
            match c {
                '\\' => self.scan_string_after_backslash(&mut raw, &mut value),
                '"' => return self.scan_string_closing(pos, raw, value),
                _ => {
                    self.next();
                    raw.push(c);
                    let buff = &mut [0u8; 4];
                    value.extend_from_slice(c.encode_utf8(buff).as_bytes());
                }
            }
        }
        self.scan_string_closing(pos, raw, value)
    }

    fn scan_string_after_backslash(&mut self, raw: &mut String, value: &mut Vec<u8>) {
        let (c, _) = self.next().unwrap();
        assert_eq!(c, '\\');
        raw.push('\\');

        let Some((c, p)) = self.next() else {
            return;
        };

        raw.push(c);
        match c {
            'n' => value.push('\n' as u8),
            'r' => value.push('\r' as u8),
            't' => value.push('\t' as u8),
            '\\' => value.push('\\' as u8),
            '0' => value.push(0),
            '\'' => value.push('\'' as u8),
            '"' => value.push('"' as u8),
            'x' => self.scan_string_hex(raw, value),
            _ => self.errors.unexpected_char(p, c),
        }
    }

    fn scan_string_hex(&mut self, raw: &mut String, value: &mut Vec<u8>) {
        let Some((c, p)) = self.peek() else { return };

        if !matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F') {
            self.errors.unexpected_char(p, c);
            return;
        }

        raw.push(c);
        self.next();
        let char_val = Self::char_to_int(c);

        let Some((c, p)) = self.peek() else { return };

        if !matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F') {
            self.errors.unexpected_char(p, c);
            return;
        }

        raw.push(c);
        self.next();
        let char_val = char_val << 4 | Self::char_to_int(c);

        value.push(char_val);
    }

    fn scan_string_closing(&mut self, pos: Pos, mut raw: String, value: Vec<u8>) -> Option<Token> {
        let Some((c, _)) = self.next() else {
            self.errors.missing_closing_quote(self.get_pos());
            return Some(Token {
                kind: TokenKind::StringLit,
                value_str: raw,
                value_bytes: value,
                char_value: '\0',
                value_number: Number::default(),
                pos,
            });
        };
        raw.push(c);
        assert_eq!(c, '\"');

        Some(Token {
            kind: TokenKind::StringLit,
            value_str: raw,
            value_bytes: value,
            char_value: '\0',
            value_number: Number::default(),
            pos,
        })
    }

    fn scan_number_lit(&mut self) -> Option<Token> {
        let (c, _) = self.peek()?;
        match c {
            '0' => self.scan_number_prefix(),
            '1'..='9' => {
                let mut raw = String::default();
                let mut value = Number::default();

                let (c, pos) = self.next().unwrap();
                raw.push(c);
                value.val = BigInt::from(Self::char_to_int(c));
                self.scan_number_base(Base::Dec, pos, raw, value)
            }
            _ => None,
        }
    }

    fn scan_number_prefix(&mut self) -> Option<Token> {
        let (c, pos) = self.next().unwrap();
        assert_eq!(c, '0');

        let mut raw = String::from("0");
        let mut value = Number::new(BigInt::default(), BigInt::default());

        let Some((c, _)) = self.scan_number_peek_with_skip_underscore(&mut raw) else {
            return Some(Token {
                kind: TokenKind::NumberLit,
                value_str: raw,
                value_bytes: Vec::default(),
                char_value: 0 as char,
                value_number: value,
                pos,
            });
        };

        match c {
            'x' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                self.scan_number_base(Base::Hex, pos, raw, value)
            }
            'b' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                self.scan_number_base(Base::Bin, pos, raw, value)
            }
            'o' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                self.scan_number_base(Base::Oct, pos, raw, value)
            }
            '0'..='7' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                value.val = value.val * 8 + Self::char_to_int(c);
                self.scan_number_base(Base::Oct, pos, raw, value)
            }
            'e' | 'E' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                self.scan_number_exponent(pos, raw, value)
            }
            '.' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                self.scan_number_fraction(pos, raw, value)
            }
            'a'..='z' | 'A'..='Z' => self.scan_number_invalid_suffix(pos, raw, value),
            _ => Some(Token {
                kind: TokenKind::NumberLit,
                value_str: raw,
                value_bytes: Vec::default(),
                char_value: 0 as char,
                value_number: value,
                pos,
            }),
        }
    }

    fn scan_number_peek_with_skip_underscore(&mut self, raw: &mut String) -> Option<(char, Pos)> {
        while let Some((c, _)) = self.peek() {
            if c != '_' {
                break;
            }
            self.next();
            raw.push(c);
        }

        self.peek()
    }

    fn scan_number_base(
        &mut self,
        base: Base,
        pos: Pos,
        mut raw: String,
        mut value: Number,
    ) -> Option<Token> {
        while let Some((c, p)) = self.scan_number_peek_with_skip_underscore(&mut raw) {
            match (base, c) {
                (Base::Dec, 'e' | 'E') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    return self.scan_number_exponent(pos, raw, value);
                }
                (Base::Dec, '.') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    return self.scan_number_fraction(pos, raw, value);
                }
                (_, '.') => break,
                (Base::Bin, '0' | '1') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    value.val = value.val * 2 + Self::char_to_int(c);
                }
                (Base::Dec, '0'..='9') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    value.val = value.val * 10 + Self::char_to_int(c);
                }
                (Base::Oct, '0'..='7') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    value.val = value.val * 8 + Self::char_to_int(c);
                }
                (Base::Hex, '0'..='9' | 'a'..='f' | 'A'..='F') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    value.val = value.val * 16 + Self::char_to_int(c);
                }
                (Base::Bin, '2'..='9') | (Base::Oct, '8'..='9') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    self.errors.invalid_digit_in_base(p, c, base as u8);
                }
                (Base::Bin | Base::Dec | Base::Oct, 'a'..='z' | 'A'..='Z')
                | (Base::Hex, 'g'..='z' | 'G'..='Z') => {
                    return self.scan_number_invalid_suffix(pos, raw, value);
                }
                _ => break,
            }
        }

        Some(Token {
            kind: TokenKind::NumberLit,
            value_str: raw,
            value_bytes: Vec::default(),
            char_value: 0 as char,
            value_number: value,
            pos,
        })
    }

    fn scan_number_fraction(
        &mut self,
        pos: Pos,
        mut raw: String,
        mut value: Number,
    ) -> Option<Token> {
        while let Some((c, _)) = self.scan_number_peek_with_skip_underscore(&mut raw) {
            match c {
                'e' | 'E' => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    return self.scan_number_exponent(pos, raw, value);
                }
                '0'..='9' => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    value.val = value.val * 10 + Self::char_to_int(c);
                    value.exp -= 1;
                }
                'a'..='z' | 'A'..='Z' => return self.scan_number_invalid_suffix(pos, raw, value),
                _ => break,
            }
        }

        Some(Token {
            kind: TokenKind::NumberLit,
            value_str: raw,
            value_bytes: Vec::default(),
            char_value: 0 as char,
            value_number: value,
            pos,
        })
    }

    fn scan_number_exponent(&mut self, pos: Pos, mut raw: String, value: Number) -> Option<Token> {
        let Some((c, p)) = self.scan_number_peek_with_skip_underscore(&mut raw) else {
            self.errors.missing_exponent_digits(self.get_pos());
            return Some(Token {
                kind: TokenKind::NumberLit,
                value_str: raw,
                value_bytes: Vec::default(),
                char_value: 0 as char,
                value_number: value,
                pos,
            });
        };

        match c {
            '-' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                self.scan_number_exponent_after_sign(true, pos, raw, value)
            }
            '0'..='9' => self.scan_number_exponent_after_sign(false, pos, raw, value),
            'a'..='z' | 'A'..='Z' => self.scan_number_invalid_suffix(pos, raw, value),
            _ => {
                self.errors.missing_exponent_digits(p);
                Some(Token {
                    kind: TokenKind::NumberLit,
                    value_str: raw,
                    value_bytes: Vec::default(),
                    char_value: 0 as char,
                    value_number: value,
                    pos,
                })
            }
        }
    }

    fn scan_number_exponent_after_sign(
        &mut self,
        is_negative_exp: bool,
        pos: Pos,
        mut raw: String,
        mut value: Number,
    ) -> Option<Token> {
        let multiplier = if is_negative_exp { -1 } else { 1 };
        let mut has_exponent = false;

        let mut exp_after_e = BigInt::default();

        while let Some((c, _)) = self.scan_number_peek_with_skip_underscore(&mut raw) {
            match c {
                '0'..='9' => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    exp_after_e = exp_after_e * 10 + (Self::char_to_int(c) as i8) * multiplier;
                    has_exponent = true;
                }
                'a'..='z' | 'A'..='Z' => {
                    value.exp += exp_after_e;
                    return self.scan_number_invalid_suffix(pos, raw, value);
                }
                _ => break,
            }
        }

        if !has_exponent {
            self.errors.missing_exponent_digits(self.get_pos());
        }

        value.exp += exp_after_e;
        Some(Token {
            kind: TokenKind::NumberLit,
            value_str: raw,
            value_bytes: Vec::default(),
            char_value: 0 as char,
            value_number: value,
            pos,
        })
    }

    fn scan_number_invalid_suffix(
        &mut self,
        pos: Pos,
        mut raw: String,
        value: Number,
    ) -> Option<Token> {
        let mut invalid_suffix = String::default();

        let (c, invalid_suffix_pos) = self.next().unwrap();
        raw.push(c);
        invalid_suffix.push(c);

        while let Some((c, _)) = self.next_if(|c| matches!(c, '0'..='9' | 'a'..='z' | 'A'..='Z')) {
            raw.push(c);
            invalid_suffix.push(c);
        }

        self.errors
            .invalid_number_suffix(invalid_suffix_pos, &invalid_suffix);
        Some(Token {
            kind: TokenKind::NumberLit,
            value_str: raw,
            value_bytes: Vec::default(),
            char_value: 0 as char,
            value_number: value,
            pos,
        })
    }

    fn scan_comments(&mut self) -> Option<Token> {
        if self.text.len() < 2 {
            return None;
        }
        if self.peek_n(2) != "//" {
            return None;
        }

        let pos = self.get_pos();
        let mut value = String::from("//");
        self.next();
        self.next();

        while let Some((c, _)) = self.next() {
            value.push(c);
            if c == '\n' {
                break;
            }
        }

        Some(Token {
            kind: TokenKind::Comment,
            value_str: value,
            value_bytes: Vec::default(),
            char_value: '\0',
            value_number: Number::default(),
            pos,
        })
    }

    const SYMBOLS: &'static [(&'static str, TokenKind)] = &[
        ("::", TokenKind::DoubleColon),
        (":", TokenKind::Colon),
        (";", TokenKind::SemiColon),
        (".", TokenKind::Dot),
        ("!=", TokenKind::NEq),
        ("!", TokenKind::Not),
        ("==", TokenKind::Eq),
        ("=", TokenKind::Equal),
        ("*", TokenKind::Mul),
        ("+", TokenKind::Add),
        ("-", TokenKind::Sub),
        ("/", TokenKind::Div),
        (":", TokenKind::Colon),
        ("<<", TokenKind::ShiftLeft),
        ("<=", TokenKind::LEq),
        ("<", TokenKind::Lt),
        (">>", TokenKind::ShiftRight),
        (">=", TokenKind::GEq),
        (">", TokenKind::Gt),
        ("{", TokenKind::OpenBlock),
        ("}", TokenKind::CloseBlock),
        ("(", TokenKind::OpenBrac),
        (")", TokenKind::CloseBrac),
        ("[", TokenKind::OpenSquare),
        ("]", TokenKind::CloseSquare),
        (",", TokenKind::Comma),
        ("%", TokenKind::Mod),
        ("&&", TokenKind::And),
        ("&", TokenKind::BitAnd),
        ("||", TokenKind::Or),
        ("|", TokenKind::BitOr),
        ("^", TokenKind::BitXor),
        ("~", TokenKind::BitNot),
        ("@", TokenKind::AtSign),
    ];

    fn scan_symbols(&mut self) -> Option<Token> {
        let top = self.peek()?;
        let pos = top.1;

        let mut sym = String::new();
        let mut kind = TokenKind::Invalid;

        while let Some((c, _)) = self.peek() {
            sym.push(c);
            let mut found = false;
            for (_, k) in Self::SYMBOLS.iter().filter(|(op, _)| op.starts_with(&sym)) {
                found = true;
                kind = *k;
            }
            if !found {
                break;
            }

            self.next();
        }

        if let TokenKind::Invalid = kind {
            None
        } else {
            Some(Token {
                kind,
                value_str: String::default(),
                value_bytes: Vec::default(),
                char_value: '\0',
                value_number: Number::default(),
                pos,
            })
        }
    }

    fn scan_invalid(&mut self) -> Option<Token> {
        let (c, pos) = self.next()?;
        self.errors.unexpected_char(pos, c);
        Some(Token {
            kind: TokenKind::Invalid,
            value_str: c.to_string(),
            value_bytes: Vec::default(),
            char_value: '\0',
            value_number: Number::default(),
            pos,
        })
    }

    fn next_if(&mut self, func: impl FnOnce(char) -> bool) -> Option<(char, Pos)> {
        let ch = self.peek()?.0;
        if func(ch) {
            self.next()
        } else {
            None
        }
    }

    fn next(&mut self) -> Option<(char, Pos)> {
        let c = self.text.chars().next()?;
        let len = c.len_utf8();
        self.text = &self.text[len..];
        let pos = self.get_pos();
        self.offset += len;
        Some((c, pos))
    }

    fn get_pos(&self) -> Pos {
        self.file_offset.with_offset(self.offset)
    }

    fn peek(&self) -> Option<(char, Pos)> {
        let c = self.text.chars().next()?;
        let pos = self.get_pos();
        Some((c, pos))
    }

    fn peek_n(&self, n: usize) -> &str {
        let mut total_len = 0;
        let mut chars = self.text.chars();
        for _ in 0..n {
            let Some(c) = chars.next() else {
                break;
            };
            total_len += c.len_utf8();
        }

        &self.text[..total_len]
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
enum Base {
    Bin = 2,
    #[default]
    Dec = 10,
    Oct = 8,
    Hex = 16,
}

trait ScanningError: ErrorReporter {
    fn unexpected_char(&self, pos: Pos, ch: char) {
        self.report(pos, format!("Unexpected char '{ch}'"));
    }

    fn multiple_char_in_literal(&self, pos: Pos) {
        self.report(
            pos,
            "Character literal may only contain one code point".to_string(),
        );
    }

    fn missing_closing_quote(&self, pos: Pos) {
        self.report(pos, String::from("Missing closing quote in string literal"));
    }

    fn invalid_digit_in_base(&self, pos: Pos, digit: char, base: u8) {
        self.report(
            pos,
            format!("Cannot use '{digit}' in {base}-base integer literal"),
        );
    }

    fn invalid_number_suffix(&self, pos: Pos, invalid_suffix: &str) {
        self.report(
            pos,
            format!("Invalid suffix \"{invalid_suffix}\" for number literal"),
        );
    }

    fn missing_exponent_digits(&self, pos: Pos) {
        self.report(pos, String::from("The exponent has no digits"));
    }
}

impl<T> ScanningError for T where T: ErrorReporter {}

#[cfg(test)]
mod test {
    use super::*;
    use crate::error::ErrorManager;
    use crate::token::FileManager;
    use core::str::FromStr;
    use num::BigInt;
    use std::path::PathBuf;

    #[test]
    fn comment() {
        let path = PathBuf::from("dummy.mg");
        let mut files = FileManager::default();
        let source = r#"
// a simple comment

// another comment
// with multiline
// grouped comment

let a = 10; // comment in the end

// nested comment // is considered a single comment

// unicode
// a۰۱۸
// foo६४
// ŝ
// ŝfoo

"this is a string // not a comment"
"this is
a multi line // not a comment
string"
        "#
        .to_string();
        let file = files.add_file(path, source);
        let error_manager = ErrorManager::default();

        let tokens = scan(&error_manager, &file);

        assert_eq!(tokens[0].kind, TokenKind::Comment);
        assert_eq!(&tokens[0].value_str, "// a simple comment\n");

        assert_eq!(tokens[1].kind, TokenKind::Comment);
        assert_eq!(&tokens[1].value_str, "// another comment\n");
        assert_eq!(tokens[2].kind, TokenKind::Comment);
        assert_eq!(&tokens[2].value_str, "// with multiline\n");
        assert_eq!(tokens[3].kind, TokenKind::Comment);
        assert_eq!(&tokens[3].value_str, "// grouped comment\n");

        assert_eq!(tokens[4].kind, TokenKind::Let);
        assert_eq!(tokens[5].kind, TokenKind::Ident);
        assert_eq!(tokens[6].kind, TokenKind::Equal);
        assert_eq!(tokens[7].kind, TokenKind::NumberLit);
        assert_eq!(tokens[8].kind, TokenKind::SemiColon);
        assert_eq!(tokens[9].kind, TokenKind::Comment);
        assert_eq!(&tokens[9].value_str, "// comment in the end\n");

        assert_eq!(tokens[10].kind, TokenKind::Comment);
        assert_eq!(
            &tokens[10].value_str,
            "// nested comment // is considered a single comment\n"
        );

        assert_eq!(&tokens[11].value_str, "// unicode\n");
        assert_eq!(&tokens[12].value_str, "// a۰۱۸\n");
        assert_eq!(&tokens[13].value_str, "// foo६४\n");
        assert_eq!(&tokens[14].value_str, "// ŝ\n");
        assert_eq!(&tokens[15].value_str, "// ŝfoo\n");

        assert_eq!(tokens[16].kind, TokenKind::StringLit);
        assert_eq!(
            &tokens[16].value_str,
            r#""this is a string // not a comment""#
        );
        assert_eq!(
            std::str::from_utf8(&tokens[16].value_bytes).unwrap(),
            r#"this is a string // not a comment"#
        );

        assert_eq!(tokens[17].kind, TokenKind::StringLit);
        assert_eq!(
            &tokens[17].value_str,
            r#""this is
a multi line // not a comment
string""#
        );
        assert_eq!(
            std::str::from_utf8(&tokens[17].value_bytes).unwrap(),
            "this is\na multi line // not a comment\nstring"
        );

        assert!(!error_manager.has_errors());
    }

    #[test]
    fn string_literal() {
        let path = PathBuf::from("dummy.mg");
        let mut files = FileManager::default();
        let source = r#"
            "basic string"
            "this is an emoji 😀 😃 😄 😁 😆 😅 😂. It should scanned properly"
            "this is a string // not a comment"
            "hex escape like \x00\x12\xAb\xCd\xef\xEF are fine"
            "\n\r\t\\\0\"\''"
            "multiline
            string"
            "invalid escape \a\b\c\d\e\f\g\h\i\j\k\l\m\o\p\q\s\u\v\w\y\z"
            "invalid hex \xgh\x\\x"
            "missing closing quote"#
            .to_string();
        let file = files.add_file(path, source);
        let mut error_manager = ErrorManager::default();

        let tokens = scan(&error_manager, &file);

        assert_eq!(tokens[0].kind, TokenKind::StringLit);
        assert_eq!(&tokens[0].value_str, r#""basic string""#);
        assert_eq!(
            std::str::from_utf8(&tokens[0].value_bytes).unwrap(),
            r#"basic string"#
        );
        assert_eq!(tokens[1].kind, TokenKind::StringLit);
        assert_eq!(
            &tokens[1].value_str,
            r#""this is an emoji 😀 😃 😄 😁 😆 😅 😂. It should scanned properly""#
        );
        assert_eq!(
            std::str::from_utf8(&tokens[1].value_bytes).unwrap(),
            r#"this is an emoji 😀 😃 😄 😁 😆 😅 😂. It should scanned properly"#
        );
        assert_eq!(tokens[2].kind, TokenKind::StringLit);
        assert_eq!(
            &tokens[2].value_str,
            r#""this is a string // not a comment""#
        );
        assert_eq!(
            std::str::from_utf8(&tokens[2].value_bytes).unwrap(),
            r#"this is a string // not a comment"#
        );
        assert_eq!(tokens[3].kind, TokenKind::StringLit);
        assert_eq!(
            &tokens[3].value_str,
            r#""hex escape like \x00\x12\xAb\xCd\xef\xEF are fine""#,
        );
        assert_eq!(
            tokens[3].value_bytes.as_slice(),
            b"hex escape like \x00\x12\xAb\xCd\xef\xEF are fine",
        );
        assert_eq!(tokens[4].kind, TokenKind::StringLit);
        assert_eq!(&tokens[4].value_str, r#""\n\r\t\\\0\"\''""#);
        assert_eq!(&tokens[4].value_bytes, b"\n\r\t\\\0\"\''",);
        assert_eq!(tokens[5].kind, TokenKind::StringLit);
        assert_eq!(
            &tokens[5].value_str,
            r#""multiline
            string""#
        );
        assert_eq!(
            std::str::from_utf8(&tokens[5].value_bytes).unwrap(),
            r#"multiline
            string"#
        );
        assert_eq!(tokens[6].kind, TokenKind::StringLit);
        assert_eq!(
            &tokens[6].value_str,
            r#""invalid escape \a\b\c\d\e\f\g\h\i\j\k\l\m\o\p\q\s\u\v\w\y\z""#
        );
        assert_eq!(&tokens[6].value_bytes, b"invalid escape ",);
        assert_eq!(tokens[7].kind, TokenKind::StringLit);
        assert_eq!(&tokens[7].value_str, r#""invalid hex \xgh\x\\x""#);
        assert_eq!(&tokens[7].value_bytes, b"invalid hex gh\\x",);
        assert_eq!(tokens[8].kind, TokenKind::StringLit);
        assert_eq!(&tokens[8].value_str, r#""missing closing quote"#);
        assert_eq!(
            std::str::from_utf8(&tokens[8].value_bytes).unwrap(),
            r#"missing closing quote"#
        );

        let errors = error_manager.take();
        assert_eq!(errors.len(), 25);
        assert_eq!(errors[0].message, "Unexpected char 'a'");
        assert_eq!(errors[1].message, "Unexpected char 'b'");
        assert_eq!(errors[2].message, "Unexpected char 'c'");
        assert_eq!(errors[3].message, "Unexpected char 'd'");
        assert_eq!(errors[4].message, "Unexpected char 'e'");
        assert_eq!(errors[5].message, "Unexpected char 'f'");
        assert_eq!(errors[6].message, "Unexpected char 'g'");
        assert_eq!(errors[7].message, "Unexpected char 'h'");
        assert_eq!(errors[8].message, "Unexpected char 'i'");
        assert_eq!(errors[9].message, "Unexpected char 'j'");
        assert_eq!(errors[10].message, "Unexpected char 'k'");
        assert_eq!(errors[11].message, "Unexpected char 'l'");
        assert_eq!(errors[12].message, "Unexpected char 'm'");
        assert_eq!(errors[13].message, "Unexpected char 'o'");
        assert_eq!(errors[14].message, "Unexpected char 'p'");
        assert_eq!(errors[15].message, "Unexpected char 'q'");
        assert_eq!(errors[16].message, "Unexpected char 's'");
        assert_eq!(errors[17].message, "Unexpected char 'u'");
        assert_eq!(errors[18].message, "Unexpected char 'v'");
        assert_eq!(errors[19].message, "Unexpected char 'w'");
        assert_eq!(errors[20].message, "Unexpected char 'y'");
        assert_eq!(errors[21].message, "Unexpected char 'z'");
        assert_eq!(errors[22].message, "Unexpected char 'g'");
        assert_eq!(errors[23].message, "Unexpected char '\\'");
        assert_eq!(
            errors[24].message,
            "Missing closing quote in string literal"
        );
    }

    #[test]
    fn number_literal() {
        let path = PathBuf::from("dummy.mg");
        let mut files = FileManager::default();
        let source = r#"
            0
            1
            12345678_90123455561_090
            01234_567
            0xabc___def01234567890deadbeef0__10_
            0_o012345670123_4567
            0b_11010101001010101010
            0_b11010101001010101010
            0__b__11010101001010101010

            123.123
            123e123
            123e-123
            123E123
            123E-123
            1.23e123
            0.123
            0e123

            0123abcdef456
            0abcde
            0x123.abcd
            0b101.101
            0o123.123
            0b123
            123eabc
            123e-1a
            0xabcghijklmnopqrstuvwxyz
            123.abcde
            123e
            123e-
        "#
        .to_string();
        let file = files.add_file(path, source);
        let mut error_manager = ErrorManager::default();

        let tokens = scan(&error_manager, &file);

        assert!(tokens[0..8]
            .iter()
            .all(|token| token.kind == TokenKind::NumberLit));
        assert_eq!(&tokens[0].value_str, "0");
        assert_eq!(u8::try_from(&tokens[0].value_number).unwrap(), 0u8);
        assert_eq!(&tokens[1].value_str, "1");
        assert_eq!(u8::try_from(&tokens[1].value_number).unwrap(), 1u8);
        assert_eq!(&tokens[2].value_str, "12345678_90123455561_090");
        assert_eq!(
            BigInt::try_from(&tokens[2].value_number).unwrap(),
            BigInt::from_str("1234567890123455561090").unwrap(),
        );
        assert_eq!(&tokens[3].value_str, "01234_567");
        assert_eq!(
            i32::try_from(&tokens[3].value_number).unwrap(),
            0o1234567i32
        );
        assert_eq!(&tokens[4].value_str, "0xabc___def01234567890deadbeef0__10_");
        assert_eq!(
            BigInt::try_from(&tokens[4].value_number).unwrap(),
            BigInt::from_str("3484607783832696065538794497962000").unwrap(),
        );
        assert_eq!(&tokens[5].value_str, "0_o012345670123_4567");
        assert_eq!(
            i64::try_from(&tokens[5].value_number).unwrap(),
            5744368105847i64,
        );
        assert_eq!(&tokens[6].value_str, "0b_11010101001010101010");
        assert_eq!(i32::try_from(&tokens[6].value_number).unwrap(), 873130);
        assert_eq!(&tokens[7].value_str, "0_b11010101001010101010");
        assert_eq!(i32::try_from(&tokens[7].value_number).unwrap(), 873130);
        assert_eq!(&tokens[8].value_str, "0__b__11010101001010101010");
        assert_eq!(i32::try_from(&tokens[8].value_number).unwrap(), 873130);

        assert_eq!(tokens[9].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[9].value_str, "123.123");
        assert_eq!(f32::try_from(&tokens[9].value_number).unwrap(), 123.123f32);
        assert_eq!(tokens[10].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[10].value_str, "123e123");
        assert_eq!(
            f32::try_from(&tokens[10].value_number).unwrap(),
            f32::INFINITY
        );
        assert_eq!(tokens[11].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[11].value_str, "123e-123");
        assert_eq!(
            f32::try_from(&tokens[11].value_number).unwrap(),
            123e-123f32
        );
        assert_eq!(tokens[12].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[12].value_str, "123E123");
        assert_eq!(
            f32::try_from(&tokens[12].value_number).unwrap(),
            f32::INFINITY
        );
        assert_eq!(tokens[13].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[13].value_str, "123E-123");
        assert_eq!(
            f32::try_from(&tokens[13].value_number).unwrap(),
            123e-123f32
        );
        assert_eq!(tokens[14].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[14].value_str, "1.23e123");
        assert_eq!(
            f32::try_from(&tokens[14].value_number).unwrap(),
            f32::INFINITY,
        );
        assert_eq!(tokens[15].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[15].value_str, "0.123");
        assert_eq!(f32::try_from(&tokens[15].value_number).unwrap(), 0.123,);
        assert_eq!(tokens[16].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[16].value_str, "0e123");
        assert_eq!(f32::try_from(&tokens[16].value_number).unwrap(), 0f32);

        assert_eq!(tokens[17].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[17].value_str, "0123abcdef456");
        assert_eq!(tokens[18].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[18].value_str, "0abcde");
        assert_eq!(tokens[19].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[19].value_str, "0x123");
        assert_eq!(i32::try_from(&tokens[19].value_number).unwrap(), 291);
        assert_eq!(tokens[20].kind, TokenKind::Dot);
        assert_eq!(tokens[21].kind, TokenKind::Ident);

        assert_eq!(tokens[22].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[22].value_str, "0b101");
        assert_eq!(i32::try_from(&tokens[22].value_number).unwrap(), 5);
        assert_eq!(tokens[23].kind, TokenKind::Dot);
        assert_eq!(tokens[24].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[24].value_str, "101");
        assert_eq!(i32::try_from(&tokens[24].value_number).unwrap(), 101);

        assert_eq!(tokens[25].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[25].value_str, "0o123");
        assert_eq!(i32::try_from(&tokens[25].value_number).unwrap(), 83);
        assert_eq!(tokens[26].kind, TokenKind::Dot);
        assert_eq!(tokens[27].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[27].value_str, "123");
        assert_eq!(i32::try_from(&tokens[27].value_number).unwrap(), 123);

        assert_eq!(tokens[28].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[28].value_str, "0b123");
        assert_eq!(tokens[29].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[29].value_str, "123eabc");
        assert_eq!(tokens[30].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[30].value_str, "123e-1a");
        assert_eq!(tokens[31].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[31].value_str, "0xabcghijklmnopqrstuvwxyz");
        assert_eq!(tokens[32].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[32].value_str, "123.abcde");
        assert_eq!(tokens[33].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[33].value_str, "123e");
        assert_eq!(tokens[34].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[34].value_str, "123e-");

        let errors = error_manager.take();
        assert_eq!(errors.len(), 10);
        assert_eq!(
            errors[0].message,
            "Invalid suffix \"abcdef456\" for number literal"
        );
        assert_eq!(
            errors[1].message,
            "Invalid suffix \"abcde\" for number literal"
        );
        assert_eq!(
            errors[2].message,
            "Cannot use '2' in 2-base integer literal",
        );
        assert_eq!(
            errors[3].message,
            "Cannot use '3' in 2-base integer literal",
        );
        assert_eq!(
            errors[4].message,
            "Invalid suffix \"abc\" for number literal",
        );
        assert_eq!(errors[5].message, "Invalid suffix \"a\" for number literal",);
        assert_eq!(
            errors[6].message,
            "Invalid suffix \"ghijklmnopqrstuvwxyz\" for number literal",
        );
        assert_eq!(
            errors[7].message,
            "Invalid suffix \"abcde\" for number literal",
        );
        assert_eq!(errors[8].message, "The exponent has no digits",);
        assert_eq!(errors[9].message, "The exponent has no digits",);
    }

    #[test]
    fn symbols() {
        let path = PathBuf::from("dummy.mg");
        let mut files = FileManager::default();
        let source = r#"
            :: : ; . != ! == = * + - / : << <= < >> >= > { } ( ) [ ] , % && & || | ^ ~ @
            :::;.!!====*+-/:<<<=<>>>=>{}()[],%&&&|||^~@
            -1
            #
        "#
        .to_string();

        let file = files.add_file(path, source);
        let mut error_manager = ErrorManager::default();

        let tokens = scan(&error_manager, &file);
        assert_eq!(tokens[0].kind, TokenKind::DoubleColon);
        assert_eq!(tokens[1].kind, TokenKind::Colon);
        assert_eq!(tokens[2].kind, TokenKind::SemiColon);
        assert_eq!(tokens[3].kind, TokenKind::Dot);
        assert_eq!(tokens[4].kind, TokenKind::NEq);
        assert_eq!(tokens[5].kind, TokenKind::Not);
        assert_eq!(tokens[6].kind, TokenKind::Eq);
        assert_eq!(tokens[7].kind, TokenKind::Equal);
        assert_eq!(tokens[8].kind, TokenKind::Mul);
        assert_eq!(tokens[9].kind, TokenKind::Add);
        assert_eq!(tokens[10].kind, TokenKind::Sub);
        assert_eq!(tokens[11].kind, TokenKind::Div);
        assert_eq!(tokens[12].kind, TokenKind::Colon);
        assert_eq!(tokens[13].kind, TokenKind::ShiftLeft);
        assert_eq!(tokens[14].kind, TokenKind::LEq);
        assert_eq!(tokens[15].kind, TokenKind::Lt);
        assert_eq!(tokens[16].kind, TokenKind::ShiftRight);
        assert_eq!(tokens[17].kind, TokenKind::GEq);
        assert_eq!(tokens[18].kind, TokenKind::Gt);
        assert_eq!(tokens[19].kind, TokenKind::OpenBlock);
        assert_eq!(tokens[20].kind, TokenKind::CloseBlock);
        assert_eq!(tokens[21].kind, TokenKind::OpenBrac);
        assert_eq!(tokens[22].kind, TokenKind::CloseBrac);
        assert_eq!(tokens[23].kind, TokenKind::OpenSquare);
        assert_eq!(tokens[24].kind, TokenKind::CloseSquare);
        assert_eq!(tokens[25].kind, TokenKind::Comma);
        assert_eq!(tokens[26].kind, TokenKind::Mod);
        assert_eq!(tokens[27].kind, TokenKind::And);
        assert_eq!(tokens[28].kind, TokenKind::BitAnd);
        assert_eq!(tokens[29].kind, TokenKind::Or);
        assert_eq!(tokens[30].kind, TokenKind::BitOr);
        assert_eq!(tokens[31].kind, TokenKind::BitXor);
        assert_eq!(tokens[32].kind, TokenKind::BitNot);
        assert_eq!(tokens[33].kind, TokenKind::AtSign);

        assert_eq!(tokens[34].kind, TokenKind::DoubleColon);
        assert_eq!(tokens[35].kind, TokenKind::Colon);
        assert_eq!(tokens[36].kind, TokenKind::SemiColon);
        assert_eq!(tokens[37].kind, TokenKind::Dot);
        assert_eq!(tokens[38].kind, TokenKind::Not);
        assert_eq!(tokens[39].kind, TokenKind::NEq);
        assert_eq!(tokens[40].kind, TokenKind::Eq);
        assert_eq!(tokens[41].kind, TokenKind::Equal);
        assert_eq!(tokens[42].kind, TokenKind::Mul);
        assert_eq!(tokens[43].kind, TokenKind::Add);
        assert_eq!(tokens[44].kind, TokenKind::Sub);
        assert_eq!(tokens[45].kind, TokenKind::Div);
        assert_eq!(tokens[46].kind, TokenKind::Colon);
        assert_eq!(tokens[47].kind, TokenKind::ShiftLeft);
        assert_eq!(tokens[48].kind, TokenKind::LEq);
        assert_eq!(tokens[49].kind, TokenKind::Lt);
        assert_eq!(tokens[50].kind, TokenKind::ShiftRight);
        assert_eq!(tokens[51].kind, TokenKind::GEq);
        assert_eq!(tokens[52].kind, TokenKind::Gt);
        assert_eq!(tokens[53].kind, TokenKind::OpenBlock);
        assert_eq!(tokens[54].kind, TokenKind::CloseBlock);
        assert_eq!(tokens[55].kind, TokenKind::OpenBrac);
        assert_eq!(tokens[56].kind, TokenKind::CloseBrac);
        assert_eq!(tokens[57].kind, TokenKind::OpenSquare);
        assert_eq!(tokens[58].kind, TokenKind::CloseSquare);
        assert_eq!(tokens[59].kind, TokenKind::Comma);
        assert_eq!(tokens[60].kind, TokenKind::Mod);
        assert_eq!(tokens[61].kind, TokenKind::And);
        assert_eq!(tokens[62].kind, TokenKind::BitAnd);
        assert_eq!(tokens[63].kind, TokenKind::Or);
        assert_eq!(tokens[64].kind, TokenKind::BitOr);
        assert_eq!(tokens[65].kind, TokenKind::BitXor);
        assert_eq!(tokens[66].kind, TokenKind::BitNot);
        assert_eq!(tokens[67].kind, TokenKind::AtSign);

        assert_eq!(tokens[68].kind, TokenKind::Sub);
        assert_eq!(tokens[69].kind, TokenKind::NumberLit);

        let errors = error_manager.take();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].message, "Unexpected char '#'");
    }
}
