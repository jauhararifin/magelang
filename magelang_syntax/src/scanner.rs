use crate::ast::BinaryOp;
use crate::error::ErrorManager;
use crate::number::Number;
use crate::token::{File, Pos, Token, TokenKind};
use num::BigInt;

pub(crate) fn scan(errors: &ErrorManager, file: &File) -> Vec<Token> {
    let mut scanner = Scanner::new(errors, file);
    let mut tokens = Vec::default();
    while let Some(token) = scanner.scan() {
        tokens.push(token);
    }
    tokens.push(Token { kind: TokenKind::Eof, pos: scanner.pos, spacing: false });
    tokens
}

struct Scanner<'a> {
    errors: &'a ErrorManager,
    text: &'a str,
    pos: Pos,
}

impl<'a> Scanner<'a> {
    fn new(errors: &'a ErrorManager, file: &'a File) -> Self {
        Self { errors, text: &file.text, pos: Pos { file: file.id, line: 1, col: 1 } }
    }

    fn scan(&mut self) -> Option<Token> {
        let has_previous_input = self.pos.line != 1 || self.pos.col != 1;
        let skipped_whitespace = self.skip_whitespace();
        let mut token = self
            .scan_word()
            .or_else(|| self.scan_char_lit())
            .or_else(|| self.scan_string_lit())
            .or_else(|| self.scan_number_lit())
            .or_else(|| self.scan_comments())
            .or_else(|| self.scan_symbols())
            .or_else(|| self.scan_invalid())?;
        token.spacing = has_previous_input && !skipped_whitespace;
        Some(token)
    }

    fn skip_whitespace(&mut self) -> bool {
        let initial_len = self.text.len();
        while let Some((ch, _)) = self.peek() {
            if ch.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
        self.text.len() != initial_len
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
            "for" => TokenKind::For,
            "defer" => TokenKind::Defer,
            "fn" => TokenKind::Fn,
            "return" => TokenKind::Return,
            "import" => TokenKind::Import,
            "as" => TokenKind::As,
            "null" => TokenKind::Null,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "continue" => TokenKind::Continue,
            "break" => TokenKind::Break,
            _ => TokenKind::Ident(value),
        };

        Some(Token { kind, pos, spacing: false })
    }

    fn scan_char_lit(&mut self) -> Option<Token> {
        let (_, pos) = self.next_if(|c| c == '\'')?;
        let mut raw = String::from("\'");

        if let Some((c, _)) = self.peek() {
            match c {
                '\\' => self.scan_char_after_backslash(pos, raw),
                '\'' => {
                    report_empty_character_literal(self.errors, pos);
                    self.scan_char_closing(pos, raw, 0 as char)
                }
                _ => {
                    self.next();
                    raw.push(c);
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
                report_unexpected_char(self.errors, p, c);
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
                        report_unexpected_char(self.errors, p, c);
                        self.scan_char_closing(pos, raw, 0 as char)
                    }
                }
            }
            _ => {
                report_unexpected_char(self.errors, p, c);
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
                report_missing_closing_quote(self.errors, self.pos, "character");
                return Some(Token { kind: TokenKind::CharLit { raw, value }, pos, spacing: false });
            };
            raw.push(c);

            if c == '\'' {
                return Some(Token { kind: TokenKind::CharLit { raw, value }, pos, spacing: false });
            }

            if !found_multichar {
                report_multiple_char_in_literal(self.errors, p);
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

    fn scan_string_internal(&mut self, pos: Pos, mut raw: String, mut value: Vec<u8>) -> Option<Token> {
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
            'n' => value.push(b'\n'),
            'r' => value.push(b'\r'),
            't' => value.push(b'\t'),
            '\\' => value.push(b'\\'),
            '0' => value.push(0),
            '\'' => value.push(b'\''),
            '"' => value.push(b'"'),
            'x' => self.scan_string_hex(raw, value),
            _ => report_unexpected_char(self.errors, p, c),
        }
    }

    fn scan_string_hex(&mut self, raw: &mut String, value: &mut Vec<u8>) {
        let Some((c, p)) = self.peek() else { return };

        if !c.is_ascii_hexdigit() {
            report_unexpected_char(self.errors, p, c);
            return;
        }

        raw.push(c);
        self.next();
        let char_val = Self::char_to_int(c);

        let Some((c, p)) = self.peek() else { return };

        if !c.is_ascii_hexdigit() {
            report_unexpected_char(self.errors, p, c);
            return;
        }

        raw.push(c);
        self.next();
        let char_val = char_val << 4 | Self::char_to_int(c);

        value.push(char_val);
    }

    fn scan_string_closing(&mut self, pos: Pos, mut raw: String, value: Vec<u8>) -> Option<Token> {
        let Some((c, _)) = self.next() else {
            report_missing_closing_quote(self.errors, self.pos, "string");
            return Some(Token { kind: TokenKind::StringLit { raw, value }, pos, spacing: false });
        };
        raw.push(c);
        assert_eq!(c, '\"');

        Some(Token { kind: TokenKind::StringLit { raw, value }, pos, spacing: false })
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
                self.scan_number_base(Base::Dec, pos, raw, value, true)
            }
            _ => None,
        }
    }

    fn scan_number_prefix(&mut self) -> Option<Token> {
        let (c, pos) = self.next().unwrap();
        assert_eq!(c, '0');

        let mut raw = String::from("0");
        let mut value = Number::new(BigInt::default(), BigInt::default(), false);

        let Some((c, _)) = self.scan_number_peek_with_skip_underscore(&mut raw) else {
            return Some(Token { kind: TokenKind::NumberLit { raw, value }, pos, spacing: false });
        };

        match c {
            'x' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                self.scan_number_base(Base::Hex, pos, raw, value, false)
            }
            'b' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                self.scan_number_base(Base::Bin, pos, raw, value, false)
            }
            'o' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                self.scan_number_base(Base::Oct, pos, raw, value, false)
            }
            '0'..='7' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                value.val = value.val * 8 + Self::char_to_int(c);
                self.scan_number_base(Base::Oct, pos, raw, value, true)
            }
            'e' | 'E' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                value.float = true;
                self.scan_number_exponent(pos, raw, value)
            }
            '.' => {
                let (c, _) = self.next().unwrap();
                raw.push(c);
                value.float = true;
                self.scan_number_fraction(pos, raw, value)
            }
            'a'..='z' | 'A'..='Z' => self.scan_number_invalid_suffix(pos, raw, value),
            _ => Some(Token { kind: TokenKind::NumberLit { raw, value }, pos, spacing: false }),
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
        mut has_digit: bool,
    ) -> Option<Token> {
        let mut has_invalid_digit = false;
        while let Some((c, p)) = self.scan_number_peek_with_skip_underscore(&mut raw) {
            match (base, c) {
                (Base::Dec, 'e' | 'E') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    value.float = true;
                    return self.scan_number_exponent(pos, raw, value);
                }
                (Base::Dec, '.') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    value.float = true;
                    return self.scan_number_fraction(pos, raw, value);
                }
                (_, '.') => break,
                (Base::Bin, '0' | '1') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    value.val = value.val * 2 + Self::char_to_int(c);
                    has_digit = true;
                }
                (Base::Dec, '0'..='9') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    value.val = value.val * 10 + Self::char_to_int(c);
                    has_digit = true;
                }
                (Base::Oct, '0'..='7') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    value.val = value.val * 8 + Self::char_to_int(c);
                    has_digit = true;
                }
                (Base::Hex, '0'..='9' | 'a'..='f' | 'A'..='F') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    value.val = value.val * 16 + Self::char_to_int(c);
                    has_digit = true;
                }
                (Base::Bin, '2'..='9') | (Base::Oct, '8'..='9') => {
                    let (c, _) = self.next().unwrap();
                    raw.push(c);
                    report_invalid_digit_in_base(self.errors, p, c, base as u8);
                    has_invalid_digit = true;
                }
                (Base::Bin | Base::Dec | Base::Oct, 'a'..='z' | 'A'..='Z') | (Base::Hex, 'g'..='z' | 'G'..='Z') => {
                    return self.scan_number_invalid_suffix(pos, raw, value);
                }
                _ => break,
            }
        }

        if !has_digit && !has_invalid_digit {
            report_missing_base_digits(self.errors, self.pos, base as u8);
        }

        Some(Token { kind: TokenKind::NumberLit { raw, value }, pos, spacing: false })
    }

    fn scan_number_fraction(&mut self, pos: Pos, mut raw: String, mut value: Number) -> Option<Token> {
        assert!(value.float);
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

        Some(Token { kind: TokenKind::NumberLit { raw, value }, pos, spacing: false })
    }

    fn scan_number_exponent(&mut self, pos: Pos, mut raw: String, value: Number) -> Option<Token> {
        assert!(value.float);
        let Some((c, p)) = self.scan_number_peek_with_skip_underscore(&mut raw) else {
            report_missing_exponent_digits(self.errors, self.pos);
            return Some(Token { kind: TokenKind::NumberLit { raw, value }, pos, spacing: false });
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
                report_missing_exponent_digits(self.errors, p);
                Some(Token { kind: TokenKind::NumberLit { raw, value }, pos, spacing: false })
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
        assert!(value.float);
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
            report_missing_exponent_digits(self.errors, self.pos);
        }

        value.exp += exp_after_e;
        Some(Token { kind: TokenKind::NumberLit { raw, value }, pos, spacing: false })
    }

    fn scan_number_invalid_suffix(&mut self, pos: Pos, mut raw: String, value: Number) -> Option<Token> {
        let mut invalid_suffix = String::default();

        let (c, invalid_suffix_pos) = self.next().unwrap();
        raw.push(c);
        invalid_suffix.push(c);

        while let Some((c, _)) = self.next_if(|c| matches!(c, '0'..='9' | 'a'..='z' | 'A'..='Z')) {
            raw.push(c);
            invalid_suffix.push(c);
        }

        report_invalid_number_suffix(self.errors, invalid_suffix_pos, &invalid_suffix);
        Some(Token { kind: TokenKind::NumberLit { raw, value }, pos, spacing: false })
    }

    fn scan_comments(&mut self) -> Option<Token> {
        if self.text.len() < 2 {
            return None;
        }
        if self.peek_n(2) != "//" {
            return None;
        }

        let pos = self.pos;
        let mut value = String::from("//");
        self.next();
        self.next();

        while let Some((c, _)) = self.next() {
            value.push(c);
            if c == '\n' {
                break;
            }
        }

        Some(Token { kind: TokenKind::Comment(value), pos, spacing: false })
    }

    fn scan_symbols(&mut self) -> Option<Token> {
        let (symbol, pos) = self.peek()?;
        let (kind, symbol_len) = match symbol {
            ':' => (TokenKind::Colon, 1),
            ';' => (TokenKind::SemiColon, 1),
            '.' => (TokenKind::Dot, 1),
            '!' if self.text.starts_with("!=") => (TokenKind::NEq, 2),
            '!' => (TokenKind::Not, 1),
            '=' if self.text.starts_with("==") => (TokenKind::Eq, 2),
            '=' => (TokenKind::Equal, 1),
            '*' if self.text.starts_with("*=") => (TokenKind::AssignOp(BinaryOp::Mul), 2),
            '*' => (TokenKind::Mul, 1),
            '+' if self.text.starts_with("+=") => (TokenKind::AssignOp(BinaryOp::Add), 2),
            '+' => (TokenKind::Add, 1),
            '-' if self.text.starts_with("-=") => (TokenKind::AssignOp(BinaryOp::Sub), 2),
            '-' => (TokenKind::Sub, 1),
            '/' if self.text.starts_with("/=") => (TokenKind::AssignOp(BinaryOp::Div), 2),
            '/' => (TokenKind::Div, 1),
            '<' if self.text.starts_with("<<=") => (TokenKind::AssignOp(BinaryOp::ShiftLeft), 3),
            '<' if self.text.starts_with("<<") => (TokenKind::ShiftLeft, 2),
            '<' if self.text.starts_with("<=") => (TokenKind::LEq, 2),
            '<' => (TokenKind::Lt, 1),
            '>' if self.text.starts_with(">>=") => (TokenKind::AssignOp(BinaryOp::ShiftRight), 3),
            '>' if self.text.starts_with(">>") => (TokenKind::ShiftRight, 2),
            '>' if self.text.starts_with(">=") => (TokenKind::GEq, 2),
            '>' => (TokenKind::Gt, 1),
            '{' => (TokenKind::OpenBlock, 1),
            '}' => (TokenKind::CloseBlock, 1),
            '(' => (TokenKind::OpenBrac, 1),
            ')' => (TokenKind::CloseBrac, 1),
            '[' => (TokenKind::OpenSquare, 1),
            ']' => (TokenKind::CloseSquare, 1),
            ',' => (TokenKind::Comma, 1),
            '%' if self.text.starts_with("%=") => (TokenKind::AssignOp(BinaryOp::Mod), 2),
            '%' => (TokenKind::Mod, 1),
            '&' if self.text.starts_with("&&=") => (TokenKind::AssignOp(BinaryOp::And), 3),
            '&' if self.text.starts_with("&&") => (TokenKind::And, 2),
            '&' if self.text.starts_with("&=") => (TokenKind::AssignOp(BinaryOp::BitAnd), 2),
            '&' => (TokenKind::BitAnd, 1),
            '|' if self.text.starts_with("||=") => (TokenKind::AssignOp(BinaryOp::Or), 3),
            '|' if self.text.starts_with("||") => (TokenKind::Or, 2),
            '|' if self.text.starts_with("|=") => (TokenKind::AssignOp(BinaryOp::BitOr), 2),
            '|' => (TokenKind::BitOr, 1),
            '^' if self.text.starts_with("^=") => (TokenKind::AssignOp(BinaryOp::BitXor), 2),
            '^' => (TokenKind::BitXor, 1),
            '~' => (TokenKind::BitNot, 1),
            '@' => (TokenKind::AtSign, 1),
            _ => return None,
        };

        for _ in 0..symbol_len {
            self.next();
        }
        Some(Token { kind, pos, spacing: false })
    }

    fn scan_invalid(&mut self) -> Option<Token> {
        let (c, pos) = self.next()?;
        report_unexpected_char(self.errors, pos, c);
        Some(Token { kind: TokenKind::Invalid(c), pos, spacing: false })
    }

    fn next_if(&mut self, func: impl FnOnce(char) -> bool) -> Option<(char, Pos)> {
        let ch = self.peek()?.0;
        if func(ch) { self.next() } else { None }
    }

    fn next(&mut self) -> Option<(char, Pos)> {
        let c = self.text.chars().next()?;
        let len = c.len_utf8();
        self.text = &self.text[len..];
        let pos = self.pos;
        if c == '\n' {
            self.pos.line += 1;
            self.pos.col = 1;
        } else {
            self.pos.col += 1;
        }
        Some((c, pos))
    }

    fn peek(&self) -> Option<(char, Pos)> {
        let c = self.text.chars().next()?;
        Some((c, self.pos))
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

fn report_unexpected_char(errors: &ErrorManager, pos: Pos, ch: char) {
    errors.report(pos, format!("Unexpected char '{ch}'"));
}

fn report_multiple_char_in_literal(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, "Character literal may only contain one code point".to_string());
}

fn report_empty_character_literal(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, "Character literal cannot be empty".to_string());
}

fn report_missing_closing_quote(errors: &ErrorManager, pos: Pos, literal_kind: &str) {
    errors.report(pos, format!("Missing closing quote in {literal_kind} literal"));
}

fn report_invalid_digit_in_base(errors: &ErrorManager, pos: Pos, digit: char, base: u8) {
    errors.report(pos, format!("Cannot use '{digit}' in {base}-base integer literal"));
}

fn report_invalid_number_suffix(errors: &ErrorManager, pos: Pos, invalid_suffix: &str) {
    errors.report(pos, format!("Invalid suffix \"{invalid_suffix}\" for number literal"));
}

fn report_missing_base_digits(errors: &ErrorManager, pos: Pos, base: u8) {
    errors.report(pos, format!("Expected at least one digit in {base}-base integer literal"));
}

fn report_missing_exponent_digits(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("The exponent has no digits"));
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::FileManager;
    use core::str::FromStr;
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
        let file = files.add_file(path, source).unwrap();
        let error_manager = ErrorManager::default();

        let tokens = scan(&error_manager, &file);

        assert_eq!(tokens[0].kind, TokenKind::Comment("// a simple comment\n".to_string()));

        assert_eq!(tokens[1].kind, TokenKind::Comment("// another comment\n".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::Comment("// with multiline\n".to_string()));
        assert_eq!(tokens[3].kind, TokenKind::Comment("// grouped comment\n".to_string()));

        assert_eq!(tokens[4].kind, TokenKind::Let);
        assert_eq!(tokens[5].kind, TokenKind::Ident("a".to_string()));
        assert_eq!(tokens[6].kind, TokenKind::Equal);
        assert_eq!(tokens[7].kind, TokenKind::NumberLit { raw: "10".to_string(), value: Number::new(10, 0, false) });
        assert_eq!(tokens[8].kind, TokenKind::SemiColon);
        assert_eq!(tokens[9].kind, TokenKind::Comment("// comment in the end\n".to_string()));

        assert_eq!(
            tokens[10].kind,
            TokenKind::Comment("// nested comment // is considered a single comment\n".to_string())
        );

        assert_eq!(tokens[11].kind, TokenKind::Comment("// unicode\n".to_string()));
        assert_eq!(tokens[12].kind, TokenKind::Comment("// a۰۱۸\n".to_string()));
        assert_eq!(tokens[13].kind, TokenKind::Comment("// foo६४\n".to_string()));
        assert_eq!(tokens[14].kind, TokenKind::Comment("// ŝ\n".to_string()));
        assert_eq!(tokens[15].kind, TokenKind::Comment("// ŝfoo\n".to_string()));

        assert_eq!(
            tokens[16].kind,
            TokenKind::StringLit {
                raw: r#""this is a string // not a comment""#.to_string(),
                value: b"this is a string // not a comment".to_vec(),
            }
        );

        assert_eq!(
            tokens[17].kind,
            TokenKind::StringLit {
                raw: r#""this is
a multi line // not a comment
string""#
                    .to_string(),
                value: b"this is\na multi line // not a comment\nstring".to_vec(),
            }
        );

        assert!(error_manager.is_empty());
    }

    #[test]
    fn character_literal() {
        let mut files = FileManager::default();
        let file = files.add_file(PathBuf::from("dummy.mg"), r#"''; '\0' '\x00' 'a' '😀' 'z"#.to_string()).unwrap();
        let mut errors = ErrorManager::default();
        let tokens = scan(&errors, &file);

        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0].kind, TokenKind::CharLit { raw: "''".to_string(), value: '\0' });
        assert_eq!(tokens[1].kind, TokenKind::SemiColon);
        assert_eq!(tokens[2].kind, TokenKind::CharLit { raw: "'\\0'".to_string(), value: '\0' });
        assert_eq!(tokens[3].kind, TokenKind::CharLit { raw: "'\\x00'".to_string(), value: '\0' });
        assert_eq!(tokens[4].kind, TokenKind::CharLit { raw: "'a'".to_string(), value: 'a' });
        assert_eq!(tokens[5].kind, TokenKind::CharLit { raw: "'😀'".to_string(), value: '😀' });
        assert_eq!(tokens[6].kind, TokenKind::CharLit { raw: "'z".to_string(), value: 'z' });

        let errors = errors.take();
        assert_eq!(errors.len(), 2);
        assert_eq!(errors[0].message, "Character literal cannot be empty");
        assert_eq!(errors[1].message, "Missing closing quote in character literal");
        let location = files.location(errors[0].pos.unwrap());
        assert_eq!((location.line, location.col), (1, 1));
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
        let file = files.add_file(path, source).unwrap();
        let mut error_manager = ErrorManager::default();

        let tokens = scan(&error_manager, &file);

        assert_eq!(
            tokens[0].kind,
            TokenKind::StringLit { raw: r#""basic string""#.to_string(), value: b"basic string".to_vec() }
        );
        assert_eq!(
            tokens[1].kind,
            TokenKind::StringLit {
                raw: r#""this is an emoji 😀 😃 😄 😁 😆 😅 😂. It should scanned properly""#.to_string(),
                value: "this is an emoji 😀 😃 😄 😁 😆 😅 😂. It should scanned properly".bytes().collect(),
            }
        );
        assert_eq!(
            tokens[2].kind,
            TokenKind::StringLit {
                raw: r#""this is a string // not a comment""#.to_string(),
                value: b"this is a string // not a comment".to_vec(),
            }
        );
        assert_eq!(
            tokens[3].kind,
            TokenKind::StringLit {
                raw: r#""hex escape like \x00\x12\xAb\xCd\xef\xEF are fine""#.to_string(),
                value: b"hex escape like \x00\x12\xAb\xCd\xef\xEF are fine".to_vec(),
            }
        );
        assert_eq!(
            tokens[4].kind,
            TokenKind::StringLit { raw: r#""\n\r\t\\\0\"\''""#.to_string(), value: b"\n\r\t\\\0\"\''".to_vec() }
        );
        assert_eq!(
            tokens[5].kind,
            TokenKind::StringLit {
                raw: r#""multiline
            string""#
                    .to_string(),
                value: b"multiline
            string"
                    .to_vec()
            }
        );
        assert_eq!(
            tokens[6].kind,
            TokenKind::StringLit {
                raw: r#""invalid escape \a\b\c\d\e\f\g\h\i\j\k\l\m\o\p\q\s\u\v\w\y\z""#.to_string(),
                value: b"invalid escape ".to_vec(),
            }
        );
        assert_eq!(
            tokens[7].kind,
            TokenKind::StringLit {
                raw: r#""invalid hex \xgh\x\\x""#.to_string(),
                value: b"invalid hex gh\\x".to_vec(),
            }
        );
        assert_eq!(
            tokens[8].kind,
            TokenKind::StringLit {
                raw: r#""missing closing quote"#.to_string(),
                value: b"missing closing quote".to_vec(),
            }
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
        assert_eq!(errors[24].message, "Missing closing quote in string literal");
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
        let file = files.add_file(path, source).unwrap();
        let mut error_manager = ErrorManager::default();

        let tokens = scan(&error_manager, &file);

        assert!(tokens[0..8].iter().all(|token| matches!(token.kind, TokenKind::NumberLit { .. })));
        assert_eq!(tokens[0].kind, TokenKind::NumberLit { raw: "0".to_string(), value: Number::new(0, 0, false) });
        assert_eq!(tokens[1].kind, TokenKind::NumberLit { raw: "1".to_string(), value: Number::new(1, 0, false) });
        assert_eq!(
            tokens[2].kind,
            TokenKind::NumberLit {
                raw: "12345678_90123455561_090".to_string(),
                value: number_from_str("1234567890123455561090", "0", false),
            }
        );
        assert_eq!(
            tokens[3].kind,
            TokenKind::NumberLit { raw: "01234_567".to_string(), value: Number::new(0o1234567, 0, false) }
        );
        assert_eq!(
            tokens[4].kind,
            TokenKind::NumberLit {
                raw: "0xabc___def01234567890deadbeef0__10_".to_string(),
                value: number_from_str("3484607783832696065538794497962000", "0", false),
            },
        );
        assert_eq!(
            tokens[5].kind,
            TokenKind::NumberLit {
                raw: "0_o012345670123_4567".to_string(),
                value: Number::new(5744368105847i64, 0, false),
            }
        );
        assert_eq!(
            tokens[6].kind,
            TokenKind::NumberLit { raw: "0b_11010101001010101010".to_string(), value: Number::new(873130, 0, false) }
        );
        assert_eq!(
            tokens[7].kind,
            TokenKind::NumberLit { raw: "0_b11010101001010101010".to_string(), value: Number::new(873130, 0, false) }
        );
        assert_eq!(
            tokens[8].kind,
            TokenKind::NumberLit {
                raw: "0__b__11010101001010101010".to_string(),
                value: Number::new(873130, 0, false),
            }
        );

        assert_eq!(
            tokens[9].kind,
            TokenKind::NumberLit { raw: "123.123".to_string(), value: Number::new(123123, -3, true) }
        );
        assert_eq!(
            tokens[10].kind,
            TokenKind::NumberLit { raw: "123e123".to_string(), value: Number::new(123, 123, true) }
        );
        assert_eq!(
            tokens[11].kind,
            TokenKind::NumberLit { raw: "123e-123".to_string(), value: Number::new(123, -123, true) }
        );
        assert_eq!(
            tokens[12].kind,
            TokenKind::NumberLit { raw: "123E123".to_string(), value: Number::new(123, 123, true) }
        );
        assert_eq!(
            tokens[13].kind,
            TokenKind::NumberLit { raw: "123E-123".to_string(), value: Number::new(123, -123, true) }
        );
        assert_eq!(
            tokens[14].kind,
            TokenKind::NumberLit { raw: "1.23e123".to_string(), value: Number::new(123, 121, true) }
        );
        assert_eq!(
            tokens[15].kind,
            TokenKind::NumberLit { raw: "0.123".to_string(), value: Number::new(123, -3, true) }
        );
        assert_eq!(
            tokens[16].kind,
            TokenKind::NumberLit { raw: "0e123".to_string(), value: Number::new(0, 123, true) }
        );

        assert_eq!(
            tokens[17].kind,
            TokenKind::NumberLit { raw: "0123abcdef456".to_string(), value: Number::new(0o123, 0, false) }
        );
        assert_eq!(
            tokens[18].kind,
            TokenKind::NumberLit { raw: "0abcde".to_string(), value: Number::new(0, 0, false) }
        );
        assert_eq!(
            tokens[19].kind,
            TokenKind::NumberLit { raw: "0x123".to_string(), value: Number::new(291, 0, false) }
        );
        assert_eq!(tokens[20].kind, TokenKind::Dot);
        assert_eq!(tokens[21].kind, TokenKind::Ident("abcd".to_string()));

        assert_eq!(tokens[22].kind, TokenKind::NumberLit { raw: "0b101".to_string(), value: Number::new(5, 0, false) });
        assert_eq!(tokens[23].kind, TokenKind::Dot);
        assert_eq!(tokens[24].kind, TokenKind::NumberLit { raw: "101".to_string(), value: Number::new(101, 0, false) });

        assert_eq!(
            tokens[25].kind,
            TokenKind::NumberLit { raw: "0o123".to_string(), value: Number::new(83, 0, false) }
        );
        assert_eq!(tokens[26].kind, TokenKind::Dot);
        assert_eq!(tokens[27].kind, TokenKind::NumberLit { raw: "123".to_string(), value: Number::new(123, 0, false) });

        assert_eq!(tokens[28].kind, TokenKind::NumberLit { raw: "0b123".to_string(), value: Number::new(1, 0, false) });
        assert_eq!(
            tokens[29].kind,
            TokenKind::NumberLit { raw: "123eabc".to_string(), value: Number::new(123, 0, true) }
        );
        assert_eq!(
            tokens[30].kind,
            TokenKind::NumberLit { raw: "123e-1a".to_string(), value: Number::new(123, -1, true) }
        );
        assert_eq!(
            tokens[31].kind,
            TokenKind::NumberLit { raw: "0xabcghijklmnopqrstuvwxyz".to_string(), value: Number::new(0xabc, 0, false) }
        );
        assert_eq!(
            tokens[32].kind,
            TokenKind::NumberLit { raw: "123.abcde".to_string(), value: Number::new(123, 0, true) }
        );
        assert_eq!(tokens[33].kind, TokenKind::NumberLit { raw: "123e".to_string(), value: Number::new(123, 0, true) });
        assert_eq!(
            tokens[34].kind,
            TokenKind::NumberLit { raw: "123e-".to_string(), value: Number::new(123, 0, true) }
        );

        let errors = error_manager.take();
        assert_eq!(errors.len(), 10);
        assert_eq!(errors[0].message, "Invalid suffix \"abcdef456\" for number literal");
        assert_eq!(errors[1].message, "Invalid suffix \"abcde\" for number literal");
        assert_eq!(errors[2].message, "Cannot use '2' in 2-base integer literal",);
        assert_eq!(errors[3].message, "Cannot use '3' in 2-base integer literal",);
        assert_eq!(errors[4].message, "Invalid suffix \"abc\" for number literal",);
        assert_eq!(errors[5].message, "Invalid suffix \"a\" for number literal",);
        assert_eq!(errors[6].message, "Invalid suffix \"ghijklmnopqrstuvwxyz\" for number literal",);
        assert_eq!(errors[7].message, "Invalid suffix \"abcde\" for number literal",);
        assert_eq!(errors[8].message, "The exponent has no digits",);
        assert_eq!(errors[9].message, "The exponent has no digits",);
    }

    #[test]
    fn radix_prefix_requires_digit() {
        let mut files = FileManager::default();
        let file = files
            .add_file(PathBuf::from("dummy.mg"), "0x 0b 0o 0x_ 0b___ 0o_ 0_x 0_b___ 0b2 0o8 0xg 0__o_".to_string())
            .unwrap();
        let mut errors = ErrorManager::default();
        let tokens = scan(&errors, &file);

        assert_eq!(tokens.len(), 13);
        for (token, expected_raw) in
            tokens.iter().zip(["0x", "0b", "0o", "0x_", "0b___", "0o_", "0_x", "0_b___", "0b2", "0o8", "0xg", "0__o_"])
        {
            let TokenKind::NumberLit { raw, value } = &token.kind else {
                panic!("expected number literal");
            };
            assert_eq!(raw, expected_raw);
            assert_eq!(value, &Number::default());
        }

        let messages: Vec<_> = errors.take().into_iter().map(|error| error.message).collect();
        assert_eq!(
            messages,
            [
                "Expected at least one digit in 16-base integer literal",
                "Expected at least one digit in 2-base integer literal",
                "Expected at least one digit in 8-base integer literal",
                "Expected at least one digit in 16-base integer literal",
                "Expected at least one digit in 2-base integer literal",
                "Expected at least one digit in 8-base integer literal",
                "Expected at least one digit in 16-base integer literal",
                "Expected at least one digit in 2-base integer literal",
                "Cannot use '2' in 2-base integer literal",
                "Cannot use '8' in 8-base integer literal",
                "Invalid suffix \"g\" for number literal",
                "Expected at least one digit in 8-base integer literal",
            ]
        );

        let file = files.add_file(PathBuf::from("valid.mg"), "0x0 0b0 0o0 0x_0 0b___0 0o_0".to_string()).unwrap();
        let errors = ErrorManager::default();
        let tokens = scan(&errors, &file);
        assert!(errors.is_empty());
        assert_eq!(tokens.len(), 7);
        for (token, expected_raw) in tokens.iter().zip(["0x0", "0b0", "0o0", "0x_0", "0b___0", "0o_0"]) {
            let TokenKind::NumberLit { raw, value } = &token.kind else {
                panic!("expected number literal");
            };
            assert_eq!(raw, expected_raw);
            assert_eq!(value, &Number::default());
        }
    }

    #[test]
    fn assignment_symbols() {
        let mut files = FileManager::default();
        let file = files
            .add_file(
                PathBuf::from("dummy.mg"),
                "+= -= *= /= %= &= |= ^= <<= >>= a+=1 a<<=b >>=<<= == <= >= &&= ||= &&|| &=&".to_string(),
            )
            .unwrap();
        let tokens = scan(&ErrorManager::default(), &file);
        let ops = [
            BinaryOp::Add,
            BinaryOp::Sub,
            BinaryOp::Mul,
            BinaryOp::Div,
            BinaryOp::Mod,
            BinaryOp::BitAnd,
            BinaryOp::BitOr,
            BinaryOp::BitXor,
            BinaryOp::ShiftLeft,
            BinaryOp::ShiftRight,
        ];
        for (i, op) in ops.iter().enumerate() {
            assert_eq!(tokens[i].kind, TokenKind::AssignOp(*op));
        }
        assert_eq!(tokens[11].kind, TokenKind::AssignOp(BinaryOp::Add));
        assert_eq!(tokens[14].kind, TokenKind::AssignOp(BinaryOp::ShiftLeft));
        assert_eq!(tokens[16].kind, TokenKind::AssignOp(BinaryOp::ShiftRight));
        assert_eq!(tokens[17].kind, TokenKind::AssignOp(BinaryOp::ShiftLeft));
        assert_eq!(tokens[18].kind, TokenKind::Eq);
        assert_eq!(tokens[19].kind, TokenKind::LEq);
        assert_eq!(tokens[20].kind, TokenKind::GEq);
        assert_eq!(tokens[21].kind, TokenKind::AssignOp(BinaryOp::And));
        assert_eq!(tokens[22].kind, TokenKind::AssignOp(BinaryOp::Or));
        assert_eq!(tokens[23].kind, TokenKind::And);
        assert_eq!(tokens[24].kind, TokenKind::Or);
        assert_eq!(tokens[25].kind, TokenKind::AssignOp(BinaryOp::BitAnd));
        assert_eq!(tokens[26].kind, TokenKind::BitAnd);
    }

    #[test]
    fn symbols() {
        let path = PathBuf::from("dummy.mg");
        let mut files = FileManager::default();
        let source = r#"
            . : ; . != ! == = * + - / : << <= < >> >= > { } ( ) [ ] , % && & || | ^ ~ @
            .:;.!!====*+-/:<<<=<>>>=>{}()[],%&&&|||^~@
            -1
            #
        "#
        .to_string();

        let file = files.add_file(path, source).unwrap();
        let mut error_manager = ErrorManager::default();

        let tokens = scan(&error_manager, &file);
        assert_eq!(tokens[0].kind, TokenKind::Dot);
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

        assert_eq!(tokens[34].kind, TokenKind::Dot);
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
        assert_eq!(tokens[69].kind, TokenKind::NumberLit { raw: "1".to_string(), value: Number::new(1, 0, false) });

        let errors = error_manager.take();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].message, "Unexpected char '#'");
    }

    #[test]
    fn token_spacing_distinguishes_joint_operators() {
        let mut files = FileManager::default();
        let file = files.add_file(PathBuf::from("dummy.mg"), ">== >= =".to_string()).unwrap();
        let tokens = scan(&ErrorManager::default(), &file);

        assert_eq!(tokens[0].kind, TokenKind::GEq);
        assert!(!tokens[0].spacing);
        assert_eq!(tokens[1].kind, TokenKind::Equal);
        assert!(tokens[1].spacing);
        assert_eq!(tokens[2].kind, TokenKind::GEq);
        assert!(!tokens[2].spacing);
        assert_eq!(tokens[3].kind, TokenKind::Equal);
        assert!(!tokens[3].spacing);
    }

    #[test]
    fn adjacent_colons_are_separate_tokens() {
        let mut files = FileManager::default();
        let file = files.add_file(PathBuf::from("dummy.mg"), "pkg::value function::<i32>()".to_string()).unwrap();
        let mut errors = ErrorManager::default();
        let tokens = scan(&errors, &file);
        assert_eq!(tokens[1].kind, TokenKind::Colon);
        assert_eq!(tokens[2].kind, TokenKind::Colon);
        assert_eq!(tokens[5].kind, TokenKind::Colon);
        assert_eq!(tokens[6].kind, TokenKind::Colon);
        assert!(errors.take().is_empty());
    }

    #[test]
    fn positions_use_code_points_and_peeking_does_not_advance() {
        let mut files = FileManager::default();
        let file = files.add_file("positions.mg".into(), "é😀\t\r\n中e\u{301}".into()).unwrap();
        let errors = ErrorManager::default();
        let mut scanner = Scanner::new(&errors, &file);
        for (ch, line, col) in [
            ('é', 1, 1),
            ('😀', 1, 2),
            ('\t', 1, 3),
            ('\r', 1, 4),
            ('\n', 1, 5),
            ('中', 2, 1),
            ('e', 2, 2),
            ('\u{301}', 2, 3),
        ] {
            let expected = Some((ch, Pos { file: file.id, line, col }));
            assert_eq!(scanner.peek(), expected);
            assert_eq!(scanner.peek(), expected);
            assert_eq!(scanner.next(), expected);
        }
        assert_eq!(scanner.peek(), None);
        assert_eq!(scanner.next(), None);
        assert_eq!(scanner.pos, Pos { file: file.id, line: 2, col: 4 });
    }

    #[test]
    fn token_positions_include_whitespace_comments_and_multiline_strings() {
        let mut files = FileManager::default();
        let file = files
            .add_file("positions.mg".into(), " \tlet café = \"é\n😀\"; // comment\r\n \u{2003}café".into())
            .unwrap();
        let errors = ErrorManager::default();
        let tokens = scan(&errors, &file);
        let (eof, tokens) = tokens.split_last().unwrap();
        assert_eq!(eof.kind, TokenKind::Eof);
        assert!(errors.is_empty());
        let positions: Vec<_> = tokens
            .iter()
            .map(|token| {
                assert_eq!(token.pos.file, file.id);
                (token.pos.line, token.pos.col)
            })
            .collect();
        assert_eq!(positions, [(1, 3), (1, 7), (1, 12), (1, 14), (2, 3), (2, 5), (3, 3)]);
        assert_eq!(eof.pos, Pos { file: file.id, line: 3, col: 7 });
    }

    #[test]
    fn empty_and_comment_only_sources_end_with_an_eof_token() {
        let mut files = FileManager::default();
        for (source, line, col) in [("", 1, 1), (" \t\r\n", 2, 1), ("// 😀", 1, 5), ("// 😀\n  ", 2, 3)] {
            let file = files.add_file("empty.mg".into(), source.into()).unwrap();
            let errors = ErrorManager::default();
            let tokens = scan(&errors, &file);
            let (eof, tokens) = tokens.split_last().unwrap();
            assert_eq!(eof.kind, TokenKind::Eof);
            assert!(errors.is_empty());
            assert!(tokens.iter().all(|token| matches!(token.kind, TokenKind::Comment(_))));
            assert_eq!(eof.pos, Pos { file: file.id, line, col });
        }
    }

    #[test]
    fn unterminated_literals_report_the_eof_position() {
        let mut files = FileManager::default();
        for (source, line, col) in [("\"é\n😀", 2, 2), ("'é", 1, 3), ("0x", 1, 3), ("1e", 1, 3)] {
            let file = files.add_file("unterminated.mg".into(), source.into()).unwrap();
            let mut errors = ErrorManager::default();
            let tokens = scan(&errors, &file);
            assert_eq!(tokens.len(), 2);
            let eof = tokens.last().unwrap();
            assert_eq!(eof.kind, TokenKind::Eof);
            assert_eq!(eof.pos, Pos { file: file.id, line, col });
            let errors = errors.take();
            assert_eq!(errors.len(), 1);
            assert_eq!(errors[0].pos, Some(eof.pos));
        }
    }

    fn number_from_str(base: &str, exp: &str, float: bool) -> Number {
        Number { val: BigInt::from_str(base).unwrap(), exp: BigInt::from_str(exp).unwrap(), float }
    }
}
