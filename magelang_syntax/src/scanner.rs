use crate::error::ErrorReporter;
use crate::number::{Number, NumberBuilder, NumberError};
use crate::string::{StringBuilder, StringError};
use crate::token::{File, Pos, Token, TokenKind};

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
                let value = Self::hex_to_int(c);

                let Some((c, p)) = self.next() else {
                    return self.scan_char_closing(pos, raw, 0 as char);
                };
                raw.push(c);

                match c {
                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        let value = value << 4 | Self::hex_to_int(c);
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

    fn hex_to_int(c: char) -> u8 {
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
        let mut pos = None;

        let mut builder = StringBuilder::default();
        while let Some((c, p)) = self.peek() {
            if !builder.add(c) {
                break;
            }
            self.next();
            if pos.is_none() {
                pos = Some(p);
            }
        }

        let literal = builder.build_literal();
        let pos = pos?;
        for error in literal.errors {
            match error {
                StringError::UnknownEscape { offset, c } => {
                    self.errors.unexpected_char(pos.with_offset(offset), c)
                }
                StringError::UnexpectedHex { offset, c } => {
                    self.errors.unexpected_char(pos.with_offset(offset), c)
                }
                StringError::MissingClosingQuote { offset } => {
                    self.errors.missing_closing_quote(pos.with_offset(offset));
                }
            }
        }

        Some(Token {
            kind: TokenKind::StringLit,
            value_str: literal.raw,
            value_bytes: literal.value,
            char_value: '\0',
            value_number: Number::default(),
            pos,
        })
    }

    fn scan_number_lit(&mut self) -> Option<Token> {
        let mut pos = None;
        let mut builder = NumberBuilder::default();
        while let Some((c, char_pos)) = self.peek() {
            if pos.is_none() {
                pos = Some(char_pos);
            }
            if !builder.add(c) {
                break;
            }
            self.next();
        }

        let result = builder.build()?;
        let pos = pos?;

        for err in result.errors {
            match err {
                NumberError::InvalidSuffix { offset } => self
                    .errors
                    .invalid_number_suffix(pos.with_offset(offset), &result.invalid_suffix),
                NumberError::InvalidDigit {
                    offset,
                    base,
                    digit,
                } => self
                    .errors
                    .invalid_digit_in_base(pos.with_offset(offset), digit, base as u8),
                NumberError::MissingExponent { offset } => {
                    self.errors.missing_exponent_digits(pos.with_offset(offset))
                }
            }
        }

        Some(Token {
            kind: TokenKind::NumberLit,
            value_str: result.raw,
            value_bytes: Vec::default(),
            char_value: '\0',
            value_number: result.value,
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
            std::str::from_utf8(&tokens[16].value_bytes).unwrap(),
            r#"this is a string // not a comment"#
        );

        assert_eq!(tokens[17].kind, TokenKind::StringLit);
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
        assert_eq!(
            std::str::from_utf8(&tokens[0].value_bytes).unwrap(),
            r#"basic string"#
        );
        assert_eq!(tokens[1].kind, TokenKind::StringLit);
        assert_eq!(
            std::str::from_utf8(&tokens[1].value_bytes).unwrap(),
            r#"this is an emoji 😀 😃 😄 😁 😆 😅 😂. It should scanned properly"#
        );
        assert_eq!(tokens[2].kind, TokenKind::StringLit);
        assert_eq!(
            std::str::from_utf8(&tokens[2].value_bytes).unwrap(),
            r#"this is a string // not a comment"#
        );
        assert_eq!(tokens[3].kind, TokenKind::StringLit);
        assert_eq!(
            tokens[3].value_bytes.as_slice(),
            b"hex escape like \x00\x12\xAb\xCd\xef\xEF are fine",
        );
        assert_eq!(tokens[4].kind, TokenKind::StringLit);
        assert_eq!(&tokens[4].value_bytes, b"\n\r\t\\\0\"\''",);
        assert_eq!(tokens[5].kind, TokenKind::StringLit);
        assert_eq!(
            std::str::from_utf8(&tokens[5].value_bytes).unwrap(),
            r#"multiline
            string"#
        );
        assert_eq!(tokens[6].kind, TokenKind::StringLit);
        assert_eq!(&tokens[6].value_bytes, b"invalid escape ",);
        assert_eq!(tokens[7].kind, TokenKind::StringLit);
        assert_eq!(&tokens[7].value_bytes, b"invalid hex h",);
        assert_eq!(tokens[8].kind, TokenKind::StringLit);
        assert_eq!(
            std::str::from_utf8(&tokens[8].value_bytes).unwrap(),
            r#"missing closing quote"#
        );

        let errors = error_manager.take();
        assert_eq!(errors.len(), 26);
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
        assert_eq!(errors[24].message, "Unexpected char '\"'");
        assert_eq!(
            errors[25].message,
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
        "#
        .to_string();
        let file = files.add_file(path, source);
        let mut error_manager = ErrorManager::default();

        let tokens = scan(&error_manager, &file);

        assert!(tokens[0..8]
            .iter()
            .all(|token| token.kind == TokenKind::NumberLit));
        assert_eq!(&tokens[0].value_str, "0");
        assert_eq!(&tokens[1].value_str, "1");
        assert_eq!(&tokens[2].value_str, "12345678_90123455561_090");
        assert_eq!(&tokens[3].value_str, "01234_567");
        assert_eq!(&tokens[4].value_str, "0xabc___def01234567890deadbeef0__10_");
        assert_eq!(&tokens[5].value_str, "0_o012345670123_4567");
        assert_eq!(&tokens[6].value_str, "0b_11010101001010101010");
        assert_eq!(&tokens[7].value_str, "0_b11010101001010101010");
        assert_eq!(&tokens[8].value_str, "0__b__11010101001010101010");

        assert_eq!(tokens[9].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[9].value_str, "123.123");
        assert_eq!(tokens[10].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[10].value_str, "123e123");
        assert_eq!(tokens[11].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[11].value_str, "123e-123");
        assert_eq!(tokens[12].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[12].value_str, "123E123");
        assert_eq!(tokens[13].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[13].value_str, "123E-123");
        assert_eq!(tokens[14].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[14].value_str, "1.23e123");
        assert_eq!(tokens[15].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[15].value_str, "0.123");
        assert_eq!(tokens[16].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[16].value_str, "0e123");

        assert_eq!(tokens[17].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[17].value_str, "0123abcdef456");
        assert_eq!(tokens[18].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[18].value_str, "0abcde");
        assert_eq!(tokens[19].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[19].value_str, "0x123");
        assert_eq!(tokens[20].kind, TokenKind::Dot);
        assert_eq!(tokens[21].kind, TokenKind::Ident);

        assert_eq!(tokens[22].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[22].value_str, "0b101");
        assert_eq!(tokens[23].kind, TokenKind::Dot);
        assert_eq!(tokens[24].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[24].value_str, "101");

        assert_eq!(tokens[25].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[25].value_str, "0o123");
        assert_eq!(tokens[26].kind, TokenKind::Dot);
        assert_eq!(tokens[27].kind, TokenKind::NumberLit);
        assert_eq!(&tokens[27].value_str, "123");

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

        let errors = error_manager.take();
        assert_eq!(errors.len(), 9);
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
