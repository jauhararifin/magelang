use crate::error::ErrorReporter;
use crate::token::{File, Pos, Token, TokenKind};
use std::collections::VecDeque;

pub fn scan(errors: &impl ErrorReporter, file: &File) -> Vec<Token> {
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
    text: VecDeque<char>,
    offset: usize,
}

impl<'a, Error: ErrorReporter> Scanner<'a, Error> {
    fn new(errors: &'a Error, file: &File) -> Self {
        Self {
            errors,
            file_offset: file.offset,
            text: file.text.chars().collect(),
            offset: 0,
        }
    }

    fn scan(&mut self) -> Option<Token> {
        self.skip_whitespace();
        self.scan_word()
            .or_else(|| self.scan_string_lit())
            .or_else(|| self.scan_number_lit())
            .or_else(|| self.scan_comments())
            .or_else(|| self.scan_symbols())
            .or_else(|| self.scan_invalid())
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.text.front() {
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

        Some(Token { kind, value, pos })
    }

    fn scan_string_lit(&mut self) -> Option<Token> {
        enum State {
            Normal,
            AfterBlackslash,
            ReadHex(u8),
            Closed,
        }

        let (_, pos) = self.next_if(|c| c == '"')?;
        let mut last_pos = pos;
        let mut value = String::from('"');
        let mut state = State::Normal;

        while let Some((c, pos)) = self.next() {
            last_pos = pos;
            value.push(c);
            match state {
                State::Normal => match c {
                    '\\' => state = State::AfterBlackslash,
                    '"' => {
                        state = State::Closed;
                        break;
                    }
                    _ => (),
                },
                State::AfterBlackslash => match c {
                    'n' | 'r' | 't' | '\\' | '0' | '"' | '\'' => state = State::Normal,
                    'x' => state = State::ReadHex(2),
                    _ => {
                        self.errors.unexpected_char(pos, c);
                        state = State::Normal;
                    }
                },
                State::ReadHex(remaining) => match c {
                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        state = if remaining == 1 {
                            State::Normal
                        } else {
                            State::ReadHex(remaining - 1)
                        }
                    }
                    '"' => {
                        self.errors.unexpected_char(pos, c);
                        state = State::Closed;
                        break;
                    }
                    _ => {
                        self.errors.unexpected_char(pos, c);
                        state = State::Normal;
                    }
                },
                State::Closed => unreachable!(),
            }
        }

        if !matches!(state, State::Closed) {
            self.errors.missing_closing_quote(last_pos);
        }

        Some(Token {
            kind: TokenKind::StringLit,
            value,
            pos,
        })
    }

    fn scan_number_lit(&mut self) -> Option<Token> {
        #[derive(Clone, Copy)]
        enum Base {
            Bin,
            Dec,
            Oct,
            Hex,
        }
        #[derive(Clone, Copy)]
        enum State {
            Init,
            Prefix,
            Integer(Base),
            Fraction,
            Exponent,
            ExponentAfterSign,
            InvalidSuffix(Pos),
        }

        let mut value = String::default();
        let mut pos = None;
        let mut state = State::Init;
        let mut is_fractional = false;
        let mut invalid_suffix = String::default();

        loop {
            while let Some((c, _)) = self.next_if(|c| c == '_') {
                value.push(c);
            }

            let Some((c, char_pos)) = self.peek() else {
                break;
            };
            if pos.is_none() {
                pos = Some(char_pos);
            }

            match state {
                State::Init => match c {
                    '0' => {
                        state = State::Prefix;
                    }
                    '1'..='9' => state = State::Integer(Base::Dec),
                    _ => return None,
                },
                State::Prefix => match c {
                    'x' => state = State::Integer(Base::Hex),
                    'b' => state = State::Integer(Base::Bin),
                    'o' => state = State::Integer(Base::Oct),
                    '0'..='7' => state = State::Integer(Base::Oct),
                    'e' | 'E' => state = State::Exponent,
                    '.' => {
                        is_fractional = true;
                        state = State::Fraction;
                    }
                    'a'..='z' | 'A'..='Z' => {
                        state = State::InvalidSuffix(char_pos);
                        continue;
                    }
                    _ => break,
                },
                State::Integer(base) => match (base, c) {
                    (Base::Dec, 'e' | 'E') => state = State::Exponent,
                    (Base::Dec, '.') => {
                        is_fractional = true;
                        state = State::Fraction;
                    }
                    (_, '.') => {
                        is_fractional = true;
                        self.errors.non_decimal_fraction(char_pos);
                    }
                    (Base::Bin, '0' | '1')
                    | (Base::Dec, '0'..='9')
                    | (Base::Oct, '0'..='7')
                    | (Base::Hex, '0'..='9')
                    | (Base::Hex, 'a'..='f')
                    | (Base::Hex, 'A'..='F') => (),
                    (Base::Bin, '2'..='9') => self.errors.invalid_digit_in_base(char_pos, c, 2),
                    (Base::Oct, '8'..='9') => self.errors.invalid_digit_in_base(char_pos, c, 8),
                    (Base::Bin | Base::Dec | Base::Oct, 'a'..='z' | 'A'..='Z')
                    | (Base::Hex, 'g'..='z' | 'G'..='Z') => {
                        state = State::InvalidSuffix(char_pos);
                        continue;
                    }
                    _ => break,
                },
                State::Fraction => match c {
                    'e' | 'E' => state = State::Exponent,
                    '0'..='9' => (),
                    'a'..='z' | 'A'..='Z' => {
                        state = State::InvalidSuffix(char_pos);
                        continue;
                    }
                    _ => break,
                },
                State::Exponent => match c {
                    '-' => {
                        is_fractional = true;
                        state = State::ExponentAfterSign;
                    }
                    '0'..='9' => state = State::ExponentAfterSign,
                    'a'..='z' | 'A'..='Z' => {
                        state = State::InvalidSuffix(char_pos);
                        continue;
                    }
                    _ => {
                        self.errors.missing_exponent_digits(char_pos);
                        break;
                    }
                },
                State::ExponentAfterSign => match c {
                    '0'..='9' => (),
                    'a'..='z' | 'A'..='Z' => {
                        state = State::InvalidSuffix(char_pos);
                        continue;
                    }
                    _ => break,
                },
                State::InvalidSuffix(..) => match c {
                    '0'..='9' | 'a'..='z' | 'A'..='Z' => invalid_suffix.push(c),
                    _ => break,
                },
            }

            let (c, _) = self.next().unwrap();
            value.push(c);
        }

        if let State::InvalidSuffix(pos) = state {
            self.errors.invalid_number_suffix(pos, &invalid_suffix);
        }

        let pos = pos?;
        if is_fractional {
            Some(Token {
                kind: TokenKind::RealLit,
                value,
                pos,
            })
        } else {
            Some(Token {
                kind: TokenKind::IntegerLit,
                value,
                pos,
            })
        }
    }

    fn scan_comments(&mut self) -> Option<Token> {
        if self.text.len() < 2 {
            return None;
        }
        if self.text[0] != '/' || self.text[1] != '/' {
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
            value,
            pos,
        })
    }

    const SYMBOLS: &[(&'static str, TokenKind)] = &[
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

        let mut value = String::new();
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

            value.push(c);
            self.next();
        }

        if let TokenKind::Invalid = kind {
            None
        } else {
            Some(Token { kind, value, pos })
        }
    }

    fn scan_invalid(&mut self) -> Option<Token> {
        let (c, pos) = self.next()?;
        self.errors.unexpected_char(pos, c);
        Some(Token {
            kind: TokenKind::Invalid,
            value: c.to_string(),
            pos,
        })
    }

    fn next_if(&mut self, func: impl FnOnce(char) -> bool) -> Option<(char, Pos)> {
        let ch = self.text.front()?;
        if func(*ch) {
            self.next()
        } else {
            None
        }
    }

    fn next(&mut self) -> Option<(char, Pos)> {
        let c = self.text.pop_front()?;
        let pos = self.get_pos();

        self.offset += 1;
        Some((c, pos))
    }

    fn get_pos(&self) -> Pos {
        self.file_offset.with_offset(self.offset)
    }

    fn peek(&self) -> Option<(char, Pos)> {
        self.text.front().map(|c| (*c, self.get_pos()))
    }
}

trait ScanningError: ErrorReporter {
    fn unexpected_char(&self, pos: Pos, ch: char) {
        self.report(pos, format!("Unexpected char '{ch}'"));
    }

    fn missing_closing_quote(&self, pos: Pos) {
        self.report(pos, String::from("Missing closing quote in string literal"));
    }

    fn non_decimal_fraction(&self, pos: Pos) {
        self.report(
            pos,
            String::from("can only use decimal number for fractional number literal"),
        );
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
        assert_eq!(&tokens[0].value, "// a simple comment\n");

        assert_eq!(tokens[1].kind, TokenKind::Comment);
        assert_eq!(&tokens[1].value, "// another comment\n");
        assert_eq!(tokens[2].kind, TokenKind::Comment);
        assert_eq!(&tokens[2].value, "// with multiline\n");
        assert_eq!(tokens[3].kind, TokenKind::Comment);
        assert_eq!(&tokens[3].value, "// grouped comment\n");

        assert_eq!(tokens[4].kind, TokenKind::Let);
        assert_eq!(tokens[5].kind, TokenKind::Ident);
        assert_eq!(tokens[6].kind, TokenKind::Equal);
        assert_eq!(tokens[7].kind, TokenKind::IntegerLit);
        assert_eq!(tokens[8].kind, TokenKind::SemiColon);
        assert_eq!(tokens[9].kind, TokenKind::Comment);
        assert_eq!(&tokens[9].value, "// comment in the end\n");

        assert_eq!(tokens[10].kind, TokenKind::Comment);
        assert_eq!(
            &tokens[10].value,
            "// nested comment // is considered a single comment\n"
        );

        assert_eq!(&tokens[11].value, "// unicode\n");
        assert_eq!(&tokens[12].value, "// a۰۱۸\n");
        assert_eq!(&tokens[13].value, "// foo६४\n");
        assert_eq!(&tokens[14].value, "// ŝ\n");
        assert_eq!(&tokens[15].value, "// ŝfoo\n");

        assert_eq!(tokens[16].kind, TokenKind::StringLit);
        assert_eq!(&tokens[16].value, r#""this is a string // not a comment""#);

        assert_eq!(tokens[17].kind, TokenKind::StringLit);
        assert_eq!(
            &tokens[17].value,
            "\"this is\na multi line // not a comment\nstring\""
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
        assert_eq!(&tokens[0].value, r#""basic string""#);
        assert_eq!(tokens[1].kind, TokenKind::StringLit);
        assert_eq!(
            &tokens[1].value,
            r#""this is an emoji 😀 😃 😄 😁 😆 😅 😂. It should scanned properly""#
        );
        assert_eq!(tokens[2].kind, TokenKind::StringLit);
        assert_eq!(&tokens[2].value, r#""this is a string // not a comment""#);
        assert_eq!(tokens[3].kind, TokenKind::StringLit);
        assert_eq!(
            &tokens[3].value,
            r#""hex escape like \x00\x12\xAb\xCd\xef\xEF are fine""#
        );
        assert_eq!(tokens[4].kind, TokenKind::StringLit);
        assert_eq!(&tokens[4].value, r#""\n\r\t\\\0\"\''""#);
        assert_eq!(tokens[5].kind, TokenKind::StringLit);
        assert_eq!(
            &tokens[5].value,
            r#""multiline
            string""#
        );
        assert_eq!(tokens[6].kind, TokenKind::StringLit);
        assert_eq!(
            &tokens[6].value,
            r#""invalid escape \a\b\c\d\e\f\g\h\i\j\k\l\m\o\p\q\s\u\v\w\y\z""#
        );
        assert_eq!(tokens[7].kind, TokenKind::StringLit);
        assert_eq!(&tokens[7].value, r#""invalid hex \xgh\x\\x""#);
        assert_eq!(tokens[8].kind, TokenKind::StringLit);
        assert_eq!(&tokens[8].value, r#""missing closing quote"#);

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
            .all(|token| token.kind == TokenKind::IntegerLit));
        assert_eq!(&tokens[0].value, "0");
        assert_eq!(&tokens[1].value, "1");
        assert_eq!(&tokens[2].value, "12345678_90123455561_090");
        assert_eq!(&tokens[3].value, "01234_567");
        assert_eq!(&tokens[4].value, "0xabc___def01234567890deadbeef0__10_");
        assert_eq!(&tokens[5].value, "0_o012345670123_4567");
        assert_eq!(&tokens[6].value, "0b_11010101001010101010");
        assert_eq!(&tokens[7].value, "0_b11010101001010101010");
        assert_eq!(&tokens[8].value, "0__b__11010101001010101010");

        assert_eq!(tokens[9].kind, TokenKind::RealLit);
        assert_eq!(&tokens[9].value, "123.123");
        assert_eq!(tokens[10].kind, TokenKind::IntegerLit);
        assert_eq!(&tokens[10].value, "123e123");
        assert_eq!(tokens[11].kind, TokenKind::RealLit);
        assert_eq!(&tokens[11].value, "123e-123");
        assert_eq!(tokens[12].kind, TokenKind::IntegerLit);
        assert_eq!(&tokens[12].value, "123E123");
        assert_eq!(tokens[13].kind, TokenKind::RealLit);
        assert_eq!(&tokens[13].value, "123E-123");
        assert_eq!(tokens[14].kind, TokenKind::RealLit);
        assert_eq!(&tokens[14].value, "1.23e123");
        assert_eq!(tokens[15].kind, TokenKind::RealLit);
        assert_eq!(&tokens[15].value, "0.123");
        assert_eq!(tokens[16].kind, TokenKind::IntegerLit);
        assert_eq!(&tokens[16].value, "0e123");

        assert_eq!(tokens[17].kind, TokenKind::IntegerLit);
        assert_eq!(&tokens[17].value, "0123abcdef456");
        assert_eq!(tokens[18].kind, TokenKind::IntegerLit);
        assert_eq!(&tokens[18].value, "0abcde");
        assert_eq!(tokens[19].kind, TokenKind::RealLit);
        assert_eq!(&tokens[19].value, "0x123.abcd");
        assert_eq!(tokens[20].kind, TokenKind::RealLit);
        assert_eq!(&tokens[20].value, "0b101.101");
        assert_eq!(tokens[21].kind, TokenKind::RealLit);
        assert_eq!(&tokens[21].value, "0o123.123");
        assert_eq!(tokens[22].kind, TokenKind::IntegerLit);
        assert_eq!(&tokens[22].value, "0b123");
        assert_eq!(tokens[23].kind, TokenKind::IntegerLit);
        assert_eq!(&tokens[23].value, "123eabc");
        assert_eq!(tokens[24].kind, TokenKind::RealLit);
        assert_eq!(&tokens[24].value, "123e-1a");
        assert_eq!(tokens[25].kind, TokenKind::IntegerLit);
        assert_eq!(&tokens[25].value, "0xabcghijklmnopqrstuvwxyz");
        assert_eq!(tokens[26].kind, TokenKind::RealLit);
        assert_eq!(&tokens[26].value, "123.abcde");
        assert_eq!(tokens[27].kind, TokenKind::IntegerLit);
        assert_eq!(&tokens[27].value, "123e");

        let errors = error_manager.take();
        assert_eq!(errors.len(), 12);
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
            "can only use decimal number for fractional number literal",
        );
        assert_eq!(
            errors[3].message,
            "can only use decimal number for fractional number literal",
        );
        assert_eq!(
            errors[4].message,
            "can only use decimal number for fractional number literal",
        );
        assert_eq!(
            errors[5].message,
            "Cannot use '2' in 2-base integer literal",
        );
        assert_eq!(
            errors[6].message,
            "Cannot use '3' in 2-base integer literal",
        );
        assert_eq!(
            errors[7].message,
            "Invalid suffix \"abc\" for number literal",
        );
        assert_eq!(errors[8].message, "Invalid suffix \"a\" for number literal",);
        assert_eq!(
            errors[9].message,
            "Invalid suffix \"ghijklmnopqrstuvwxyz\" for number literal",
        );
        assert_eq!(
            errors[10].message,
            "Invalid suffix \"abcde\" for number literal",
        );
        assert_eq!(errors[11].message, "The exponent has no digits",);
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
        assert_eq!(tokens[69].kind, TokenKind::IntegerLit);

        let errors = error_manager.take();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].message, "Unexpected char '#'");
    }
}
