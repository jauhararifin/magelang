use crate::errors::SyntaxErrorAccumulator;
use crate::tokens::{Token, TokenKind};
use magelang_common::{ErrorAccumulator, FileId, Pos};
use std::collections::VecDeque;

pub(crate) fn scan(err_channel: &ErrorAccumulator, file_id: FileId, source: &str) -> Vec<Token> {
    let mut source_code = VecDeque::new();
    for (offset, ch) in source.chars().enumerate() {
        source_code.push_back(CharPos {
            ch,
            offset: offset as u32,
        });
    }

    let mut scanner = Scanner::new(err_channel, file_id, source_code);
    let mut tokens = vec![];
    while let Some(tok) = scanner.scan() {
        tokens.push(tok);
    }

    tokens
}

#[derive(Debug)]
pub(crate) struct CharPos {
    pub ch: char,
    pub offset: u32,
}

trait QueueExt {
    fn next_if(&mut self, func: impl FnOnce(char) -> bool) -> Option<CharPos>;
}

impl QueueExt for VecDeque<CharPos> {
    fn next_if(&mut self, func: impl FnOnce(char) -> bool) -> Option<CharPos> {
        let ch = self.front()?;
        if func(ch.ch) {
            return self.pop_front();
        }
        None
    }
}

struct Scanner<'err> {
    errors: SyntaxErrorAccumulator<'err>,
    file_id: FileId,
    source_code: VecDeque<CharPos>,
}

static SYMBOLS: &[(&str, TokenKind)] = &[
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
    ("#", TokenKind::Pound),
];

impl<'err> Scanner<'err> {
    fn new(err_channel: &'err ErrorAccumulator, file_id: FileId, source_code: VecDeque<CharPos>) -> Self {
        Self {
            errors: SyntaxErrorAccumulator::new(err_channel, file_id),
            file_id,
            source_code,
        }
    }

    fn scan(&mut self) -> Option<Token> {
        self.skip_whitespace();
        self.scan_builtin()
            .or_else(|| self.scan_word())
            .or_else(|| self.scan_string_lit())
            .or_else(|| self.scan_number_lit())
            .or_else(|| self.scan_comment())
            .or_else(|| self.scan_symbol())
            .or_else(|| self.scan_unexpected_chars())
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.source_code.front() {
            if ch.ch.is_whitespace() {
                self.source_code.pop_front();
            } else {
                break;
            }
        }
    }

    fn scan_builtin(&mut self) -> Option<Token> {
        let tok = self.source_code.next_if(|c| c == '@')?;
        let pos = Pos::new(self.file_id, tok.offset);

        let mut value = String::from(tok.ch);
        let valid_char = |c: char| c.is_alphabetic() || c.is_ascii_digit() || c == '_';
        while let Some(c) = self.source_code.next_if(valid_char) {
            value.push(c.ch);
        }
        Some(Token {
            kind: TokenKind::Builtin,
            value: value.into(),
            pos,
        })
    }

    fn scan_word(&mut self) -> Option<Token> {
        let initial = |c: char| c.is_alphabetic() || c == '_';
        let tok = self.source_code.next_if(initial)?;

        let mut value = String::from(tok.ch);
        let valid_char = |c: char| c.is_alphabetic() || c.is_ascii_digit() || c == '_';
        while let Some(c) = self.source_code.next_if(valid_char) {
            value.push(c.ch);
        }

        let pos = Pos::new(self.file_id, tok.offset);
        let kind = match value.as_str() {
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "fn" => TokenKind::Fn,
            "return" => TokenKind::Return,
            "import" => TokenKind::Import,
            "as" => TokenKind::As,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "continue" => TokenKind::Continue,
            "break" => TokenKind::Break,
            _ => TokenKind::Ident,
        };

        Some(Token {
            kind,
            value: value.into(),
            pos,
        })
    }

    fn scan_string_lit(&mut self) -> Option<Token> {
        enum State {
            Normal,
            AfterBlackslash,
            ReadHex(u8),
            Closed,
        }

        let tok = self.source_code.next_if(|c| c == '"')?;
        let pos = Pos::new(self.file_id, tok.offset);
        let mut last_offset = tok.offset;
        let mut value = String::from('"');
        let mut state = State::Normal;

        while let Some(char_pos) = self.source_code.pop_front() {
            let ch = char_pos.ch;
            let offset = char_pos.offset;
            last_offset = offset;
            value.push(ch);
            match state {
                State::Normal => match ch {
                    '\\' => state = State::AfterBlackslash,
                    '"' => {
                        state = State::Closed;
                        break;
                    }
                    _ => (),
                },
                State::AfterBlackslash => match ch {
                    'n' | 'r' | 't' | '\\' | '0' | '"' | '\'' => state = State::Normal,
                    'x' => state = State::ReadHex(2),
                    _ => {
                        self.errors.unexpected_char(offset, ch);
                        state = State::Normal;
                    }
                },
                State::ReadHex(remaining) => match ch {
                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        state = if remaining == 1 {
                            State::Normal
                        } else {
                            State::ReadHex(remaining - 1)
                        }
                    }
                    '"' => {
                        self.errors.unexpected_char(offset, ch);
                        state = State::Closed;
                    }
                    _ => {
                        self.errors.unexpected_char(offset, ch);
                        state = State::Normal;
                    }
                },
                State::Closed => unreachable!(),
            }
        }

        if !matches!(state, State::Closed) {
            self.errors.missing_closing_quote(last_offset);
        }

        Some(Token {
            kind: TokenKind::StringLit,
            value: value.into(),
            pos,
        })
    }

    fn scan_number_lit(&mut self) -> Option<Token> {
        #[derive(Clone, Copy)]
        pub enum Base {
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
            InvalidSuffix,
        }

        let mut value = String::default();
        let mut offset = None;
        let mut state = State::Init;
        let mut is_fractional = false;

        loop {
            while let Some(c) = self.source_code.next_if(|c| c == '_') {
                value.push(c.ch);
            }

            let Some(char_pos) = self.source_code.front() else {
                break;
            };
            let c = char_pos.ch;
            if offset.is_none() {
                offset = Some(char_pos.offset);
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
                        state = State::InvalidSuffix;
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
                        self.errors.non_decimal_fraction(char_pos.offset);
                    }
                    (Base::Bin, '0' | '1')
                    | (Base::Dec, '0'..='9')
                    | (Base::Oct, '0'..='7')
                    | (Base::Hex, '0'..='9')
                    | (Base::Hex, 'a'..='f')
                    | (Base::Hex, 'A'..='F') => (),
                    (Base::Bin, '2'..='9') => {
                        self.errors.invalid_digit_in_base(char_pos.offset, c, 2);
                    }
                    (Base::Oct, '8'..='9') => {
                        self.errors.invalid_digit_in_base(char_pos.offset, c, 8);
                    }
                    (Base::Bin | Base::Dec | Base::Oct, 'a'..='z' | 'A'..='Z') | (Base::Hex, 'g'..='z' | 'G'..='Z') => {
                        state = State::InvalidSuffix;
                        continue;
                    }
                    _ => break,
                },
                State::Fraction => match c {
                    'e' | 'E' => state = State::Exponent,
                    '0'..='9' => (),
                    'a'..='z' | 'A'..='Z' => {
                        state = State::InvalidSuffix;
                        continue;
                    }
                    _ => break,
                },
                State::Exponent => match c {
                    '-' => state = State::ExponentAfterSign,
                    '0'..='9' => state = State::ExponentAfterSign,
                    'a'..='z' | 'A'..='Z' => {
                        state = State::InvalidSuffix;
                        continue;
                    }
                    _ => break,
                },
                State::ExponentAfterSign => match c {
                    '0'..='9' => (),
                    'a'..='z' | 'A'..='Z' => {
                        state = State::InvalidSuffix;
                        continue;
                    }
                    _ => break,
                },
                State::InvalidSuffix => match c {
                    '0'..='9' | 'a'..='z' | 'A'..='Z' => (),
                    _ => break,
                },
            }

            let c = self.source_code.pop_front().unwrap();
            value.push(c.ch);
        }

        let offset = offset?;

        if is_fractional {
            Some(Token {
                kind: TokenKind::RealLit,
                value: value.into(),
                pos: Pos::new(self.file_id, offset),
            })
        } else {
            Some(Token {
                kind: TokenKind::IntegerLit,
                value: value.into(),
                pos: Pos::new(self.file_id, offset),
            })
        }
    }

    fn scan_symbol(&mut self) -> Option<Token> {
        let top = self.source_code.front()?;
        let (path, offset) = (self.file_id, top.offset);

        let mut value = String::new();
        let mut sym = String::new();
        let mut kind = TokenKind::Invalid;

        while let Some(c) = self.source_code.front() {
            sym.push(c.ch);
            let mut found = false;
            for (_, k) in SYMBOLS.iter().filter(|(op, _)| op.starts_with(&sym)) {
                found = true;
                kind = *k;
            }
            if !found {
                break;
            }

            value.push(c.ch);
            self.source_code.pop_front();
        }

        if let TokenKind::Invalid = kind {
            None
        } else {
            let pos = Pos::new(path, offset);
            Some(Token {
                kind,
                value: value.into(),
                pos,
            })
        }
    }

    fn scan_comment(&mut self) -> Option<Token> {
        let value = self.next_if_prefix("//")?;
        let pos = Pos::new(self.file_id, value[0].offset);
        let mut value = String::from("//");

        while let Some(c) = self.source_code.pop_front() {
            value.push(c.ch);
            if c.ch == '\n' {
                break;
            }
        }

        Some(Token {
            kind: TokenKind::Comment,
            value: value.into(),
            pos,
        })
    }

    fn next_if_prefix(&mut self, prefix: &str) -> Option<Vec<CharPos>> {
        for (i, c) in prefix.chars().enumerate() {
            let charpos = self.source_code.get(i)?;
            if charpos.ch != c {
                return None;
            }
        }

        let mut result = vec![];
        for _ in 0..prefix.len() {
            result.push(self.source_code.pop_front().unwrap());
        }
        Some(result)
    }

    fn scan_unexpected_chars(&mut self) -> Option<Token> {
        let char_pos = self.source_code.pop_front()?;
        let c = char_pos.ch;
        self.errors.unexpected_char(char_pos.offset, c);
        Some(Token {
            kind: TokenKind::Invalid,
            value: String::from(c).into(),
            pos: Pos::new(self.file_id, char_pos.offset),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use magelang_common::Error;

    macro_rules! test_scan_string_lit {
        ($name:ident, $source:expr $(,$errors:expr)*) => {
            #[test]
            fn $name() {
                let err_accumulator = ErrorAccumulator::default();
                let text = $source;
                let source_code = text
                    .char_indices()
                    .map(|(offset, ch)| CharPos {
                        offset: offset as u32,
                        ch,
                    })
                    .collect::<VecDeque<_>>();
                let mut scanner = Scanner::new(&err_accumulator, FileId::new(0), source_code);
                let tok = scanner
                    .scan_string_lit()
                    .expect("expected to scan a single string literal");

                assert_eq!(tok.kind, TokenKind::StringLit);
                assert_eq!(tok.value, text.into());

                let expected_errors = vec![$($errors),*];
                assert_eq!(err_accumulator.take(), expected_errors);
            }
        };
    }

    test_scan_string_lit!(happy_path, "\"some string\"");
    test_scan_string_lit!(
        missing_closing_quote,
        "\"some string",
        Error::new(
            Pos::new(FileId::new(0), 11),
            "Missing closing quote in string literal".to_string()
        )
    );
    test_scan_string_lit!(
        multi_errors,
        "\"some \\xyz string",
        Error::new(Pos::new(FileId::new(0), 8), "Unexpected character 'y'".to_string()),
        Error::new(
            Pos::new(FileId::new(0), 16),
            "Missing closing quote in string literal".to_string()
        )
    );
    test_scan_string_lit!(tab_escape, "\"this char (\\t) is a tab\"");
    test_scan_string_lit!(carriage_return_escape, "\"this char (\\r) is a CR\"");
    test_scan_string_lit!(double_quote_escape, "\"this char (\\\") is a double quote\"");
    test_scan_string_lit!(escaped_raw_bytes, "\"this is a \\x00\\x01\\x02\\xfF raw bytes\"");
    test_scan_string_lit!(
        escaped_raw_bytes_with_err,
        "\"raw byte \\x*f\"",
        Error::new(Pos::new(FileId::new(0), 12), "Unexpected character '*'".to_string())
    );
    test_scan_string_lit!(
        escaped_raw_bytes_with_err2,
        "\"raw byte \\x\"",
        Error::new(Pos::new(FileId::new(0), 12), "Unexpected character '\"'".to_string())
    );
    test_scan_string_lit!(
        escaped_raw_bytes_with_err3,
        "\"raw byte \\xa\"",
        Error::new(Pos::new(FileId::new(0), 13), "Unexpected character '\"'".to_string())
    );
    test_scan_string_lit!(
        escaped_raw_bytes_with_err4,
        "\"raw byte \\xah\"",
        Error::new(Pos::new(FileId::new(0), 13), "Unexpected character 'h'".to_string())
    );

    macro_rules! test_scan_number_lit {
        ($name:ident, $source:expr, $kind:expr $(,$errors:expr)*) => {
            #[test]
            fn $name() {
                let err_accumulator = ErrorAccumulator::default();
                let text = $source;
                let source_code = text
                    .char_indices()
                    .map(|(offset, ch)| CharPos {
                        offset: offset as u32,
                        ch,
                    })
                    .collect::<VecDeque<_>>();
                let mut scanner = Scanner::new(&err_accumulator, FileId::new(0), source_code);
                let tok = scanner
                    .scan_number_lit()
                    .expect("expected to scan a single number literal");

                assert_eq!(tok.kind, $kind);
                assert_eq!(tok.value, text.into());

                let expected_errors = vec![$($errors),*];
                assert_eq!(err_accumulator.take(), expected_errors);
            }
        };
    }

    test_scan_number_lit!(decimal_value, "12345", TokenKind::IntegerLit);
    test_scan_number_lit!(octal_value1, "0777", TokenKind::IntegerLit);
    test_scan_number_lit!(octal_value2, "0o777", TokenKind::IntegerLit);
    test_scan_number_lit!(binary_value, "0b11010101011", TokenKind::IntegerLit);
    test_scan_number_lit!(hex_value, "0xdeadbeef09", TokenKind::IntegerLit);
    test_scan_number_lit!(decimal_floating_value1, "12345.67e-10", TokenKind::RealLit);
    test_scan_number_lit!(decimal_floating_value2, "12345.e-10", TokenKind::RealLit);
    test_scan_number_lit!(decimal_floating_value3, "12e10", TokenKind::IntegerLit);
}
