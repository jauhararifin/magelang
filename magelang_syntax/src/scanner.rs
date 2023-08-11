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
            InvalidSuffix,
        }

        let mut value = String::default();
        let mut pos = None;
        let mut state = State::Init;
        let mut is_fractional = false;

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

            let (c, _) = self.next().unwrap();
            value.push(c);
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
        self.report(pos, format!("Unexpected char '{ch}"));
    }

    fn missing_closing_quote(&self, pos: Pos) {
        self.report(pos, String::from("Missing closing quote in string literal"));
    }

    fn non_decimal_fraction(&self, pos: Pos) {
        self.report(
            pos,
            String::from("Can only use decimal number for fractional number literal"),
        );
    }

    fn invalid_digit_in_base(&self, pos: Pos, digit: char, base: u8) {
        self.report(
            pos,
            format!("Cannot use '{digit}' in {base}-base integer literal"),
        );
    }
}

impl<T> ScanningError for T where T: ErrorReporter {}
