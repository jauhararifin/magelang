use crate::errors::{
    invalid_digit_in_base, missing_closing_quote, non_decimal_fraction, unexpected_char, unexpected_newline,
};
use crate::number_lit::{scan_number_lit, NumberLitErrorKind};
use crate::string_lit::{scan_string_lit, StringLitErrKind};
use crate::tokens::{Token, TokenKind};
use magelang_common::{ErrorAccumulator, FileId, FileInfo, Pos};
use std::collections::VecDeque;

pub(crate) fn scan(err_channel: &ErrorAccumulator, file_info: &FileInfo) -> Vec<Token> {
    let mut source_code = VecDeque::new();
    for (offset, ch) in file_info.text.chars().enumerate() {
        source_code.push_back(CharPos {
            ch,
            offset: offset as u32,
        });
    }

    let mut scanner = Scanner::new(err_channel, file_info.id, source_code);
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

struct Scanner<'err> {
    err_channel: &'err ErrorAccumulator,
    file_id: FileId,
    source_code: VecDeque<CharPos>,
}

impl<'err> Scanner<'err> {
    fn new(err_channel: &'err ErrorAccumulator, file_id: FileId, source_code: VecDeque<CharPos>) -> Self {
        Self {
            err_channel,
            file_id,
            source_code,
        }
    }

    fn scan(&mut self) -> Option<Token> {
        self.skip_whitespace();
        self.scan_word()
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
        let string_lit_result = scan_string_lit(self.source_code.iter())?;

        for err in &string_lit_result.errors {
            match err.kind {
                StringLitErrKind::FoundNewline => self
                    .err_channel
                    .push(unexpected_newline(Pos::new(self.file_id, err.offset))),
                StringLitErrKind::MissingClosingQuote => self
                    .err_channel
                    .push(missing_closing_quote(Pos::new(self.file_id, err.offset))),
                StringLitErrKind::UnexpectedChar(ch) => {
                    self.err_channel
                        .push(unexpected_char(Pos::new(self.file_id, err.offset), ch));
                }
            }
        }

        for _ in 0..string_lit_result.consumed {
            self.source_code.pop_front();
        }

        Some(Token {
            kind: TokenKind::StringLit,
            value: string_lit_result.value.into(),
            pos: Pos::new(self.file_id, string_lit_result.offset),
        })
    }

    fn scan_number_lit(&mut self) -> Option<Token> {
        let number_lit_result = scan_number_lit(self.source_code.iter())?;

        for err in &number_lit_result.errors {
            match err.kind {
                NumberLitErrorKind::InvalidDigit { digit, base } => {
                    self.err_channel
                        .push(invalid_digit_in_base(Pos::new(self.file_id, err.offset), digit, base))
                }
                NumberLitErrorKind::NonDecimalFraction => self
                    .err_channel
                    .push(non_decimal_fraction(Pos::new(self.file_id, err.offset))),
            }
        }

        for _ in 0..number_lit_result.consumed {
            self.source_code.pop_front();
        }

        if number_lit_result.is_fractional {
            Some(Token {
                kind: TokenKind::RealLit,
                value: number_lit_result.value.into(),
                pos: Pos::new(self.file_id, number_lit_result.offset),
            })
        } else {
            Some(Token {
                kind: TokenKind::IntegerLit,
                value: number_lit_result.value.into(),
                pos: Pos::new(self.file_id, number_lit_result.offset),
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
            let span = Pos::new(path, offset);
            Some(Token {
                kind,
                value: value.into(),
                pos: span,
            })
        }
    }

    fn scan_comment(&mut self) -> Option<Token> {
        let value = self.next_if_prefix("//")?;
        let span = Pos::new(self.file_id, value[0].offset);
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
            pos: span,
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
        let span = Pos::new(self.file_id, char_pos.offset);
        self.err_channel.push(unexpected_char(span, c));
        Some(Token {
            kind: TokenKind::Invalid,
            value: String::from(c).into(),
            pos: Pos::new(self.file_id, char_pos.offset),
        })
    }
}
