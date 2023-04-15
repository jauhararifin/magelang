use crate::errors::{missing_closing_quote, unexpected_char, unexpected_newline};
use crate::tokens::{Token, TokenKind};
use magelang_common::{ErrorAccumulator, FileId, FileInfo, Span};
use std::collections::VecDeque;

pub(crate) fn scan(err_channel: &ErrorAccumulator, file_info: &FileInfo) -> Vec<Token> {
    let mut source_code = VecDeque::new();
    for (offset, ch) in file_info.text.chars().enumerate() {
        source_code.push_back(CharPos {
            ch,
            file_id: file_info.id,
            offset,
        });
    }

    let mut scanner = Scanner::new(err_channel, source_code);
    let mut tokens = vec![];
    while let Some(tok) = scanner.scan() {
        tokens.push(tok);
    }

    tokens
}

#[derive(Debug)]
struct CharPos {
    ch: char,
    file_id: FileId,
    offset: usize,
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
    (",", TokenKind::Comma),
    ("%", TokenKind::Mod),
    ("&&", TokenKind::And),
    ("&", TokenKind::BitAnd),
    ("||", TokenKind::Or),
    ("|", TokenKind::BitOr),
    ("^", TokenKind::BitXor),
    ("~", TokenKind::BitNot),
];

struct Scanner<'err> {
    err_channel: &'err ErrorAccumulator,
    source_code: VecDeque<CharPos>,
}

impl<'err> Scanner<'err> {
    fn new(err_channel: &'err ErrorAccumulator, source_code: VecDeque<CharPos>) -> Self {
        Self {
            err_channel,
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
        let tok = self.next_if(initial)?;

        let mut value = String::from(tok.ch);
        let valid_char = |c: char| c.is_alphabetic() || c.is_ascii_digit() || c == '_';
        while let Some(c) = self.next_if(valid_char) {
            value.push(c.ch);
        }

        let span = Span::new(tok.file_id, tok.offset, value.len());
        let kind = match value.as_str() {
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "while" => TokenKind::While,
            "fn" => TokenKind::Fn,
            "return" => TokenKind::Return,
            "import" => TokenKind::Import,
            "as" => TokenKind::As,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Ident,
        };

        Some(Token {
            kind,
            value: value.into(),
            span,
        })
    }

    fn next_if(&mut self, func: impl FnOnce(char) -> bool) -> Option<CharPos> {
        let ch = self.source_code.front()?;
        if func(ch.ch) {
            return self.source_code.pop_front();
        }
        None
    }

    fn scan_string_lit(&mut self) -> Option<Token> {
        let opening_tok = self.next_if(|c| c == '"')?;
        let start_offset = opening_tok.offset;
        let mut end_offset = start_offset;

        let opening_quote = opening_tok.ch;
        let mut value = String::from(opening_tok.ch);
        let mut after_backslash = false;
        let mut is_closed = false;

        while let Some(c) = self.source_code.pop_front() {
            end_offset = c.offset;

            if c.ch == '\n' {
                let span = Span::new(c.file_id, c.offset, 1);
                self.err_channel.push(unexpected_newline(span));
                break;
            }

            if after_backslash {
                let ch = c.ch;
                match ch {
                    'n' => value.push('\n'),
                    'r' => value.push('\r'),
                    't' => value.push('\t'),
                    '\\' => value.push('\\'),
                    '0' => value.push('\0'),
                    '"' => value.push('"'),
                    '\'' => value.push('\''),
                    '`' => value.push('`'),
                    _ => self
                        .err_channel
                        .push(unexpected_char(Span::new(c.file_id, c.offset, 1), ch)),
                };
                after_backslash = false;
            } else if c.ch == '\\' {
                after_backslash = true;
            } else if c.ch == opening_quote {
                value.push(c.ch);
                is_closed = true;
                break;
            } else {
                value.push(c.ch);
            }
        }

        if !is_closed {
            let span = Span::new(opening_tok.file_id, end_offset, 1);
            self.err_channel.push(missing_closing_quote(span));
            None
        } else {
            Some(Token {
                kind: TokenKind::StringLit,
                value: value.into(),
                span: Span::new(opening_tok.file_id, start_offset, end_offset - start_offset + 1),
            })
        }
    }

    fn scan_number_lit(&mut self) -> Option<Token> {
        let tok = self.next_if(|c| c.is_ascii_digit())?;

        let mut value = String::from(tok.ch);
        while let Some(c) = self.next_if(|c| c.is_ascii_digit()) {
            value.push(c.ch);
        }

        if let Some(dot) = self.next_if(|c| c == '.') {
            value.push(dot.ch);
            while let Some(c) = self.next_if(|c| c.is_ascii_digit()) {
                value.push(c.ch);
            }
            let span = Span::new(tok.file_id, tok.offset, value.len());
            return Some(Token {
                kind: TokenKind::RealLit,
                value: value.into(),
                span,
            });
        }

        let span = Span::new(tok.file_id, tok.offset, value.len());
        Some(Token {
            kind: TokenKind::IntegerLit,
            value: value.into(),
            span,
        })
    }

    fn scan_symbol(&mut self) -> Option<Token> {
        let top = self.source_code.front()?;
        let (path, offset) = (top.file_id, top.offset);

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
            let span = Span::new(path, offset, value.len());
            Some(Token {
                kind,
                value: value.into(),
                span,
            })
        }
    }

    fn scan_comment(&mut self) -> Option<Token> {
        let value = self.next_if_prefix("//")?;
        let mut span = Span::new(value[0].file_id, value[0].offset, 2);
        let mut value = String::from("//");

        while let Some(c) = self.source_code.pop_front() {
            value.push(c.ch);
            if c.ch == '\n' {
                break;
            }
            span.union(&Span::new(c.file_id, c.offset, 1));
        }

        Some(Token {
            kind: TokenKind::Comment,
            value: value.into(),
            span,
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
        let span = Span::new(char_pos.file_id, char_pos.offset, 1);
        self.err_channel.push(unexpected_char(span, c));
        Some(Token {
            kind: TokenKind::Invalid,
            value: String::from(c).into(),
            span: Span::new(char_pos.file_id, char_pos.offset, 1),
        })
    }
}
