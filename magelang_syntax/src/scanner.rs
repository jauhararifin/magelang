use crate::tokens::{Token, TokenKind};
use lazy_static::lazy_static;
use magelang_common::{Error, ErrorAccumulator, FileId, FileInfo, Span};
use std::{iter::Peekable, vec::IntoIter};

pub(crate) fn scan(err_channel: &ErrorAccumulator, file_info: &FileInfo) -> Vec<Token> {
    let mut source_code = vec![];
    for (offset, ch) in file_info.text.chars().enumerate() {
        source_code.push(CharPos {
            ch,
            file_id: file_info.id,
            offset,
        });
    }

    let mut scanner = Scanner::new(err_channel, source_code.into_iter());
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

lazy_static! {
    static ref SYMBOLS: Vec<(&'static str, TokenKind)> = vec![
        ("=", TokenKind::Equal),
        (":", TokenKind::Colon),
        (";", TokenKind::SemiColon),
        ("{", TokenKind::OpenBlock),
        ("}", TokenKind::CloseBlock),
        ("(", TokenKind::OpenBrac),
        (")", TokenKind::CloseBrac),
        (",", TokenKind::Comma),
    ];
}

struct Scanner<'a> {
    err_channel: &'a ErrorAccumulator,
    source_code: Peekable<IntoIter<CharPos>>,
}

impl<'a> Scanner<'a> {
    fn new(err_channel: &'a ErrorAccumulator, source_code: impl Iterator<Item = CharPos>) -> Self {
        Self {
            err_channel,
            source_code: source_code.collect::<Vec<_>>().into_iter().peekable(),
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
        while let Some(ch) = self.source_code.peek() {
            if ch.ch.is_whitespace() {
                self.source_code.next();
            } else {
                break;
            }
        }
    }

    fn scan_word(&mut self) -> Option<Token> {
        let initial = |c: &CharPos| c.ch.is_alphabetic() || c.ch == '_';
        let tok = self.source_code.next_if(initial)?;

        let mut value = String::from(tok.ch);
        let valid_char = |c: &CharPos| c.ch.is_alphabetic() || c.ch.is_ascii_digit() || c.ch == '_';
        while let Some(c) = self.source_code.next_if(valid_char) {
            value.push(c.ch);
        }

        let span = Span::new(tok.file_id, tok.offset, value.len());
        let kind = match value.as_str() {
            "fn" => TokenKind::Fn,
            "return" => TokenKind::Return,
            "import" => TokenKind::Import,
            _ => TokenKind::Ident,
        };

        Some(Token {
            kind,
            value: value.into(),
            span,
        })
    }

    fn scan_string_lit(&mut self) -> Option<Token> {
        let opening_tok = self.source_code.next_if(|c| c.ch == '"')?;
        let start_offset = opening_tok.offset;
        let mut end_offset = start_offset;

        let opening_quote = opening_tok.ch;
        let mut value = String::from(opening_tok.ch);
        let mut after_backslash = false;
        let mut is_closed = false;

        for c in self.source_code.by_ref() {
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
        let tok = self.source_code.next_if(|c| c.ch.is_ascii_digit())?;

        let mut value = String::from(tok.ch);
        while let Some(c) = self.source_code.next_if(|c| c.ch.is_ascii_digit()) {
            value.push(c.ch);
        }

        if let Some(dot) = self.source_code.next_if(|c| c.ch == '.') {
            value.push(dot.ch);
            while let Some(c) = self.source_code.next_if(|c| c.ch.is_ascii_digit()) {
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
        let top = self.source_code.peek()?;
        let (path, offset) = (top.file_id, top.offset);

        let mut value = String::new();
        let mut sym = String::new();
        let mut kind = TokenKind::Invalid;

        while let Some(c) = self.source_code.peek() {
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
            self.source_code.next();
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

    // scan_comment should be called after scan_symbol. scan_comment assumes that the
    // first two characters are not a valid operator symbol. scan_comment also assumes
    // that if the first character read is '/', the second character must be '/' as well.
    fn scan_comment(&mut self) -> Option<Token> {
        let c = self.source_code.next_if(|c| c.ch == '/')?;

        let mut value = String::from(c.ch);

        let Some(c) = self.source_code.next_if(|c| c.ch == '/') else {
            unreachable!();
        };
        value.push(c.ch);

        for c in self.source_code.by_ref() {
            value.push(c.ch);
            if c.ch == '\n' {
                break;
            }
        }

        let span = Span::new(c.file_id, c.offset, value.len());
        Some(Token {
            kind: TokenKind::Comment,
            value: value.into(),
            span,
        })
    }

    fn scan_unexpected_chars(&mut self) -> Option<Token> {
        let char_pos = self.source_code.next()?;
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

fn unexpected_char(span: Span, ch: char) -> Error {
    Error::new(span, format!("Unexpected character '{}'", ch))
}

fn unexpected_newline(span: Span) -> Error {
    Error::new(span, String::from("Unexpected newline"))
}

fn missing_closing_quote(span: Span) -> Error {
    Error::new(span, String::from("Missing closing quote in string literal"))
}
