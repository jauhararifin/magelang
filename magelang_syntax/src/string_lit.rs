use crate::scanner::CharPos;
use std::rc::Rc;

enum State {
    Normal,
    AfterBlackslash,
    ReadBinary0,
    ReadBinary1(u8),
    Closed,
}

pub(crate) fn scan_string_lit<'a>(source: impl Iterator<Item = &'a CharPos>) -> Option<StringLitResult> {
    let mut text_iter = source.peekable();

    let opening_tok = text_iter.next_if(|ch| ch.ch == '"')?;
    let start_offset = opening_tok.offset;
    let opening_quote = opening_tok.ch;
    let mut end_offset = start_offset;

    let mut value = String::from(opening_quote);
    let mut content = Vec::<u8>::default();
    let mut state = State::Normal;
    let mut errors = vec![];

    while let Some(char_pos) = text_iter.next() {
        let (ch, offset) = (char_pos.ch, char_pos.offset);
        end_offset = offset;

        if ch == '\n' {
            errors.push(StringLitError {
                kind: StringLitErrKind::FoundNewline,
                offset,
            });

            let mut consumed = end_offset - start_offset + 1;
            for char_pos in text_iter {
                consumed += 1;
                if char_pos.ch == opening_quote {
                    break;
                }
            }

            return Some(StringLitResult {
                value,
                content,
                offset: start_offset,
                len: end_offset - start_offset + 1,
                consumed,
                errors,
            });
        }

        value.push(ch);

        match state {
            State::Normal => {
                if ch == '\\' {
                    state = State::AfterBlackslash;
                } else if ch == opening_quote {
                    state = State::Closed;
                    break;
                } else {
                    let mut buff = [0u8; 4];
                    let res = ch.encode_utf8(&mut buff);
                    content.extend_from_slice(res.as_bytes());
                }
            }
            State::AfterBlackslash => {
                match ch {
                    'n' => {
                        content.push('\n' as u8);
                        state = State::Normal;
                    }
                    'r' => {
                        content.push('\r' as u8);
                        state = State::Normal;
                    }
                    't' => {
                        content.push('\t' as u8);
                        state = State::Normal;
                    }
                    '\\' => {
                        content.push('\\' as u8);
                        state = State::Normal;
                    }
                    '0' => {
                        content.push(0);
                        state = State::Normal;
                    }
                    '"' => {
                        content.push('"' as u8);
                        state = State::Normal;
                    }
                    '\'' => {
                        content.push('\'' as u8);
                        state = State::Normal;
                    }
                    'x' => {
                        state = State::ReadBinary0;
                    }
                    _ => {
                        errors.push(StringLitError {
                            kind: StringLitErrKind::UnexpectedChar(ch),
                            offset,
                        });
                        state = State::Normal;
                    }
                };
            }
            State::ReadBinary0 => {
                if ch >= '0' && ch <= '9' {
                    state = State::ReadBinary1(ch as u8 - '0' as u8 + 0x0u8);
                } else if ch >= 'a' && ch <= 'f' {
                    state = State::ReadBinary1(ch as u8 - 'a' as u8 + 0xau8);
                } else if ch >= 'A' && ch <= 'F' {
                    state = State::ReadBinary1(ch as u8 - 'A' as u8 + 0xAu8);
                } else if ch == opening_quote {
                    errors.push(StringLitError {
                        kind: StringLitErrKind::UnexpectedChar(ch),
                        offset,
                    });
                    state = State::Closed;
                    break;
                } else {
                    errors.push(StringLitError {
                        kind: StringLitErrKind::UnexpectedChar(ch),
                        offset,
                    });
                    state = State::Normal;
                }
            }
            State::ReadBinary1(byte) => {
                if ch >= '0' && ch <= '9' {
                    let byte = byte << 16 + ch as u8 - '0' as u8 + 0x0u8;
                    content.push(byte);
                    state = State::Normal;
                } else if ch >= 'a' && ch <= 'f' {
                    let byt = byte << 16 + ch as u8 - 'a' as u8 + 0xau8;
                    content.push(byt);
                    state = State::Normal;
                } else if ch >= 'A' && ch <= 'F' {
                    let byt = byte << 16 + ch as u8 - 'A' as u8 + 0xAu8;
                    content.push(byt);
                    state = State::Normal;
                } else if ch == opening_quote {
                    errors.push(StringLitError {
                        kind: StringLitErrKind::UnexpectedChar(ch),
                        offset,
                    });
                    state = State::Closed;
                    break;
                } else {
                    errors.push(StringLitError {
                        kind: StringLitErrKind::UnexpectedChar(ch),
                        offset,
                    });
                    state = State::Normal;
                }
            }
            State::Closed => unreachable!(),
        }
    }

    if !matches!(state, State::Closed) {
        errors.push(StringLitError {
            kind: StringLitErrKind::MissingClosingQuote,
            offset: end_offset,
        });
    }

    Some(StringLitResult {
        value,
        content,
        offset: start_offset,
        len: end_offset - start_offset + 1,
        consumed: end_offset - start_offset + 1,
        errors,
    })
}

#[derive(Debug)]
pub(crate) struct StringLitResult {
    pub value: String,
    pub content: Vec<u8>,
    pub offset: usize,
    pub len: usize,
    pub consumed: usize,
    pub errors: Vec<StringLitError>,
}

#[derive(Debug)]
pub(crate) struct StringLitError {
    pub kind: StringLitErrKind,
    pub offset: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StringLitErrKind {
    FoundNewline,
    MissingClosingQuote,
    UnexpectedChar(char),
}

// parse_string_lit assumes that s is a valid string literal.
pub fn parse_string_lit(s: &str) -> Rc<[u8]> {
    let charpos: Vec<_> = s
        .chars()
        .enumerate()
        .map(|(offset, ch)| CharPos { offset, ch })
        .collect();
    scan_string_lit(charpos.iter())
        .expect("the parsed string literal should be a valid string literal")
        .content
        .into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::CharPos;

    macro_rules! test_parse_string_lit {
        ($name:ident, $s:expr, $e:expr, $content:expr, $consumed:expr $(,$errs:expr)*) => {
            #[test]
            fn $name() {
                let text = $s;
                let expected_value = $e;
                let expected_content = $content;
                let expected_errors = vec![$($errs),*];
                let expected_consumed = $consumed;

                let char_pos: Vec<_> = text
                    .chars()
                    .enumerate()
                    .map(|(offset, ch)| CharPos { ch, offset })
                    .collect();
                let result = scan_string_lit(char_pos.iter()).expect("should return a single string");
                assert_eq!(result.value.as_str(), expected_value);
                assert_eq!(result.content, expected_content);

                let errors: Vec<_> = result.errors.iter().map(|err| err.kind).collect();
                assert_eq!(errors, expected_errors);
                assert_eq!(result.consumed, expected_consumed);
            }
        };
    }

    test_parse_string_lit!(
        happy_path,
        "\"some string\"",
        "\"some string\"",
        "some string".as_bytes(),
        13
    );
    test_parse_string_lit!(
        tab_escape,
        "\"this char (\t) is a tab\"",
        "\"this char (\t) is a tab\"",
        "this char (\t) is a tab".as_bytes(),
        24
    );
    test_parse_string_lit!(
        carriage_return_escape,
        "\"this char (\r) is a CR\"",
        "\"this char (\r) is a CR\"",
        "this char (\r) is a CR".as_bytes(),
        23
    );
    test_parse_string_lit!(
        double_quote_escape,
        "\"There is a \\\" quote here\"",
        "\"There is a \\\" quote here\"",
        "There is a \" quote here".as_bytes(),
        26
    );
    test_parse_string_lit!(
        contain_newline,
        "\"some string \n with newline\"",
        "\"some string ",
        "some string ".as_bytes(),
        28,
        StringLitErrKind::FoundNewline
    );
}
