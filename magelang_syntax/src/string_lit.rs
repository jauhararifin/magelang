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
                        content.push(b'\n');
                        state = State::Normal;
                    }
                    'r' => {
                        content.push(b'\r');
                        state = State::Normal;
                    }
                    't' => {
                        content.push(b'\t');
                        state = State::Normal;
                    }
                    '\\' => {
                        content.push(b'\\');
                        state = State::Normal;
                    }
                    '0' => {
                        content.push(0);
                        state = State::Normal;
                    }
                    '"' => {
                        content.push(b'"');
                        state = State::Normal;
                    }
                    '\'' => {
                        content.push(b'\'');
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
                if ('0'..='9').contains(&ch) {
                    state = State::ReadBinary1(ch as u8 - b'0');
                } else if ('a'..='f').contains(&ch) {
                    state = State::ReadBinary1(ch as u8 - b'a' + 0xau8);
                } else if ('A'..='F').contains(&ch) {
                    state = State::ReadBinary1(ch as u8 - b'A' + 0xAu8);
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
                if ('0'..='9').contains(&ch) {
                    let byte = (byte << 4) | (ch as u8 - b'0');
                    content.push(byte);
                    state = State::Normal;
                } else if ('a' ..='f').contains(&ch) {
                    let byte = (byte << 4) | (ch as u8 - b'a' + 0xau8);
                    content.push(byte);
                    state = State::Normal;
                } else if ('A' ..='F').contains(&ch) {
                    let byte = (byte << 4) | (ch as u8 - b'A' + 0xAu8);
                    content.push(byte);
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

    if !errors.is_empty() {
        content = vec![];
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
    let result = scan_string_lit(charpos.iter()).expect("the parsed string literal should be a valid string literal");
    if !result.errors.is_empty() {
        panic!(
            "the parsed string is not a valid string due to these errors {:?}",
            result.errors
        );
    }
    result.content.into()
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
                assert_eq!(result.value.as_str(), expected_value, "value mismatched");
                assert_eq!(result.content, expected_content, "content mismatched");

                let errors: Vec<_> = result.errors.iter().map(|err| err.kind).collect();
                assert_eq!(errors, expected_errors, "error mismatched");
                assert_eq!(result.consumed, expected_consumed, "the number of consumed chars don't matched");
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
        missing_closing_quote,
        "\"some string",
        "\"some string",
        [],
        12,
        StringLitErrKind::MissingClosingQuote
    );
    test_parse_string_lit!(
        multi_errors,
        "\"some \\xyz string",
        "\"some \\xyz string",
        [],
        17,
        StringLitErrKind::UnexpectedChar('y'),
        StringLitErrKind::MissingClosingQuote
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
    test_parse_string_lit!(
        escaped_raw_byte,
        "\"this is a \\x00\\x01\\x02\\xfF raw bytes\"",
        "\"this is a \\x00\\x01\\x02\\xfF raw bytes\"",
        {
            let mut p = "this is a ".as_bytes().to_vec();
            p.extend_from_slice(&[0u8, 1u8, 2u8, 0xffu8]);
            p.extend_from_slice(" raw bytes".as_bytes());
            p
        },
        38
    );
    test_parse_string_lit!(
        escaped_raw_byte_with_err,
        "\"raw byte \\x*f\"",
        "\"raw byte \\x*f\"",
        [],
        15,
        StringLitErrKind::UnexpectedChar('*')
    );
    test_parse_string_lit!(
        escaped_raw_byte_with_err2,
        "\"raw byte \\x\"",
        "\"raw byte \\x\"",
        [],
        13,
        StringLitErrKind::UnexpectedChar('"')
    );
    test_parse_string_lit!(
        escaped_raw_byte_with_err3,
        "\"raw byte \\xa\"",
        "\"raw byte \\xa\"",
        [],
        14,
        StringLitErrKind::UnexpectedChar('"')
    );
    test_parse_string_lit!(
        escaped_raw_byte_with_err4,
        "\"raw byte \\xah\"",
        "\"raw byte \\xah\"",
        [],
        15,
        StringLitErrKind::UnexpectedChar('h')
    );
}
