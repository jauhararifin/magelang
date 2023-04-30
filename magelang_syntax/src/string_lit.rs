use crate::scanner::CharPos;
use std::rc::Rc;

pub(crate) fn scan_string_lit<'a>(source: impl Iterator<Item = &'a CharPos>) -> Option<StringLitResult> {
    let mut text_iter = source.peekable();

    let opening_tok = text_iter.next_if(|ch| ch.ch == '"')?;
    let start_offset = opening_tok.offset;
    let opening_quote = opening_tok.ch;
    let mut end_offset = start_offset;

    let mut value = String::from(opening_quote);
    let mut after_backslash = false;
    let mut is_closed = false;
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
                offset: start_offset,
                len: end_offset - start_offset + 1,
                consumed,
                errors,
            });
        }

        if after_backslash {
            match ch {
                'n' => value.push_str("\\n"),
                'r' => value.push_str("\\r"),
                't' => value.push_str("\\t"),
                '\\' => value.push_str("\\\\"),
                '0' => value.push_str("\\0"),
                '"' => value.push_str("\\\""),
                '\'' => value.push_str("\\\'"),
                '`' => value.push_str("\\`"),
                _ => errors.push(StringLitError {
                    kind: StringLitErrKind::UnexpectedChar(ch),
                    offset,
                }),
            };
            after_backslash = false;
        } else if ch == '\\' {
            after_backslash = true;
        } else if ch == opening_quote {
            value.push(ch);
            is_closed = true;
            break;
        } else {
            value.push(ch);
        }
    }

    if !is_closed {
        errors.push(StringLitError {
            kind: StringLitErrKind::MissingClosingQuote,
            offset: end_offset,
        });
    }

    Some(StringLitResult {
        value,
        offset: start_offset,
        len: end_offset - start_offset + 1,
        consumed: end_offset - start_offset + 1,
        errors,
    })
}

#[derive(Debug)]
pub(crate) struct StringLitResult {
    pub value: String,
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
pub fn parse_string_lit(s: &str) -> Rc<str> {
    let s = &s[1..s.len() - 1];

    let mut value = String::default();
    let mut after_backslash = false;
    for c in s.chars() {
        if after_backslash {
            match c {
                'n' => value.push('\n'),
                'r' => value.push('\r'),
                't' => value.push('\t'),
                '\\' => value.push('\\'),
                '0' => value.push('\0'),
                '"' => value.push('"'),
                '\'' => value.push('\''),
                '`' => value.push('`'),
                _ => panic!("the parsed string literal is not a valid string literal"),
            };
        } else if c == '\\' {
            after_backslash = true;
            continue;
        } else {
            value.push(c);
        }
    }

    value.into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::CharPos;

    macro_rules! test_parse_string_lit {
        ($name:ident, $s:expr, $e:expr, $consumed:expr $(,$errs:expr)*) => {
            #[test]
            fn $name() {
                let text = $s;
                let expected_value = $e;
                let expected_errors = vec![$($errs),*];
                let expected_consumed = $consumed;

                let char_pos: Vec<_> = text
                    .chars()
                    .enumerate()
                    .map(|(offset, ch)| CharPos { ch, offset })
                    .collect();
                let result = scan_string_lit(char_pos.iter()).expect("should return a single string");
                assert_eq!(result.value.as_str(), expected_value);

                let errors: Vec<_> = result.errors.iter().map(|err| err.kind).collect();
                assert_eq!(errors, expected_errors);
                assert_eq!(result.consumed, expected_consumed);
            }
        };
    }

    test_parse_string_lit!(happy_path, "\"some string\"", "\"some string\"", 13);
    test_parse_string_lit!(
        tab_escape,
        "\"this char (\t) is a tab\"",
        "\"this char (\t) is a tab\"",
        24
    );
    test_parse_string_lit!(
        carriage_return_escape,
        "\"this char (\r) is a CR\"",
        "\"this char (\r) is a CR\"",
        23
    );
    test_parse_string_lit!(
        double_quote_escape,
        "\"There is a \\\" quote here\"",
        "\"There is a \\\" quote here\"",
        26
    );
    test_parse_string_lit!(
        contain_newline,
        "\"some string \n with newline\"",
        "\"some string ",
        28,
        StringLitErrKind::FoundNewline
    );
}
