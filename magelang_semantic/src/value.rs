use std::rc::Rc;

// The input string literal is assumed to be a valid string literal.
pub fn value_from_string_lit(literal: &str) -> Option<Rc<[u8]>> {
    let mut result = vec![];
    let mut literal = literal.bytes();

    let quote = literal.next()?;
    if quote as char != '"' {
        return None;
    }

    while let Some(c) = literal.next() {
        if c == b'\\' {
            let c = literal.next()?;
            let escaped_char = match c as char {
                'n' => b'\n',
                'r' => b'\r',
                't' => b'\t',
                '0' => 0u8,
                '"' => b'"',
                '\'' => b'\'',
                'x' => {
                    let into_u8 = |c: u8| match c as char {
                        '0'..='9' => Some(c - b'0'),
                        'a'..='f' => Some(c - b'a'),
                        'A'..='F' => Some(c - b'A'),
                        _ => None,
                    };
                    let b0 = literal.next().and_then(into_u8)?;
                    let b1 = literal.next().and_then(into_u8)?;
                    b0 << 4 | b1
                }
                _ => return None,
            };
            result.push(escaped_char);
        } else if c == b'"' {
            break;
        } else {
            result.push(c);
        }
    }

    let done = literal.next().is_none();
    if !done {
        return None;
    }

    Some(result.into())
}
