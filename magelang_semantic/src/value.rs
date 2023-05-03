use std::rc::Rc;

// The input string literal is assumed to be a valid string literal.
pub fn value_from_string_lit(literal: &str) -> Rc<[u8]> {
    let mut result = vec![];
    let mut literal = literal[1..literal.len() - 1].bytes();
    while let Some(c) = literal.next() {
        if c == b'\\' {
            let c = literal
                .next()
                .expect("the input str literal is not a valid string literal");
            let escaped_char = match c as char {
                'n' => b'\n',
                'r' => b'\r',
                't' => b'\t',
                '0' => 0u8,
                '"' => b'"',
                '\'' => b'\'',
                'x' => {
                    let into_u8 = |c: u8| match c as char {
                        '0'..='9' => c - b'0',
                        'a'..='f' => c - b'a',
                        'A'..='F' => c - b'A',
                        _ => unreachable!("the input str literal is not a valid string literal"),
                    };
                    let b0 = into_u8(
                        literal
                            .next()
                            .expect("the input str literal is not a valid string literal"),
                    );
                    let b1 = into_u8(
                        literal
                            .next()
                            .expect("the input str literal is not a valid string literal"),
                    );
                    b0 << 4 | b1
                }
                _ => unreachable!("the input str literal is not a valid string literal"),
            };
            result.push(escaped_char);
        } else {
            result.push(c);
        }
    }

    result.into()
}
