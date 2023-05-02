use crate::scanner::CharPos;

#[derive(Debug, Clone, Copy)]
enum State {
    Init,
    Prefix,
    Integer(Base),
    Fraction,
    Exponent,
    ExponentAfterSign,
    InvalidSuffix,
}

#[derive(Debug, Clone, Copy)]
pub enum Base {
    Bin,
    Dec,
    Oct,
    Hex,
}

pub(crate) fn scan_number_lit<'a>(source: impl Iterator<Item = &'a CharPos>) -> Option<NumberLitResult> {
    let mut source = source.peekable();

    let mut num_value = NumberValue::default();
    let mut value = String::default();
    let offset = source.peek().map(|c| c.offset).unwrap_or_default();

    let mut state = State::Init;
    let mut errors = vec![];
    let mut is_fractional = false;

    loop {
        while let Some(c) = source.next_if(|c| c.ch == '_') {
            value.push(c.ch);
        }

        let Some(c) = source.peek() else {
            break;
        };
        let c = *c;

        match state {
            State::Init => match c.ch {
                '0' => state = State::Prefix,
                '1'..='9' => state = State::Integer(Base::Dec),
                _ => return None,
            },
            State::Prefix => match c.ch {
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
            State::Integer(base) => match (base, c.ch) {
                (Base::Dec, 'e' | 'E') => state = State::Exponent,
                (Base::Dec, '.') => {
                    state = State::Fraction;
                    is_fractional = true;
                }
                (_, '.') => errors.push(NumberLitError {
                    kind: NumberLitErrorKind::NonDecimalFraction,
                    offset: c.offset,
                }),

                (Base::Bin, '0' | '1')
                | (Base::Dec, '0'..='9')
                | (Base::Oct, '0'..='7')
                | (Base::Hex, '0'..='9' | 'a'..='f' | 'A'..='F') => (),

                (Base::Bin, '2'..='9') => errors.push(NumberLitError {
                    kind: NumberLitErrorKind::InvalidDigit { digit: c.ch, base: 2 },
                    offset: c.offset,
                }),
                (Base::Oct, '8'..='9') => errors.push(NumberLitError {
                    kind: NumberLitErrorKind::InvalidDigit { digit: c.ch, base: 8 },
                    offset: c.offset,
                }),

                (Base::Bin | Base::Dec | Base::Oct, 'a'..='z' | 'A'..='Z') | (Base::Hex, 'g'..='z' | 'G'..='Z') => {
                    state = State::InvalidSuffix;
                    continue;
                }

                _ => break,
            },
            State::Fraction => match c.ch {
                'e' | 'E' => state = State::Exponent,
                '0'..='9' => (),
                'a'..='z' | 'A'..='Z' => {
                    state = State::InvalidSuffix;
                    continue;
                }
                _ => break,
            },
            State::Exponent => match c.ch {
                '-' => state = State::ExponentAfterSign,
                '0'..='9' => state = State::ExponentAfterSign,
                'a'..='z' | 'A'..='Z' => {
                    state = State::InvalidSuffix;
                    continue;
                }
                _ => break,
            },
            State::ExponentAfterSign => match c.ch {
                '0'..='9' => (),
                'a'..='z' | 'A'..='Z' => {
                    state = State::InvalidSuffix;
                    continue;
                }
                _ => break,
            },
            State::InvalidSuffix => match c.ch {
                '0'..='9' | 'a'..='z' | 'A'..='Z' => (),
                _ => break,
            },
        }

        let c = source.next().unwrap();
        value.push(c.ch);
    }

    let len = value.len();
    if len == 0 {
        return None;
    }

    Some(NumberLitResult {
        value,
        num_value,
        is_fractional,
        offset,
        len,
        consumed: len,
        errors,
    })
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct NumberLitResult {
    pub value: String,
    pub num_value: NumberValue,
    pub is_fractional: bool,
    pub offset: usize,
    pub len: usize,
    pub consumed: usize,
    pub errors: Vec<NumberLitError>,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub(crate) struct NumberValue {
    pub integer: usize,
    pub frac: usize,
    pub exp: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct NumberLitError {
    pub kind: NumberLitErrorKind,
    pub offset: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NumberLitErrorKind {
    InvalidDigit { digit: char, base: u8 },
    NonDecimalFraction,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::CharPos;

    macro_rules! test_parse_number_lit {
        ($name:ident, $s:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let text = $s;
                let expected_result = $expected;

                let char_pos: Vec<_> = text
                    .chars()
                    .enumerate()
                    .map(|(offset, ch)| CharPos { ch, offset })
                    .collect();
                let result = scan_number_lit(char_pos.iter()).expect("should return a single number result");
                assert_eq!(result, expected_result, "result mismatched");
            }
        };
    }

    test_parse_number_lit!(
        normal_decimal_value,
        "12345",
        NumberLitResult {
            value: "12345".into(),
            num_value: NumberValue::default(),
            is_fractional: false,
            offset: 0,
            len: 5,
            consumed: 5,
            errors: vec![],
        }
    );
    test_parse_number_lit!(
        octal_value,
        "0777",
        NumberLitResult {
            value: "0777".into(),
            num_value: NumberValue::default(),
            is_fractional: false,
            offset: 0,
            len: 4,
            consumed: 4,
            errors: vec![],
        }
    );
    test_parse_number_lit!(
        octal_value2,
        "0o777",
        NumberLitResult {
            value: "0o777".into(),
            num_value: NumberValue::default(),
            is_fractional: false,
            offset: 0,
            len: 5,
            consumed: 5,
            errors: vec![],
        }
    );
    test_parse_number_lit!(
        binaru_value,
        "0b11010101011",
        NumberLitResult {
            value: "0b11010101011".into(),
            num_value: NumberValue::default(),
            is_fractional: false,
            offset: 0,
            len: 13,
            consumed: 13,
            errors: vec![],
        }
    );
    test_parse_number_lit!(
        hex_value,
        "0xdeadbeef0123456789",
        NumberLitResult {
            value: "0xdeadbeef0123456789".into(),
            num_value: NumberValue::default(),
            is_fractional: false,
            offset: 0,
            len: 20,
            consumed: 20,
            errors: vec![],
        }
    );
}
