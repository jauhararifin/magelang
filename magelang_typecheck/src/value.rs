use num::{bigint::TryFromBigIntError, BigInt, Zero};

// The input string literal is assumed to be a valid string literal.
pub(crate) fn value_from_string_lit(literal: &str) -> Option<Vec<u8>> {
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
                        'a'..='f' => Some(c - b'a' + 0xa),
                        'A'..='F' => Some(c - b'A' + 0xa),
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

    Some(result)
}

pub(crate) fn value_from_num_lit(literal: &str) -> Option<Number> {
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
    }

    let mut state = State::Init;
    let mut num = NumberBuilder::default();

    for c in literal.chars() {
        if c == '_' {
            continue;
        }

        match state {
            State::Init => match c {
                '0' => state = State::Prefix,
                '1'..='9' => {
                    num.add_dec(c);
                    state = State::Integer(Base::Dec);
                }
                _ => return None,
            },
            State::Prefix => match c {
                'x' => state = State::Integer(Base::Hex),
                'b' => state = State::Integer(Base::Bin),
                'o' => state = State::Integer(Base::Oct),
                '0'..='7' => {
                    num.add_oct(c);
                    state = State::Integer(Base::Oct);
                }
                'e' | 'E' => {
                    num.set_exp();
                    state = State::Exponent;
                }
                '.' => {
                    num.set_frac();
                    state = State::Fraction;
                }
                _ => return None,
            },
            State::Integer(base) => match (base, c) {
                (Base::Dec, 'e' | 'E') => {
                    num.set_exp();
                    state = State::Exponent;
                }
                (Base::Dec, '.') => {
                    num.set_frac();
                    state = State::Fraction;
                }
                (_, '.') => return None,
                (Base::Bin, '0' | '1') => num.add_bin(c),
                (Base::Dec, '0'..='9') => num.add_dec(c),
                (Base::Oct, '0'..='7') => num.add_oct(c),
                (Base::Hex, '0'..='9' | 'a'..='f' | 'A'..='F') => num.add_hex(c),
                _ => return None,
            },
            State::Fraction => match c {
                'e' | 'E' => {
                    num.set_exp();
                    state = State::Exponent;
                }
                '0'..='9' => num.add_dec(c),
                _ => return None,
            },
            State::Exponent => match c {
                '-' => {
                    num.set_neg();
                    state = State::ExponentAfterSign;
                }
                '0'..='9' => {
                    num.add_dec(c);
                    state = State::ExponentAfterSign;
                }
                _ => return None,
            },
            State::ExponentAfterSign => match c {
                '0'..='9' => num.add_dec(c),
                _ => return None,
            },
        }
    }

    Some(num.into())
}

#[derive(Default)]
pub(crate) struct NumberBuilder {
    val: BigInt,
    exp: BigInt,
    frac: u64,

    is_frac: bool,
    is_exp: bool,
}

impl NumberBuilder {
    fn set_neg(&mut self) {
        if self.is_exp {
            self.exp *= -1;
        } else {
            self.val *= -1;
        }
    }

    fn add_bin(&mut self, c: char) {
        let c = (c as u128) - ('0' as u128);
        if self.is_exp {
            self.exp = (&self.exp << 1) + c;
        } else {
            if self.is_frac {
                self.frac += 1;
            }
            self.val = (&self.val << 1) + c;
        }
    }

    fn add_dec(&mut self, c: char) {
        let c = (c as u128) - ('0' as u128);
        if self.is_exp {
            self.exp = (&self.exp * 10) + c;
        } else {
            if self.is_frac {
                self.frac += 1;
            }
            self.val = (&self.val * 10) + c;
        }
    }

    fn add_oct(&mut self, c: char) {
        let c = (c as u128) - ('0' as u128);
        if self.is_exp {
            self.exp = (&self.exp << 3) + c;
        } else {
            if self.is_frac {
                self.frac += 1;
            }
            self.val = (&self.val << 3) + c;
        }
    }

    fn add_hex(&mut self, c: char) {
        let c = match c {
            '0'..='9' => (c as u32) - ('0' as u32),
            'a'..='f' => (c as u32) - ('a' as u32),
            'A'..='F' => (c as u32) - ('A' as u32),
            _ => unreachable!(),
        };
        if self.is_exp {
            self.exp = (&self.exp << 4) + c;
        } else {
            if self.is_frac {
                self.frac += 1;
            }
            self.val = (&self.val << 4) + c;
        }
    }

    fn set_frac(&mut self) {
        self.is_frac = true;
    }

    fn set_exp(&mut self) {
        self.is_exp = true;
    }
}

// Number is represented as val * 10 ^ exp
#[derive(Debug)]
pub(crate) struct Number {
    val: BigInt,
    exp: BigInt,
}

impl From<NumberBuilder> for Number {
    fn from(mut value: NumberBuilder) -> Self {
        // remove trailing zero after comma
        while value.frac > 0 && (&value.val % 10u64).is_zero() {
            value.val /= 10;
            value.frac -= 1;
        }

        let mut x = value.val.clone();
        while !x.is_zero() {
            value.exp += 1;
            x /= 10;
        }

        Self {
            val: value.val,
            exp: value.exp,
        }
    }
}

impl Number {
    fn is_int(&self) -> bool {
        let mut x = self.val.clone();
        let mut digit = BigInt::zero();
        while !x.is_zero() {
            digit += 1;
            x /= 10;
        }
        self.exp.ge(&digit)
    }

    fn is_too_big(&self) -> bool {
        self.exp.ge(&BigInt::from(20))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum TryFromNumberError {
    NotInt,
    OutOfRange,
}

impl<T> From<TryFromBigIntError<T>> for TryFromNumberError {
    fn from(_: TryFromBigIntError<T>) -> Self {
        Self::OutOfRange
    }
}

macro_rules! number_try_into {
    ($target: ty) => {
        impl TryInto<$target> for Number {
            type Error = TryFromNumberError;
            fn try_into(self) -> Result<$target, Self::Error> {
                if !self.is_int() {
                    return Err(TryFromNumberError::NotInt);
                }
                if self.is_too_big() {
                    return Err(TryFromNumberError::OutOfRange);
                }

                let mut exp: u32 = self.exp.try_into()?;
                let mut x = self.val.clone();
                while !x.is_zero() {
                    x /= 10;
                    exp -= 1;
                }

                let value = self.val * BigInt::from(10).pow(exp);
                let value: $target = value.try_into()?;
                Ok(value)
            }
        }
    };
}

number_try_into!(i8);
number_try_into!(i16);
number_try_into!(i32);
number_try_into!(i64);
number_try_into!(u8);
number_try_into!(u16);
number_try_into!(u32);
number_try_into!(u64);

#[cfg(test)]
mod tests {
    use super::value_from_num_lit;

    #[test]
    fn parse_int() {
        let val = value_from_num_lit("0x1").expect("cannot parse number");
        assert_eq!(Ok(1u8), val.try_into());
    }
}
