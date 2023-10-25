use num::{bigint::TryFromBigIntError, traits::Signed, BigInt, Zero};
use std::num::ParseFloatError;

#[derive(Debug, Default)]
pub struct Number {
    pub val: BigInt,
    pub exp: BigInt,
}

impl Number {
    pub fn from_str(s: &str) -> Result<Self, Vec<NumberError>> {
        let mut builder = NumberBuilder::default();
        for c in s.chars() {
            if !builder.add(c) {
                break;
            }
        }
        builder.build_number()
    }

    pub fn is_int(&self) -> bool {
        let mut exp = self.exp.clone();
        let mut x = self.val.clone();
        while x.is_positive() && (&x % 10u8).is_zero() {
            x /= 10;
            exp += 1;
        }
        exp.is_positive() || exp.is_zero()
    }

    fn is_too_big(&self) -> bool {
        self.exp.ge(&BigInt::from(20))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TryFromNumberError {
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

                let exp: u32 = self.exp.try_into().expect("cannot parse exponent");

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

macro_rules! number_try_into_float {
    ($target: ty) => {
        impl TryInto<$target> for Number {
            type Error = ParseFloatError;
            fn try_into(self) -> Result<$target, Self::Error> {
                let s = format!("{}e{}", self.val, self.exp);
                s.parse::<$target>()
            }
        }
    };
}

number_try_into_float!(f32);
number_try_into_float!(f64);

pub(crate) struct Literal {
    pub(crate) value: String,
    pub(crate) invalid_suffix: String,
    pub(crate) errors: Vec<NumberError>,
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum Base {
    Bin = 2,
    #[default]
    Dec = 10,
    Oct = 8,
    Hex = 16,
}

#[derive(Default, Clone, Copy)]
enum State {
    #[default]
    Init,
    Prefix,
    Integer(Base),
    Fraction,
    Exponent,
    ExponentAfterSign,
    InvalidSuffix,
}

#[derive(Default)]
pub struct NumberBuilder {
    state: State,

    offset: usize,
    pub text: String,
    invalid_suffix: String,
    errors: Vec<NumberError>,

    value: BigInt,
    exp: BigInt,
    frac_digit: u32,
    exp_neg: bool,
}

#[derive(Debug)]
pub enum NumberError {
    InvalidSuffix {
        offset: usize,
    },
    NonDecimalFraction {
        offset: usize,
    },
    InvalidDigit {
        offset: usize,
        base: Base,
        digit: char,
    },
    MissingExponent {
        offset: usize,
    },
}

impl NumberBuilder {
    pub fn add(&mut self, c: char) -> bool {
        self.offset += 1;
        if c == '_' {
            self.text.push(c);
            if matches!(self.state, State::InvalidSuffix) {
                self.invalid_suffix.push(c);
            }
            return true;
        }

        match &self.state {
            State::Init => match c {
                '0' => {
                    self.state = State::Prefix;
                }
                '1'..='9' => {
                    self.state = State::Integer(Base::Dec);
                    self.value = &self.value * 10 + into_digit(c);
                }
                _ => return false,
            },
            State::Prefix => match c {
                'x' => self.state = State::Integer(Base::Hex),
                'b' => self.state = State::Integer(Base::Bin),
                'o' => self.state = State::Integer(Base::Oct),
                '0'..='7' => {
                    self.state = State::Integer(Base::Oct);
                    self.value = &self.value * 8 + into_digit(c);
                }
                'e' | 'E' => self.state = State::Exponent,
                '.' => {
                    self.state = State::Fraction;
                }
                'a'..='z' | 'A'..='Z' => {
                    self.state = State::InvalidSuffix;
                    self.errors.push(NumberError::InvalidSuffix {
                        offset: self.offset,
                    });
                    self.invalid_suffix.push(c);
                }
                _ => return false,
            },
            State::Integer(base) => match (base, c) {
                (Base::Dec, 'e' | 'E') => self.state = State::Exponent,
                (Base::Dec, '.') => {
                    self.state = State::Fraction;
                }
                (_, '.') => return false,
                (Base::Bin, '0' | '1') => {
                    self.value = &self.value * 2 + into_digit(c);
                }
                (Base::Dec, '0'..='9') => {
                    self.value = &self.value * 10 + into_digit(c);
                }
                (Base::Oct, '0'..='7') => {
                    self.value = &self.value * 8 + into_digit(c);
                }
                (Base::Hex, '0'..='9' | 'a'..='f' | 'A'..='F') => {
                    self.value = &self.value * 16 + into_digit(c);
                }
                (Base::Bin, '2'..='9') | (Base::Oct, '8'..='9') => {
                    self.errors.push(NumberError::InvalidDigit {
                        offset: self.offset,
                        base: *base,
                        digit: c,
                    });
                }
                (Base::Bin | Base::Dec | Base::Oct, 'a'..='z' | 'A'..='Z')
                | (Base::Hex, 'g'..='z' | 'G'..='Z') => {
                    self.errors.push(NumberError::InvalidSuffix {
                        offset: self.offset,
                    });
                    self.state = State::InvalidSuffix;
                    self.invalid_suffix.push(c);
                }
                _ => return false,
            },
            State::Fraction => match c {
                'e' | 'E' => {
                    self.state = State::Exponent;
                }
                '0'..='9' => {
                    self.value = &self.value * 10 + into_digit(c);
                    self.frac_digit += 1;
                }
                'a'..='z' | 'A'..='Z' => {
                    self.errors.push(NumberError::InvalidSuffix {
                        offset: self.offset,
                    });
                    self.state = State::InvalidSuffix;
                    self.invalid_suffix.push(c);
                }
                _ => return false,
            },
            State::Exponent => match c {
                '-' => {
                    self.state = State::ExponentAfterSign;
                    self.exp_neg = true;
                }
                '0'..='9' => {
                    self.state = State::ExponentAfterSign;
                    self.exp = &self.exp * 10 + into_digit(c);
                }
                'a'..='z' | 'A'..='Z' => {
                    self.errors.push(NumberError::InvalidSuffix {
                        offset: self.offset,
                    });
                    self.state = State::InvalidSuffix;
                    self.invalid_suffix.push(c);
                }
                _ => {
                    self.errors.push(NumberError::MissingExponent {
                        offset: self.offset,
                    });
                    return false;
                }
            },
            State::ExponentAfterSign => match c {
                '0'..='9' => {
                    self.exp = &self.exp * 10 + into_digit(c);
                }
                'a'..='z' | 'A'..='Z' => {
                    self.errors.push(NumberError::InvalidSuffix {
                        offset: self.offset,
                    });
                    self.state = State::InvalidSuffix;
                    self.invalid_suffix.push(c);
                }
                _ => return false,
            },
            State::InvalidSuffix => match c {
                '0'..='9' | 'a'..='z' | 'A'..='Z' => self.invalid_suffix.push(c),
                _ => return false,
            },
        };

        self.text.push(c);

        true
    }

    pub fn build_number(self) -> Result<Number, Vec<NumberError>> {
        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        let mut exp = self.exp;
        if self.exp_neg {
            exp *= -1;
        }
        exp -= self.frac_digit;

        Ok(Number {
            val: self.value,
            exp,
        })
    }

    pub(crate) fn build_token(self) -> Option<Literal> {
        if self.text.is_empty() {
            return None;
        }
        Some(Literal {
            value: self.text,
            invalid_suffix: self.invalid_suffix,
            errors: self.errors,
        })
    }
}

fn into_digit(c: char) -> u8 {
    match c {
        '0'..='9' => (c as u8) - ('0' as u8),
        'a'..='f' => (c as u8) - ('a' as u8) + 10,
        'A'..='F' => (c as u8) - ('A' as u8) + 10,
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::Number;

    #[test]
    fn parse_number() {
        macro_rules! test_parse_int {
            ($s:expr, $expected:expr) => {
                let number = Number::from_str($s).expect("cannot parse number");
                assert_eq!($expected, number.try_into().unwrap(),)
            };
        }

        test_parse_int!("0x1", 1u8);
        test_parse_int!("0x001", 1u8);
        test_parse_int!("0x0__01", 1u8);
        test_parse_int!("0xff", 0xffu8);
        test_parse_int!("0x_f_f_", 0xffu8);
        test_parse_int!("0b1101011", 0b1101011u8);
        test_parse_int!("0", 0u8);
        test_parse_int!("0x0", 0u8);
        test_parse_int!("0o0", 0u8);
        test_parse_int!("0b0", 0u8);
        test_parse_int!("100", 100u8);
        test_parse_int!("1e9", 1000000000u32);
        test_parse_int!("1.23e2", 123u32);
        test_parse_int!("1.23e3", 1230u32);
    }
}
