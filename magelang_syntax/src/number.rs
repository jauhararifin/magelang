use num::{bigint::TryFromBigIntError, traits::Signed, BigInt, Zero};
use std::num::ParseFloatError;

#[derive(Debug, PartialEq, Eq, Clone, Default, Hash)]
pub struct Number {
    pub val: BigInt,
    pub exp: BigInt,
}

impl Number {
    pub fn new<T>(base: T, exp: T) -> Self
    where
        T: Into<BigInt>,
    {
        Self {
            val: base.into(),
            exp: exp.into(),
        }
    }

    pub fn is_int(&self) -> bool {
        let mut exp = self.exp.clone();
        let mut base = self.val.clone();
        while (&base % 10u8).is_zero() {
            base /= 10;
            exp += 1;
        }

        !exp.is_negative()
    }

    pub fn to_string(&self) -> String {
        format!("{}e{}", self.val, self.exp)
    }

    fn to_int(&self) -> Result<BigInt, TryFromNumberError> {
        let mut exp = self.exp.clone();
        let mut base = self.val.clone();
        while !base.is_zero() && (&base % 10u8).is_zero() {
            base /= 10;
            exp += 1;
        }

        if base.is_zero() {
            return Ok(BigInt::zero());
        }

        if exp.is_negative() {
            return Err(TryFromNumberError::NotInt);
        }

        let Ok(exp) = u32::try_from(exp) else {
            return Err(TryFromNumberError::OutOfRange);
        };
        if exp > 20 {
            return Err(TryFromNumberError::OutOfRange);
        }

        Ok(base * BigInt::from(10).pow(exp))
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
        impl TryFrom<&Number> for $target {
            type Error = TryFromNumberError;
            fn try_from(value: &Number) -> Result<Self, Self::Error> {
                let val = value.to_int()?;
                <$target>::try_from(val).map_err(|_| TryFromNumberError::OutOfRange)
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
        impl TryFrom<&Number> for $target {
            type Error = ParseFloatError;
            fn try_from(value: &Number) -> Result<Self, Self::Error> {
                let s = format!("{}e{}", value.val, value.exp);
                s.parse::<$target>()
            }
        }
    };
}

number_try_into_float!(f32);
number_try_into_float!(f64);

pub(crate) struct NumberResult {
    pub(crate) raw: String,
    pub(crate) value: Number,
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

    pub(crate) fn build(self) -> Option<NumberResult> {
        if self.text.is_empty() {
            return None;
        }

        let mut exp = self.exp;
        if self.exp_neg {
            exp *= -1;
        }
        exp -= self.frac_digit;

        Some(NumberResult {
            raw: self.text,
            value: Number {
                val: self.value,
                exp,
            },
            invalid_suffix: self.invalid_suffix,
            errors: self.errors,
        })
    }
}

fn into_digit(c: char) -> u8 {
    match c {
        '0'..='9' => (c as u8) - b'0',
        'a'..='f' => (c as u8) - b'a' + 10,
        'A'..='F' => (c as u8) - b'A' + 10,
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::{Number, NumberBuilder};

    #[test]
    fn parse_number() {
        macro_rules! test_parse_int {
            ($s:expr, $expected:expr) => {
                let number = number_from_str($s);
                assert_eq!($expected, (&number).try_into().unwrap())
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
        test_parse_int!("0xfffc", 0xfffcu16);
        test_parse_int!("0xfffd", 0xfffdu16);
        test_parse_int!("0xfffe", 0xfffeu16);
        test_parse_int!("0xffff", 0xffffu16);
    }

    fn number_from_str(s: &str) -> Number {
        let mut builder = NumberBuilder::default();
        for c in s.chars() {
            if !builder.add(c) {
                break;
            }
        }

        builder.build().unwrap().value
    }
}
