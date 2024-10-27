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
        while !base.is_zero() && (&base % 10u8).is_zero() {
            base /= 10;
            exp += 1;
        }

        !exp.is_negative()
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

                let mut digits = val.iter_u64_digits();
                if let Some(digit) = digits.next() {
                    let mut d = digit as i64;
                    if val.is_negative() {
                        d = d.wrapping_mul(-1);
                    }
                    Ok(d as $target)
                } else {
                    Ok(0 as $target)
                }
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

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test {
        ($name:ident, $base:expr, $exp:expr, $expected:expr, $type:ident) => {
            #[test]
            fn $name() {
                let n = Number::new($base, $exp);
                let expected: $type = $expected;
                assert_eq!(expected, $type::try_from(&n).unwrap());
            }
        };
    }

    test!(test_0e0, 0, 0, 0, u8);
    test!(test_1e0, 1, 0, 1, u8);
    test!(
        test_5744368105847e0,
        5744368105847i64,
        0,
        5744368105847i64,
        i64
    );

    test!(test_123_p_123, 123123, -3, 123.123f64, f64);
    test!(test_123e123, 123, 123, 123e123, f64);
    test!(test_123e_neg123, 123, -123, 123e-123, f64);
    test!(test_0e123, 0, 123, 0f64, f64);
    test!(test_123e_neg1, 123, -1, 123e-1f64, f64);
}
