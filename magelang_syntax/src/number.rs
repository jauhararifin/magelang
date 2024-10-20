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

    pub fn from_int<T>(number: T) -> Self
    where
        T: Into<BigInt>,
    {
        Self {
            val: number.into(),
            exp: BigInt::zero(),
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

impl std::ops::Add for Number {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        todo!();
    }
}

impl std::ops::Sub for Number {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        todo!();
    }
}

impl std::ops::Mul for Number {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        todo!();
    }
}

impl std::ops::Div for Number {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        todo!();
    }
}

impl std::ops::Rem for Number {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self::Output {
        todo!();
    }
}

impl std::ops::BitOr for Number {
    type Output = Option<Self>;
    fn bitor(self, rhs: Self) -> Self::Output {
        todo!();
    }
}

impl std::ops::BitAnd for Number {
    type Output = Option<Self>;
    fn bitand(self, rhs: Self) -> Self::Output {
        todo!();
    }
}

impl std::ops::BitXor for Number {
    type Output = Option<Self>;
    fn bitxor(self, rhs: Self) -> Self::Output {
        todo!();
    }
}

impl std::ops::Shl for Number {
    type Output = Option<Self>;
    fn shl(self, rhs: Self) -> Self::Output {
        todo!();
    }
}

impl std::ops::Shr for Number {
    type Output = Option<Self>;
    fn shr(self, rhs: Self) -> Self::Output {
        todo!();
    }
}

impl std::ops::Not for Number {
    type Output = Option<Self>;
    fn not(self) -> Self::Output {
        todo!();
    }
}

impl std::ops::Neg for Number {
    type Output = Self;
    fn neg(self) -> Self::Output {
        todo!();
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        todo!();
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        todo!();
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
number_try_into!(BigInt);

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
    use core::str::FromStr;

    macro_rules! test {
        ($name:ident, $base:expr, $exp:expr, $expected:expr, BigInt) => {
            #[test]
            fn $name() {
                let n = Number::new($base, $exp);
                let expected = BigInt::from_str($expected).unwrap();
                assert_eq!(expected, BigInt::try_from(&n).unwrap());
            }
        };
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
        test_1234567890123455561090e0,
        BigInt::from_str("1234567890123455561090").unwrap(),
        BigInt::from(0),
        "1234567890123455561090",
        BigInt
    );
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
