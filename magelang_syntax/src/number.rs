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
