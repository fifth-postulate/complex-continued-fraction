use crate::{traits::{Invert, IsZero, Ceiling, Integral}};
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Complex<T> {
    real: T,
    imaginary: T,
}

impl<T> Complex<T> {
    pub fn create(real: T, imaginary: T) -> Self {
        Complex { real, imaginary }
    }
}

impl<T : IsZero> Complex<T> {
    pub fn is_zero(self) -> bool {
        self.real.is_zero() && self.imaginary.is_zero()
    }
}


impl<T: Ceiling<Output = T>> Integral for Complex<T> {
    fn integral(&self) -> Self {
        Self::create(self.real.ceil(), self.imaginary.ceil())
    }
}

impl<T: Invert<Output=T> + Mul<T, Output=T> + Add<T, Output=T> + Neg<Output = T> + Copy> Invert for Complex<T> {
    type Output = Self;

    fn invert(&self) -> Option<Self::Output> {
        self.norm().invert().map(|n| self.conjugate() * n)
    }
}

impl<T: Neg<Output = T>> Complex<T> {
    pub fn conjugate(self) -> Self {
        Self::create(self.real, -self.imaginary)
    }
}

impl<T: Add<T, Output = T> + Mul<T, Output = T> + Copy> Complex<T> {
    fn norm(self) -> T {
        self.real * self.real + self.imaginary * self.imaginary
    }
}

impl<T: Mul<T, Output = T> + Copy> Mul<T> for Complex<T> {
    type Output = Self;

    fn mul(self, rhs: T) -> Self::Output {
        Self::create(self.real * rhs, self.imaginary * rhs)
    }
}

impl<T: Add<T, Output = T>> Add<Self> for Complex<T> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::create(self.real + rhs.real, self.imaginary + rhs.imaginary)
    }
}

impl<T: Add<T, Output=T> + Mul<T, Output=T> + Sub<T, Output=T> + Copy> Mul<Self> for Complex<T> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::create(
            self.real * rhs.real - self.imaginary * rhs.imaginary,
            self.real * rhs.imaginary + self.imaginary * rhs.real,
        )
    }
}

impl<T: Neg<Output = T>> Neg for Complex<T> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::create(-self.real, -self.imaginary)
    }
}

impl<T: Neg<Output= T> + Add<T, Output = T>> Sub<Self> for Complex<T> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + (-rhs)
    }
}

impl<T: Add<T, Output=T> + Mul<T, Output=T> + Sub<T, Output=T> + Invert<Output=T> + Neg<Output = T> + Copy > Div<Self> for Complex<T> {
    type Output = Option<Self>;

    fn div(self, rhs: Self) -> Self::Output {
        rhs.invert().map(|z| self * z)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rational::{Rational, test_util::arbitrary_rational};
    use proptest::prelude::*;

    const MAX: i128 = 1000;

    #[test]
    fn multiplication_should_work_correctly() {
        let left = Complex::create(2, 3);
        let right = Complex::create(2, 1);

        let actual = left * right;

        let expected = Complex::create(1, 8);
        assert_eq!(actual, expected);
    }

    #[test]
    fn addition_should_work_correctly() {
        let left = Complex::create(2, 3);
        let right = Complex::create(5, 7);

        let actual = left + right;

        let expected = Complex::create(7, 10);
        assert_eq!(actual, expected);
    }

    prop_compose! {
        fn arbitrary_complex(max: i128)
            (real in arbitrary_rational(max, max),
             imaginary in arbitrary_rational(max, max)) -> Complex<Rational> {
             Complex::create(real, imaginary)
         }
    }

    proptest! {
        #[test]
        fn addition_is_commutative(
            left in arbitrary_complex(MAX),
            right in arbitrary_complex(MAX),
        ) {
            let lr = left + right;
            let rl = right + left;

            assert_eq!(lr, rl)
        }

        #[test]
        fn addition_is_associative(
            left in arbitrary_complex(MAX),
            middle in arbitrary_complex(MAX),
            right in arbitrary_complex(MAX),
        ) {
            let l_mr = left + (middle + right);
            let lm_r = (left + middle) + right;

            assert_eq!(l_mr, lm_r)
        }

        #[test]
        fn multiplication_is_commutative(
            left in arbitrary_complex(MAX),
            right in arbitrary_complex(MAX),
        ) {
            let lr = left * right;
            let rl = right * left;

            assert_eq!(lr, rl)
        }

        #[test]
        fn multiplication_is_associative(
            left in arbitrary_complex(MAX),
            middle in arbitrary_complex(MAX),
            right in arbitrary_complex(MAX),
        ) {
            let l_mr = left * (middle * right);
            let lm_r = (left * middle) * right;

            assert_eq!(l_mr, lm_r)
        }

        #[test]
        fn negate_twice_is_identity(
            r in arbitrary_complex(MAX),
        ) {
            let actual = -(-r);

            assert_eq!(actual, r)
        }
    }
}
