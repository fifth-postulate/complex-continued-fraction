use crate::rational::Rational;
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Complex {
    real: Rational,
    imaginary: Rational,
}

impl Complex {
    pub fn create<T: Into<Rational>>(real: T, imaginary: T) -> Self {
        Complex {
            real: real.into(),
            imaginary: imaginary.into(),
        }
    }

    pub fn is_zero(self) -> bool {
        self.real.is_zero() && self.imaginary.is_zero()
    }

    pub fn conjugate(self) -> Self {
        Self::create(self.real, -self.imaginary)
    }

    pub fn norm(self) -> Rational {
        self.real * self.real + self.imaginary * self.imaginary
    }

    pub fn invert(self) -> Option<Self> {
        self.norm().invert().map(|n| self.conjugate() * n)
    }
}

impl Mul<Rational> for Complex {
    type Output = Self;

    fn mul(self, rhs: Rational) -> Self::Output {
        Self::create(self.real * rhs, self.imaginary * rhs)
    }
}

impl Add<Self> for Complex {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::create(self.real + rhs.real, self.imaginary + rhs.imaginary)
    }
}

impl Mul<Self> for Complex {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::create(
            self.real * rhs.real - self.imaginary * rhs.imaginary,
            self.real * rhs.imaginary + self.imaginary * rhs.real,
        )
    }
}

impl Neg for Complex {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::create(-self.real, -self.imaginary)
    }
}

impl Sub<Self> for Complex {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + (-rhs)
    }
}

impl Div<Self> for Complex {
    type Output = Option<Self>;

    fn div(self, rhs: Self) -> Self::Output {
        rhs.invert().map(|z| self * z)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rational::test_util::arbitrary_rational;
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
             imaginary in arbitrary_rational(max, max)) -> Complex {
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
