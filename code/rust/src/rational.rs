use crate::traits::{Ceiling, Integral, Invert, IsZero};
use std::convert::From;
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Rational {
    numerator: i128,
    denominator: i128,
}

impl Rational {
    pub fn create(numerator: i128, denominator: i128) -> Option<Self> {
        if denominator == 0 {
            Option::None
        } else {
            Option::Some(Self::safe_create(numerator, denominator))
        }
    }

    fn safe_create(numerator: i128, denominator: i128) -> Self {
        let (n, d) = (numerator.abs(), denominator.abs());
        let (gcd, _, _) = egcd(n, d);
        let s = sign(numerator) * sign(denominator);
        Rational {
            numerator: s * (n / gcd),
            denominator: d / gcd,
        }
    }
}

fn sign(n: i128) -> i128 {
    if n < 0 {
        -1
    } else if n > 0 {
        1
    } else {
        0
    }
}

fn egcd(a: i128, b: i128) -> (i128, i128, i128) {
    egcd_helper(a.abs(), b.abs(), sign(a), 0, 0, sign(b))
}

fn egcd_helper(a: i128, b: i128, s: i128, t: i128, u: i128, v: i128) -> (i128, i128, i128) {
    if b == 0 {
        (a, s, t)
    } else {
        let q = a / b;
        egcd_helper(b, a % b, u, v, s - q * u, t - q * v)
    }
}

impl IsZero for Rational {
    fn is_zero(&self) -> bool {
        self.numerator == 0
    }
}

impl Ceiling for Rational {
    type Output = Rational;

    fn ceil(&self) -> Self::Output {
        let n = self.numerator / self.denominator;
        Self::safe_create(n, 1)
    }
}

impl Integral for Rational {
    fn integral(&self) -> Self {
        let shifted = *self + Rational::safe_create(1, 2);
        shifted.ceil()
    }
}

impl Invert for Rational {
    type Output = Self;

    fn invert(&self) -> Option<Self::Output> {
        Self::create(self.denominator, self.numerator)
    }
}

impl From<i128> for Rational {
    fn from(source: i128) -> Self {
        Self::safe_create(source, 1)
    }
}

impl Mul<Self> for Rational {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::safe_create(
            self.numerator * rhs.numerator,
            self.denominator * rhs.denominator,
        )
    }
}

impl Add<Self> for Rational {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::safe_create(
            self.numerator * rhs.denominator + self.denominator * rhs.numerator,
            self.denominator * rhs.denominator,
        )
    }
}

impl Neg for Rational {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::safe_create(-self.numerator, self.denominator)
    }
}

impl Sub<Self> for Rational {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + (-rhs)
    }
}

impl Div<Self> for Rational {
    type Output = Option<Self>;

    fn div(self, rhs: Self) -> Self::Output {
        rhs.invert().map(|q| self * q)
    }
}

#[cfg(test)]
pub mod test_util {
    use super::*;
    use proptest::prelude::*;

    prop_compose! {
        pub fn arbitrary_rational(
                max_numerator: i128, max_denominator : i128)
            (n in -max_numerator .. max_numerator,
             d in 1 .. max_denominator) -> Rational {
             Rational::safe_create(n, d)
         }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::rational::test_util::arbitrary_rational;
    use proptest::prelude::*;

    const MAX: i128 = 100_000;

    #[test]
    fn multiplication_should_work_correctly() {
        let left: Rational = Rational::create(2, 3).expect("a correct rational");
        let right: Rational = Rational::create(5, 7).expect("a correct rational");

        let actual = left * right;

        let expected = Rational::create(10, 21).expect("a correct rational");
        assert_eq!(actual, expected);
    }

    #[test]
    fn addition_should_work_correctly() {
        let left: Rational = Rational::create(2, 3).expect("a correct rational");
        let right: Rational = Rational::create(5, 7).expect("a correct rational");

        let actual = left + right;

        let expected = Rational::create(29, 21).expect("a correct rational");
        assert_eq!(actual, expected);
    }

    #[test]
    fn ceiling_of_halve_should_work_correctly() {
        let halve = Rational::create(2, 3).expect("a correct rational");

        let actual = halve.ceil();

        assert_eq!(actual, 0.into());
    }

    #[test]
    fn ceiling_of_51_37_should_work_correctly() {
        let q = Rational::create(51, 37).expect("a correct rational");

        let actual = q.ceil();

        assert_eq!(actual, 1.into());
    }

    #[test]
    fn integral_of_a_third_is_zero() {
        let q = Rational::create(1, 3).expect("a correct rational");

        let actual = q.integral();

        assert_eq!(actual, 0.into())
    }

    #[test]
    fn integral_of_two_third_is_zero() {
        let q = Rational::create(2, 3).expect("a correct rational");

        let actual = q.integral();

        assert_eq!(actual, 1.into())
    }

    proptest! {
        #[test]
        fn addition_is_commutative(
            left in arbitrary_rational(MAX, MAX),
            right in arbitrary_rational(MAX, MAX),
        ) {
            let lr = left + right;
            let rl = right + left;

            assert_eq!(lr, rl)
        }

        #[test]
        fn addition_is_associative(
            left in arbitrary_rational(MAX, MAX),
            middle in arbitrary_rational(MAX, MAX),
            right in arbitrary_rational(MAX, MAX),
        ) {
            let l_mr = left + (middle + right);
            let lm_r = (left + middle) + right;

            assert_eq!(l_mr, lm_r)
        }

        #[test]
        fn multiplication_is_commutative(
            left in arbitrary_rational(MAX, MAX),
            right in arbitrary_rational(MAX, MAX),
        ) {
            let lr = left * right;
            let rl = right * left;

            assert_eq!(lr, rl)
        }

        #[test]
        fn multiplication_is_associative(
            left in arbitrary_rational(MAX, MAX),
            middle in arbitrary_rational(MAX, MAX),
            right in arbitrary_rational(MAX, MAX),
        ) {
            let l_mr = left * (middle * right);
            let lm_r = (left * middle) * right;

            assert_eq!(l_mr, lm_r)
        }

        #[test]
        fn negate_twice_is_identity(
            r in arbitrary_rational(MAX, MAX),
        ) {
            let actual = -(-r);

            assert_eq!(actual, r)
        }
    }
}
