mod depth;

use crate::complex::Complex;
use crate::traits::{Ceiling, Integral, Invert, IsZero};
use depth::Depth;
use std::ops::{Add, Mul, Neg};

#[derive(Debug, PartialEq, Eq)]
pub enum Result<T> {
    Finished(Vec<Complex<T>>),
    Ongoing((Vec<Complex<T>>, Complex<T>)),
}

pub fn continued_fraction<
    T: Add<T, Output = T>
        + Mul<T, Output = T>
        + Neg<Output = T>
        + Invert<Output = T>
        + IsZero
        + Ceiling<Output = T>
        + Integral
        + Copy,
>(
    z: Complex<T>,
    max: Depth,
) -> Result<T> {
    let mut result = vec![];
    let mut current = Depth::Finite(0);
    let mut integral = z.integral();
    result.push(integral);
    let mut residue = z - integral;
    while current < max && !residue.is_zero() {
        current = current.next();

        let inv = residue.invert().expect("non-zero residue");
        integral = inv.integral();
        result.push(integral);
        residue = inv - integral;
    }

    if residue.is_zero() {
        Result::Finished(result)
    } else {
        Result::Ongoing((result, residue))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rational::Rational;

    #[test]
    fn real_rational_should_have_a_finite_continued_fraction() {
        let q = Rational::create(1, 3).expect("a rational");
        let source = Complex::create(q, 0.into());

        let actual = continued_fraction(source, Depth::Infinite);

        assert_eq!(
            actual,
            Result::Finished(vec![
                Complex::create(0.into(), 0.into()),
                Complex::create(3.into(), 0.into()),
            ])
        );
    }

    #[test]
    fn rational_should_have_a_finite_continued_fraction() {
        let q = Rational::create(4, 3).expect("a rational");
        let source = Complex::create(q, 1.into());

        let actual = continued_fraction(source, Depth::Finite(10));

        assert_eq!(
            actual,
            Result::Finished(vec![
                Complex::create(1.into(), 1.into()),
                Complex::create(3.into(), 0.into()),
            ])
        );
    }
}
