mod depth;

use crate::complex::Complex;
use crate::traits::{Ceiling, Integral, Invert, IsZero};
use depth::Depth;
use std::ops::{Add, Mul, Neg};

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
