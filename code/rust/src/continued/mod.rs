mod depth;

use crate::complex::Complex;
use crate::traits::IsZero;
use depth::Depth;

pub enum Result<T> {
    Finished(Vec<Complex<T>>),
    Ongoing((Vec<Complex<T>>, Complex<T>)),
}

pub fn continued_fraction<T: IsZero + Copy>(z: Complex<T>, max: Depth) -> Result<T> {
    let mut result = vec![];
    let mut current = Depth::Finite(0);
    let mut residue = z;
    while current < max && !residue.is_zero() {
        current.next();
    }

    if residue.is_zero() {
        Result::Finished(result)
    } else {
        Result::Ongoing((result, residue))
    }
}
