mod depth;

use crate::complex::Complex;
use depth::Depth;

pub enum Result {
    Finished(Vec<Complex>),
    Ongoing((Vec<Complex>, Complex)),
}

pub fn continued_fraction(z: Complex, max: Depth) -> Result {
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
