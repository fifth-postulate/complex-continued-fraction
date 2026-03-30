pub trait Invert {
    type Output;

    fn invert(&self) -> Option<Self::Output>;
}

pub trait IsZero {
    fn is_zero(&self) -> bool;
}

pub trait Ceiling {
    type Output;

    fn ceil(&self) -> Self::Output;
}

pub trait Integral {
    fn integral(&self) -> Self;
}
