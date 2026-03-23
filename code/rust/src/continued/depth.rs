use std::cmp::{Ord, Ordering, PartialOrd};
use std::convert::From;

#[derive(Debug, PartialEq, Eq)]
pub enum Depth {
    Finite(u32),
    Infinite,
}

impl Depth {
    pub fn next(&self) -> Self {
        match self {
            Depth::Finite(depth) => Depth::Finite(depth + 1),
            Depth::Infinite => Depth::Infinite,
        }
    }
}

impl From<u32> for Depth {
    fn from(depth: u32) -> Self {
        Depth::Finite(depth)
    }
}

impl Ord for Depth {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Depth::Finite(left), Depth::Finite(right)) => left.cmp(right),
            (Depth::Finite(_), Depth::Infinite) => Ordering::Less,
            (Depth::Infinite, Depth::Finite(_)) => Ordering::Greater,
            (Depth::Infinite, Depth::Infinite) => Ordering::Equal,
        }
    }
}

impl PartialOrd for Depth {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Option::Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn depth_can_be_created_from_u32() {
        let actual: Depth = 37u32.into();
        let expected = Depth::Finite(37);

        assert_eq!(actual, expected)
    }

    #[test]
    fn less_then_works_correctly() {
        assert!(Depth::Finite(37) < Depth::Finite(51));
        assert!(Depth::Finite(37) < Depth::Infinite);
    }
}
