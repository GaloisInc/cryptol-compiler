use crate::bitvec_fixed::BitVec;
use crate::traits::Literal;

impl<const W: usize, const L: usize> Literal for BitVec<W, L> {
    type Length = ();

    fn number_u64(_n: Self::Length, x: u64) -> Self {
        Self::from(x)
    }

    fn number_integer(_n: Self::Length, x: &num::BigUint) -> Self {
        BitVec::<W,L>::from(x)
    }
}
