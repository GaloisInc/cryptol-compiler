use crate::bitvec_fixed::BitVec;
use crate::traits::*;

impl<const W: usize, const L: usize> Length for BitVec<W, L> {
  type Length = ();
}

impl<const W: usize, const L: usize> Literal for BitVec<W, L> {
  fn number_u64(_n: Self::Length, x: u64) -> Self { Self::from(x) }
  fn number_int(_n: Self::Length, x: &num::BigUint) -> Self { Self::from(x) }
}

impl<const W: usize, const L: usize> Zero for BitVec<W, L> {
  fn zero(_n: Self::Length) -> Self { Self::zero() }
}
