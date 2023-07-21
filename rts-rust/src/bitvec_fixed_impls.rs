use crate::bitvec_fixed::BitVec;
use crate::Literal;
use crate::Zero;

impl<const W: usize, const L: usize> Literal for BitVec<W, L> {
  type Length = ();

  fn number_u64(_n: Self::Length, x: u64) -> Self {
    Self::from(x)
  }

  fn number_integer(_n: Self::Length, x: &num::BigUint) -> Self {
    Self::from(x)
  }
}

impl<const W: usize, const L: usize> Zero for BitVec<W, L> {
  type Length = ();

  fn zero(_n: Self::Length) -> Self { Self::zero() }
}
