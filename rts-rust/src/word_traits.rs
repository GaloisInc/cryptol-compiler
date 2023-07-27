use crate::word::Word;
use crate::traits::*;

impl<const W: usize, const L: usize> Length for Word<W, L> {
  type Length = ();
}

impl<const W: usize, const L: usize> Literal for Word<W, L> {
  fn number_u64(_n: Self::Length, x: u64) -> Self { Self::from(x) }
  fn number_int(_n: Self::Length, x: &num::BigUint) -> Self { Self::from(x) }
}

impl<const W: usize, const L: usize> Zero for Word<W, L> {
  fn zero(_n: Self::Length) -> Self { Self::zero() }
}

impl<const W: usize, const L: usize> Integral for Word<W, L> {
  fn to_u64    (x: &Self)       -> u64         {
    assert!(x <= &Self::from(u64::MAX));
    <_>::from(x)
  }
  fn to_integer(x: &Self)       -> num::BigInt { <_>::from(x) }

  fn div   (x: &Self, y: &Self) -> Self        { x / y }
  fn modulo(x: &Self, y: &Self) -> Self        { x % y }
}

impl<const W: usize, const L: usize> Ring for Word<W, L> {
  fn negate      (x: &Self)           -> Self { -x }
  fn add         (x: &Self, y: &Self) -> Self { x + y }
  fn mul         (x: &Self, y: &Self) -> Self { x * y }
  fn sub         (x: &Self, y: &Self) -> Self { x - y }
  fn exp         (x: &Self, y: u64)   -> Self { x.exp(y) }
  fn from_integer(x: &num::BigInt)    -> Self { Self::from(x) }
}
