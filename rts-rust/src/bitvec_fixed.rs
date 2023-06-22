use std::ops::{Add,Sub,Neg,Mul,Div,Rem};
use crypto_bigint as c;
use num::bigint as n;

/// BitVec is a wrapper around crypto_bigint::Uint, that allows us to handle
/// bit vectors whose sizes are not multiples of 64.   We represent such values
/// with a bit vector with an extra limb, and the value is *left shifted* so
/// that its most-significant bit is in the most-significant bit of the `Uint`.
/// The unused least significant bits are set to 0.  This representation allows
/// us to reuse many operations without any additional overhead
/// (e.g., add, compare, etc).
#[derive(Debug)]
pub struct BitVec<const W: usize, const L: usize>(c::Uint<L>);

pub const fn limbs_for_bits(w: usize) -> usize {
  if w == 0 { return 1; }   // crypto_bigint does not support 0 limbs
  (w + c::Limb::BITS - 1) / c::Limb::BITS
}

#[macro_export]
/// A conveninet way to write a `BitVec` type without having to specify
/// the number of limbs.
macro_rules! BitVec {

  // XXX: we can add extra cases here to handle small bit-vectors
  // (e.g., <= 32)

  ($w:literal) => { $crate::bitvec_fixed::BitVec
                       < $w
                       , {$crate::bitvec_fixed::limbs_for_bits($w)}
                       >
  };
}

impl<const W: usize, const L: usize> BitVec<W,L> {

  const SIZE_OK: ()      = assert!(limbs_for_bits(W) == L);

  /// Total number of bits in the underlying type
  const BITS: usize      = L * c::Limb::BITS;

  /// Extra padding space
  const PAD: usize       = Self::BITS - W;

  /// Clear up any bits in the least significant position of the full Uint
  pub fn fix_underflow(&mut self) {
    if Self::PAD == 0 { return; }
    let BitVec(me) = self;
    let buf = me.as_words_mut();
    let mask : c::Word = ! ((1 << Self::PAD) - 1);
    buf[0] &= mask
  }

  /// Convert to Uint from crypto_bigint
  pub fn to_uint(&self) -> c::Uint<L> {
    let BitVec(v) = self;
    if Self::PAD == 0 { return *v }
    v >> Self::PAD
  }


  /// Construct using a Uint from `crypto-bigint`.
  pub fn from_uint(x: c::Uint<L>) -> Self {
    let _ = Self::SIZE_OK;
    if Self::PAD == 0 { return BitVec(x) }
    BitVec(x << Self::PAD)
  }

  /// Construct using a u64
  pub fn from_u64(x: u64) -> Self {
    Self::from_uint(<c::Uint<L>>::from_u64(x))
  }

  /// Construct using a BigUint from `num`.
  pub fn from_integer(n: &n::BigUint) -> Self {
    let mut result = < c::Uint<L> >::default();
    let buf = result.as_words_mut();

    for (r,d) in buf.iter_mut().zip(n.iter_u64_digits()) { *r = d }

    Self::from_uint(result)
  }

}

impl<const W: usize, const L: usize> Add for &BitVec<W,L> {
  type Output = BitVec<W,L>;

  fn add(self, other: Self) -> Self::Output {
    let BitVec(lhs) = self;
    let BitVec(rhs) = other;
    BitVec(lhs.wrapping_add(rhs))
  }
}

impl<const W: usize, const L: usize> Sub for &BitVec<W,L> {
  type Output = BitVec<W,L>;

  fn sub(self, other: Self) -> Self::Output {
    let BitVec(lhs) = self;
    let BitVec(rhs) = other;
    BitVec(lhs.wrapping_sub(rhs))
  }
}

impl<const W: usize, const L: usize> Neg for &BitVec<W,L> {
  type Output = BitVec<W,L>;

  fn neg(self) -> Self::Output {
    &Self::Output::from_u64(0) - self
  }
}

impl<const W: usize, const L: usize> Mul for &BitVec<W,L> {
  type Output = BitVec<W,L>;

  fn mul(self, other: Self) -> Self::Output {
    let lhs = self.to_uint();
    let BitVec(rhs) = other;
    BitVec(lhs.wrapping_mul(&rhs))
  }
}

impl<const W: usize, const L: usize> Div for &BitVec<W,L> {
  type Output = BitVec<W,L>;

  fn div(self, other: Self) -> Self::Output {
    let BitVec(lhs) = self;
    let rhs = other.to_uint();
    let mut result = BitVec(lhs.wrapping_div(&rhs));
    result.fix_underflow();
    result
  }
}


impl<const W: usize, const L: usize> Rem for &BitVec<W,L> {
  type Output = BitVec<W,L>;

  fn rem(self, other: Self) -> Self::Output {
    let lhs = self.to_uint();
    let rhs = other.to_uint();
    Self::Output::from_uint(lhs.wrapping_rem(&rhs))
  }
}







#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test() {
  }

}





