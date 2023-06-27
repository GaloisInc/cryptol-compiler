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
#[derive(Debug,PartialEq,Eq,Clone)]
pub struct BitVec<const W: usize, const L: usize>(pub c::Uint<L>);

pub const fn limbs_for_bits(w: usize) -> usize {
  if w == 0 { return 1; }   // crypto_bigint does not support 0 limbs
  (w + c::Limb::BITS - 1) / c::Limb::BITS
}

#[macro_export]
/// A convenient way to write a `BitVec` type without having to specify
/// the number of limbs.
macro_rules! BitVec {

  // XXX: we can add extra cases here to handle small bit-vectors
  // (e.g., <= 32)

  ($w:expr) => { $crate::bitvec_fixed::BitVec
                       < $w
                       , {$crate::bitvec_fixed::limbs_for_bits($w)}
                       >
  };
}


impl<const W: usize, const L: usize> BitVec<W,L> {

  const SIZE_OK: ()          = assert!(limbs_for_bits(W) == L);

  /// Total number of bits in the underlying type
  pub const BITS: usize      = L * c::Limb::BITS;

  /// Extra padding space
  pub const PAD: usize       = Self::BITS - W;

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
}


// -----------------------------------------------------------------------------
// From


// From u64
impl<const W: usize, const L: usize> From<u64> for BitVec<W,L> {
  fn from(x : u64) -> Self { Self::from(<c::Uint<L>>::from_u64(x)) }
}


// From u128
impl<const W: usize, const L: usize> From<u128> for BitVec<W,L> {
  fn from(x : u128) -> Self { Self::from(<c::Uint<L>>::from_u128(x)) }
}




// From Integer
impl<const W: usize, const L: usize> From<&n::BigUint> for BitVec<W,L> {
  fn from(n: &n::BigUint) -> Self {
    let mut result = < c::Uint<L> >::default();
    let buf        = result.as_words_mut();
    for (r,d) in buf.iter_mut().zip(n.iter_u64_digits()) { *r = d }
    Self::from(result)
  }
}

impl<const W: usize, const L: usize> From<c::Uint<L>> for BitVec<W,L> {
  fn from(x: c::Uint<L>) -> Self {
    let _ = Self::SIZE_OK;
    if Self::PAD == 0 { return BitVec(x) }
    BitVec(x << Self::PAD)
  }
}


// -----------------------------------------------------------------------------



// -----------------------------------------------------------------------------
// Arithmetic
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
    let x = BitVec::<W,L>::from(0_u64);
    &x - self
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
    Self::Output::from(lhs.wrapping_rem(&rhs))
  }
}



// -----------------------------------------------------------------------------
// Append

impl<const WU: usize, const LU: usize> BitVec<WU,LU> {

  pub fn append
    < const WL: usize, const LL: usize
    , const WO: usize, const LO: usize
    >
    (&self, lower_bv: &BitVec<WL,LL>) -> BitVec<WO,LO> {

    let BitVec(uint_upper) = self;
    let BitVec(uint_lower) = lower_bv;
    let upper              = uint_upper.as_words();
    let lower              = uint_lower.as_words();
    let mut uint_out : c::Uint<LO> = Default::default();
    let out                = uint_out.as_words_mut();

    assert_eq!(LU, upper.len());
    assert_eq!(LL, lower.len());
    assert_eq!(LO, out.len());

    let pad_upper = BitVec::<WU,LU>::PAD;
    let pad_lower = BitVec::<WL,LL>::PAD;
    let mid       = LO - LU;

    // First we copy the upper word into the output.
    if WU > 0 { out[ mid ..].copy_from_slice(upper); }

    if pad_upper == 0 || WU == 0 {
      if WL > 0 { out[0 .. LL].copy_from_slice(lower); }
    } else {

      let have      = c::Limb::BITS - pad_upper;
      let mut out_v = upper[0];
      for i in (0 .. LL).rev() {
        let w        = lower[i];
        out[mid - i] = out_v | (w >> have);
        out_v        = w << pad_upper;
      }
      if have > pad_lower { out[0] = out_v; }
    }

    BitVec::<WO,LO>(uint_out)
  }
}


#[macro_export]
macro_rules! append {
  ($W1:expr,$W2:expr,$xs:expr,$ys:expr) => { {
    const L2: usize = $crate::bitvec_fixed::limbs_for_bits($W2);
    const W3: usize = $W1 + $W2;
    const L3: usize = $crate::bitvec_fixed::limbs_for_bits(W3);
    $xs.append::<$W2, L2, W3, L3>($ys)
  } }

}


impl<const W: usize, const L: usize> BitVec<W,L> {
  pub fn join<const N: usize, const EW: usize, const EL: usize>
    (xs: &[ BitVec<EW,EL>; N ]) -> Self {

    assert_eq!(N * EW, W);
    assert_eq!(L, limbs_for_bits(W));

    let mut uint_out : c::Uint<L> = Default::default();
    let out = uint_out.as_words_mut();
    if EW == 0 { return BitVec(uint_out); }

    let pad_el = BitVec::<EW,EL>::PAD;

    if pad_el == 0 {
      for i in 0 .. N {
        let BitVec(el) = xs[i];
        out[ (L - (i+1) * EL) .. (L - i * EL) ].copy_from_slice(el.as_words());
      }
    } else {
      todo!(); // XXX
    }

    BitVec(uint_out)
  }
}





#[cfg(test)]
mod tests {
  use num::bigint::ToBigUint;
  use super::*;

  #[test]
  fn test_append() {
    let x0  = <BitVec!(0)>::from(0_u64);

    let v1 : u64 = 0b1;
    let x1  = <BitVec!(1)>::from(v1);

    let v5 : u64 = 0b10001;
    let x5  = <BitVec!(5)>::from(v5);

    let v64 : u64 = (1 << 63) + 1;
    let x64 = <BitVec!(64)>::from(v64);



    // small small
    assert_eq!(append!(0,0,&x0,&x0), x0);
    assert_eq!(append!(0,1,&x0,&x1), x1);
    assert_eq!(append!(1,0,&x1,&x0), x1);
    assert_eq!(append!(5,1,&x5,&x1),   <BitVec!(6)>::from(0b100011_u64));
    assert_eq!(append!(1,5,&x1,&x5),   <BitVec!(6)>::from(0b110001_u64));

    let m = (((v1 as u128) << 64) | (v64 as u128)).to_biguint().unwrap();
    assert_eq!(append!(1,64,&x1,&x64), <BitVec!(65)>::from(&m));

    let m1 = (((v64 as u128) << 1) | (v1 as u128)).to_biguint().unwrap();
    assert_eq!(append!(64,1,&x64,&x1), <BitVec!(65)>::from(&m1));

  }

  #[test]
  fn test_join() {
    let v64 : u64 = (1 << 63) + 3;
    let x64 = <BitVec!(64)>::from(v64);

    let v128 = ((v64 as u128) << 64) | (v64 as u128);
    let arr  = [x64.clone(),x64];
    assert_eq!(<BitVec!(128)>::join(&arr), <BitVec!(128)>::from(v128));
  }

}





