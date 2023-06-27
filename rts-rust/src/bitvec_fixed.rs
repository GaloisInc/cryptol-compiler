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
#[derive(Debug,PartialEq,Eq)]
pub struct BitVec<const W: usize, const L: usize>(pub c::Uint<L>);

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

  ($w:expr) => { $crate::bitvec_fixed::BitVec
                       < $w
                       , {$crate::bitvec_fixed::limbs_for_bits($w)}
                       >
  };
}


impl<const W: usize, const L: usize> BitVec<W,L> {

  const SIZE_OK: ()      = assert!(limbs_for_bits(W) == L);

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

impl<const WU: usize, const LU: usize> BitVec<WU,LU> {

  pub fn append
    < const WL: usize, const LL: usize
    , const WO: usize, const LO: usize
    >
    (&self, lower_bv: &BitVec<WL,LL>) -> BitVec<WO,LO> {

    let BitVec(uint_upper) = self;
    let BitVec(uint_lower) = lower_bv;
    let upper = uint_upper.as_words();
    let lower = uint_lower.as_words();
    let mut uint_out : c::Uint<LO> = Default::default();
    let out = uint_out.as_words_mut();

    assert_eq!(LU, upper.len());
    assert_eq!(LL, lower.len());
    assert_eq!(LO, out.len());

    let pad_upper: usize  = LU * c::Limb::BITS - WU;
    let pad_lower: usize  = LL * c::Limb::BITS - WL;
    let pad_out:   usize  = LO * c::Limb::BITS - WO;

    const fn other_half(this_half: usize) -> usize { c::Limb::BITS - this_half }

    let mut out_ix : usize    = 0;  // limb to write into
    let mut out_v  : c::Word  = 0;  // partial word to write into index

    let mut write_out = |x : c::Word| {
      if pad_out == 0 {
        out[out_ix] = x;
      } else {
        out[out_ix] = (x << pad_out) | out_v;
        out_v       = x >> other_half(pad_out);
      }
      out_ix += 1;
    };

    // Complete a partially read word, and produce a new partially read one.
    // assume: read_have < c::Limb::BITS
    let read_in = |read_arr:&[c::Word], read_have, read_v, read_ix| {
      let v = read_arr[read_ix];
      if read_have == 0 {
        (v,0)
      } else {
        ( (v << read_have) | read_v
        , v >> other_half(read_have)
        )
      }
    };

    let mut read_v = lower[0] >> pad_lower;
    let mut have   = other_half(pad_lower);
    for i in 1 .. LL {
      let (a,new_read_v) = read_in(lower,have,read_v,i);
      write_out(a);
      read_v = new_read_v;
    }

    let start = upper[0] >> pad_upper;
    let have2 = other_half(pad_upper);

    if have == c::Limb::BITS {
      write_out(read_v);
      read_v = start;
      have   = have2;
    } else {
      let w   = (start << have) | read_v;
      let tot = have + have2;

      if tot >= c::Limb::BITS {
        write_out(w);
        read_v = start >> other_half(have);
        have   = tot - c::Limb::BITS;
      } else {
        read_v = w;
        have   = tot;
      }
    }

    if have == c::Limb::BITS {
      write_out(read_v);
      have   = 0;
      read_v = 0;
    }

    for i in 1 .. LU {
      let (a,new_read_v) = read_in(upper,have,read_v,i);
      write_out(a);
      read_v = new_read_v;
    }

    if have > 0 { write_out(read_v) }

    BitVec::<WO,LO>(uint_out)
  }
}


#[macro_export]
macro_rules! append {
  ($W1:expr,$W2:expr,$xs:expr,$ys:expr) => { {
    const L1: usize = $crate::bitvec_fixed::limbs_for_bits($W1);
    const L2: usize = $crate::bitvec_fixed::limbs_for_bits($W2);
    const W3: usize = $W1 + $W2;
    const L3: usize = $crate::bitvec_fixed::limbs_for_bits(W3);
    $xs.append::<$W2, L2, W3, L3>($ys)
  } }

}







#[cfg(test)]
mod tests {
  use num::bigint as n;

  #[test]
  fn test_append() {
    let v1 : u64 = 0b1;
    let x1  = <BitVec!(1)>::from_u64(v1);
    let v5 : u64 = 0b10001;
    let x5  = <BitVec!(5)>::from_u64(v5);
    let v64 : u64 = (1 << 63) + 1;
    let x64 = <BitVec!(64)>::from_u64(v64);

    let m = (vy).to_bigint();

    // small small
    assert_eq!(append!(5,1,&x5,&x1), <BitVec!(6)>::from_u64(0b100011));

  }

}





