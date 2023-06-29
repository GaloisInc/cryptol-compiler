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
#[derive(Debug,PartialEq,Eq,Copy,Clone)]
pub struct BitVec<const W: usize, const L: usize>(pub c::Uint<L>);

pub const fn limbs_for_bits(w: usize) -> usize {
  if w == 0 { return 1; }   // crypto_bigint does not support 0 limbs
  (w + c::Limb::BITS - 1) / c::Limb::BITS
}


impl<const W: usize, const L: usize> BitVec<W,L> {

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

  /// Expose internal representation
  pub fn as_words(&self) -> &[c::Word] {
    let BitVec(v) = self;
    v.as_words()
  }

  /// Expose internal representation
  pub fn as_words_mut(&mut self) -> &mut [c::Word] {
    let BitVec(v) = self;
    v.as_words_mut()
  }

  pub fn index_front(&self, i: usize) -> bool {
    assert!(i < W);
    let BitVec(uint) = self;
    uint.bit_vartime(c::Uint::<L>::BITS - 1 - i)
  }

  pub fn index_back(&self, i: usize) -> bool {
    assert!(i < W);
    let BitVec(uint) = self;
    uint.bit_vartime(i + Self::PAD)
  }

  pub fn take<const W1: usize, const L1: usize>(&self) -> BitVec<W1,L1> {
    assert!(W1 <= W);
    assert_eq!(L1, limbs_for_bits(W1));
    let mut uint = <c::Uint<L1>>::default();
    uint.as_words_mut().copy_from_slice(&self.as_words()[ (L - L1) .. ]);
    let mut res = BitVec(uint);
    res.fix_underflow();
    res
  }


  /// Note that the parameters are the size of the result,
  /// NOT how much to drop.
  pub fn drop<const W1: usize, const L1: usize>(&self) -> BitVec<W1,L1> {
    assert!(W1 <= W);
    assert_eq!(L1, limbs_for_bits(W1));
    let mut uint = <c::Uint<L1>>::default();
    let p1 = Self::PAD;
    let p2 = BitVec::<W1,L1>::PAD;
    let ws = self.as_words();
    let mut out = uint.as_words_mut();
    if p1 == p2 {
      out.copy_from_slice(&ws[ .. L1 ]);
    } else if p1 < p2 {
      let off = p2 - p1;
      let w0 = ws[0];
      out[0] = w0 << off;
      let have = c::Limb::BITS - p2;
      let mut prev = w0 >> p2;

      for i in 1 .. L1 {
        let w = ws[i];
        out[i] = (w << have) | prev;
        prev = w >> p2;
      }
    } else {
      let off = p1 - p2;
      let mut prev = ws[0] >> off;
      let have = c::Limb::BITS - p1;
      for i in 0 .. L1 {
        let w = ws[i+1];
          out[i] = (w << have) | prev;
          prev = w >> p1;
      }
    }
    BitVec(uint)
  }

}



// -----------------------------------------------------------------------------
// From


// From u32
impl<const W: usize, const L: usize> From<u32> for BitVec<W,L> {
  fn from(x : u32) -> Self {
    Self::from(<c::Uint<L>>::from_u64((x as u64) << 32))
  }
}


// From u64
impl<const W: usize, const L: usize> From<u64> for BitVec<W,L> {
  fn from(x : u64) -> Self {
    Self::from(<c::Uint<L>>::from(x))
  }
}

// From u128
impl<const W: usize, const L: usize> From<u128> for BitVec<W,L> {
  fn from(x : u128) -> Self {
    if L < 128 / c::Limb::BITS {
      Self::from(x as u64)
    } else {
      Self::from(<c::Uint<L>>::from(x))
    }
  }
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
    assert_eq!(L, limbs_for_bits(W));
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

  // XXX: do we need both to_uint?
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

    let upper = self.as_words();
    let lower = lower_bv.as_words();
    let mut uint_out : c::Uint<LO> = Default::default();
    let out = uint_out.as_words_mut();

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



impl<const W: usize, const L: usize> BitVec<W,L> {

  // Big endian, like Cryptol
  pub fn join<const PARTS: usize, const EACH: usize, const EACH_L: usize>
    (xs: &[ BitVec<EACH,EACH_L>; PARTS ]) -> Self {

    assert_eq!(PARTS * EACH, W);
    assert_eq!(L, limbs_for_bits(W));

    let mut uint_out : c::Uint<L> = Default::default();
    let out = uint_out.as_words_mut();

    // Joining 0 bit vectors
    if EACH == 0 || PARTS == 0 { return BitVec(uint_out); }

    let pad_el    = BitVec::<EACH,EACH_L>::PAD;
    let have_last = c::Limb::BITS - pad_el; // valid bits in 0 word


    if pad_el == 0 {

      // Copy full limbs
      for i in 0 .. PARTS  {
        out[(L - (i+1) * EACH_L) .. (L - i * EACH_L)]
           .copy_from_slice(xs[i].as_words());
      }

    } else {

      let mut buf    = 0;                  // partial word to output
      let mut have   = 0;
      let mut out_ix = L;
      // 1 bigger than where we want write,
      // so that we don't wrap on last iteration.

      for bitvec in xs {

        let ws = bitvec.as_words();

        if have == 0 {
          out[ (out_ix - EACH_L + 1) .. out_ix ].copy_from_slice(&ws[1..]);
          buf     = ws[0];
          have    = c::Limb::BITS - pad_el;
          out_ix -= EACH_L - 1;
        } else {

          // Copy upper full words
          for j in (1 .. EACH_L).rev() {
            let w = ws[j];
            buf |= w >> have;
            out_ix -= 1;      // pre-decrement, as we are 1 bigger.
            out[out_ix] = buf;
            buf  = w << (c::Limb::BITS - have);
          }

          // Last word is not full
          let w = ws[0];
          buf |= w >> have;
          have += have_last;

          if have >= c::Limb::BITS {
            out_ix -= 1;
            out[out_ix] = buf;
            have -= c::Limb::BITS;
            buf = w << (c::Limb::BITS - have - pad_el);
          }
        }
      }

      // Do we have any leftovers
      if have > 0 { out[0] = buf; }
    }

    BitVec(uint_out)
  }
}



// -----------------------------------------------------------------------------
// Traversal

impl<const W: usize, const L: usize> BitVec<W,L> {

  pub fn traverse_bits<'a>(&'a self) -> impl Iterator<Item = bool> + 'a {

    struct S<'a, const W: usize, const L: usize> {
      vec: &'a BitVec<W,L>,
      ix: usize
    }

    impl<'b, const W: usize, const L: usize> Iterator for S<'b, W,L> {
      type Item = bool;
      fn next(&mut self) -> Option<Self::Item> {
        if self.ix >= W {
          None
        } else {
          let i = self.ix;
          self.ix += 1;
          Some(self.vec.index_front(i))
        }
      }
    }

    S::<'a,W,L> { vec: self, ix: 0 }
  }

  pub fn split<'a, const W1: usize, const L1: usize>(&'a self) ->
    impl Iterator<Item = BitVec<W1,L1>> + 'a {

    struct S<'a, const W:  usize, const L: usize
               , const W1: usize, const L1: usize> {
      vec: &'a BitVec<W,L>
    }

    impl<'b, const W:  usize, const L: usize
           , const W1: usize, const L1: usize> Iterator for S<'b,W,L,W1,L1> {
      type Item = BitVec<W1,L1>;
      fn next(&mut self) -> Option<Self::Item> {
        None // XXX
      }
    }

    S::<'a, W, L, W1, L1> { vec: self }
  }

}

// -----------------------------------------------------------------------------
// Formatting
// XXX



// -----------------------------------------------------------------------------
// Macros

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

#[macro_export]
macro_rules! append {
  ($FRONT:expr,$BACK:expr,$xs:expr,$ys:expr) => { {
    const L2: usize = $crate::bitvec_fixed::limbs_for_bits($BACK);
    const W3: usize = $FRONT + $BACK;
    const L3: usize = $crate::bitvec_fixed::limbs_for_bits(W3);
    $xs.append::<$BACK, L2, W3, L3>($ys)
  } }
}

#[macro_export]
macro_rules! join {
  ($PARTS:expr,$EACH:expr,$xs:expr) => { {
    const EACH_L: usize = $crate::bitvec_fixed::limbs_for_bits($EACH);
    const OUT_W:  usize = $PARTS * $EACH;
    const OUT_L:  usize = $crate::bitvec_fixed::limbs_for_bits(OUT_W);
    $crate::bitvec_fixed
        ::BitVec::<OUT_W,OUT_L>::join::<{$PARTS},{$EACH},EACH_L>($xs)
  } }
}

#[macro_export]
macro_rules! take {
  ($FRONT:expr,$xs:expr) => { {
    const L : usize = $crate::bitvec_fixed::limbs_for_bits($FRONT);
    $xs.take::<{$FRONT},L>()
  } }
}


#[macro_export]
macro_rules! drop {
  ($FRONT:expr,$BACK:expr,$xs:expr) => { {
    const L : usize = $crate::bitvec_fixed::limbs_for_bits($BACK);
    $xs.drop::<{$BACK},L>()
  } }
}





// -----------------------------------------------------------------------------
// Tests

#[cfg(test)]
mod tests {
  use num::bigint::ToBigUint;
  use num::bigint::BigUint;

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
    assert_eq!(join!(2,64,&arr), <BitVec!(128)>::from(v128));

    let x1 = <BitVec!(1)>::from(1_u64);
    let arr2  = [x1.clone(),x1.clone(),x1];
    assert_eq!(join!(3,1,&arr2)
              , <BitVec!(3)>::from(0b111_u64));

    let ans = u32::MAX as u128;
    assert_eq!( join!(3,32, &[ <BitVec!(32)>::from(ans); 3 ])
              , <BitVec!(96)>::from((ans << 64) | (ans << 32) | ans)
              );

    assert_eq!( join!(3,63, &[ <BitVec!(63)>::from(0x0123456789abcdef_u64); 3 ])
              , <BitVec!(189)>::from(&"6974557483762978536120337476781526266153645112264740335".parse::<BigUint>().unwrap())
              );

  }

  #[test]
  fn test_index() {
    let a = <BitVec!(3)>::from(0b111_u64);
    assert_eq!(a.index_front(0), true);

    let x = <BitVec!(127)>::from(0b110_u64);
    assert_eq!(x.index_back(0), false);
    assert_eq!(x.index_back(1), true);
    assert_eq!(x.index_back(2), true);
    assert_eq!(x.index_front(2), false);

    let y = <BitVec!(127)>::from(0b110_u128 << 124);
    assert_eq!(y.index_front(0), true);
    assert_eq!(y.index_front(1), true);
    assert_eq!(y.index_front(2), false);
    assert_eq!(y.index_back(2), false);
  }


  #[test]
  fn test_traversal() {
    let x = <BitVec!(3)>::from(0b111_u64);
    let mut count = 0;
    for i in x.traverse_bits() {
      count += 1;
      assert_eq!(i,true);
    }
    assert_eq!(count,3);
  }


  #[test]
  fn test_take_drop() {
    let x = <BitVec!(4)>::from(0b101_u64);
    assert_eq!(take!(2,x), <BitVec!(2)>::from(0b01_u64));
    assert_eq!(drop!(2,2,x), <BitVec!(2)>::from(0b01_u64));
    assert_eq!(drop!(1,3,x), <BitVec!(3)>::from(0b101_u64));
  }

}





