use std::ops::{Add,Sub,Neg,Mul,Div,Rem};
use crypto_bigint as c;
use std::fmt::*;

/// Word is a wrapper around crypto_bigint::Uint, that allows us to handle
/// bit vectors whose sizes are not multiples of 64.   We represent such values
/// with a bit vector with an extra limb, and the value is *left shifted* so
/// that its most-significant bit is in the most-significant bit of the `Uint`.
/// The unused least significant bits are set to 0.  This representation allows
/// us to reuse many operations without any additional overhead
/// (e.g., add, compare, etc).
#[derive(Debug,PartialEq,Eq,PartialOrd,Ord,Copy,Clone)]
pub struct Word<const W: usize, const L: usize>(pub c::Uint<L>);


/// How many bits we need to represent the given number of bits.
pub const fn limbs_for_bits(w: usize) -> usize {
  if w == 0 { return 1; }   // crypto_bigint does not support 0 limbs
  (w + c::Limb::BITS - 1) / c::Limb::BITS
}


impl<const W: usize, const L: usize> Word<W,L> {

  /// Total number of bits in the underlying type, including padding.
  pub const BITS: usize      = L * c::Limb::BITS;

  /// Extra padding space
  pub const PAD: usize       = Self::BITS - W;

  /// A vector filled with 0.
  pub fn zero() -> Word<W,L> {
    assert_eq!(L, limbs_for_bits(W));
    Word(c::Uint::<L>::default())
  }

  /// Clear up any bits in the least significant position of the full Uint.
  pub fn fix_underflow(&mut self) {
    if Self::PAD == 0 { return; }
    let Word(me) = self;
    let buf = me.as_words_mut();
    buf[0] &= !((1 << Self::PAD) - 1);
  }

  /// Convert to Uint from crypto_bigint
  pub fn to_uint(&self) -> c::Uint<L> {
    let Word(v) = self;
    if Self::PAD == 0 { return *v }
    v >> Self::PAD
  }

  /// Expose internal representation
  pub fn as_internal_uint(&self) -> &c::Uint<L> {
    let Word(v) = self;
    v
  }

  /// Expose internal representation
  pub fn as_internal_uint_mut(&mut self) -> &mut c::Uint<L> {
    let Word(v) = self;
    v
  }

  /// Expose internal representation
  pub fn as_words(&self) -> &[c::Word] {
    self.as_internal_uint().as_words()
  }

  /// Expose internal representation
  pub fn as_words_mut(&mut self) -> &mut [c::Word] {
    self.as_internal_uint_mut().as_words_mut()
  }


  /// Get the bit at the given position. 0 is most significant bit.
  pub fn index_front(&self, i: usize) -> bool {
    assert!(i < W);
    self.as_internal_uint().bit_vartime(Self::BITS - 1 - i)
  }

  /// Get the bit at the given position. 0 is least significant bit.
  pub fn index_back(&self, i: usize) -> bool {
    assert!(i < W);
    self.as_internal_uint().bit_vartime(i + Self::PAD)
  }

  /// Extract a sub-bitvector of the given length, starting at the given
  /// position. 0 is the most significant bit.
  pub fn slice_be<const W1: usize, const L1: usize>
    (&self, i: usize) -> Word<W1,L1> {
    assert!((i + W1) <= W);
    let ws = self.as_words();
    let mut result = Word::<W1,L1>::zero();

    // Slice 0
    if W1 == 0 { return result }

    let out   = result.as_words_mut();
    let start = L - i / c::Limb::BITS;  // 1 bigger than where we read
    let off   = i % c::Limb::BITS;

    if off == 0 {

      // Slice aligned
      out.copy_from_slice(&ws[(start - L1) .. start]);

    } else {

      // Slice unaligned
      let mut prev = ws[start - 1] << off;
      let other = c::Limb::BITS - off;
      for j in 1 .. L1 {
        let w = ws[start - j - 1];
        out[L1 - j] = prev | (w >> other);
        prev = w << off;
      }
      out[0] = prev;

    }

    result.fix_underflow();
    result
  }

  pub fn slice_le<const W1: usize, const L1: usize>
    (&self, i: usize) -> Word<W1,L1> {
    assert!((i + W1) <= W);
    self.slice_be(W - W1 - i)
  }


  pub fn take<const W1: usize, const L1: usize>(&self) -> Word<W1,L1> {
    self.slice_be(0)
  }

  /// Note that the parameters are the size of the result,
  /// NOT how much to drop.
  pub fn drop<const W1: usize, const L1: usize>(&self) -> Word<W1,L1> {
    self.slice_be(W - W1)
  }

  /// Convert to a vector in base 2^32.  Least significant first
  /// Convenient for conversion to bignum
  pub fn as_vec_u32(&self) -> Vec<u32> {
    let mut result = Vec::<u32>::new();
    if W == 0 { return result; }

    let ws = self.as_words();
    let pad = Word::<W,L>::PAD;
    // we assume that c::Limb::BITS + 32 <= 128
    let mut w = (ws[0] >> pad) as u128;
    let mut have = c::Limb::BITS - pad;

    while have >= 32 {
      result.push(w as u32);
      w = w >> 32;
      have -= 32;
    };

    for v in &ws[1..] {
      w = ((*v as u128) << have) | w;
      have += c::Limb::BITS;
      while have >= 32 {
        result.push(w as u32);
        w = w >> 32;
        have -= 32;
      };
    }
    if have > 0 {
      result.push(w as u32);
    }
    result
  }



}



// -----------------------------------------------------------------------------
// From and To

// To u8
/// Get the least significant bits
impl<const W: usize, const L: usize> From<&Word<W,L>> for u8 {
  fn from(x: &Word<W,L>) -> Self {
    let pad   = Word::<W,L>::PAD;
    let have  = c::Limb::BITS - pad;
    let ws    = x.as_words();
    let mut w = ws[0] >> pad;
    if W >= 8 && have < 8 {
      w |= ws[1] << have;
    }
    w as u8
  }
}


// To Word (aka u64)
/// Get the least significant bits
impl<const W: usize, const L: usize> From<&Word<W,L>> for c::Word {
  fn from(x: &Word<W,L>) -> Self {
    x.slice_le::<{c::Limb::BITS},1>(0).as_words()[0]
  }
}


// To num::BigUint
/// Get the least significant bits
impl<const W: usize, const L: usize> From<&Word<W,L>> for num::BigUint {
  fn from(x: &Word<W,L>) -> Self {
    Self::new(x.as_vec_u32())
  }
}

// To num::BigInt
/// Get the least significant bits
impl<const W: usize, const L: usize> From<&Word<W,L>> for num::BigInt {
  fn from(x: &Word<W,L>) -> Self { <_>::from(num::BigUint::from(x)) }
}





// From u32
impl<const W: usize, const L: usize> From<u32> for Word<W,L> {
  fn from(x : u32) -> Self {
    Self::from(<c::Uint<L>>::from_u64(x as u64))
  }
}


// From u64
impl<const W: usize, const L: usize> From<u64> for Word<W,L> {
  fn from(x : u64) -> Self {
    Self::from(<c::Uint<L>>::from(x))
  }
}

// From u128
impl<const W: usize, const L: usize> From<u128> for Word<W,L> {
  fn from(x : u128) -> Self {
    if L < 128 / c::Limb::BITS {
      Self::from(x as u64)
    } else {
      Self::from(<c::Uint<L>>::from(x))
    }
  }
}

// From num::BigUint
impl<const W: usize, const L: usize> From<&num::BigUint> for Word<W,L> {
  // XXX: We first make uint, then shift it, which does 2 copies.
  // Should be able to do it in one go.
  fn from(n: &num::BigUint) -> Self {
    let mut result = < c::Uint<L> >::default();
    let buf        = result.as_words_mut();
    for (r,d) in buf.iter_mut().zip(n.iter_u64_digits()) { *r = d }
    Self::from(result)
  }
}

// From num::BigInt
impl<const W: usize, const L: usize> From<&num::BigInt> for Word<W,L> {
  // XXX: We first make uint, then shift it, which does 2 copies.
  // Should be able to do it in one go.
  fn from(n: &num::BigInt) -> Self {
    let mut result = < c::Uint<L> >::default();
    let pad : u8   = match n.sign() {
                       num::bigint::Sign::Minus => 255,
                       _ => 0
                     };
    let buf        = result.as_words_mut();
    let mut bytes  = n.to_signed_bytes_le().into_iter();
    for i in buf.iter_mut() {
      let mut w: c::Word = 0;
      for z in 0 .. c::Limb::BYTES {
        let b = match bytes.next() {
                  None    => pad,
                  Some(x) => x
                };
        w |= (b as c::Word) << (8 * z);
      }
      *i = w;
    }
    let it = Self::from(result);
    it
  }
}

// From c::Uint
impl<const W: usize, const L: usize> From<c::Uint<L>> for Word<W,L> {
  fn from(x: c::Uint<L>) -> Self {
    assert_eq!(L, limbs_for_bits(W));
    if Self::PAD == 0 { return Word(x) }
    Word(x << Self::PAD)
  }
}


// -----------------------------------------------------------------------------



// -----------------------------------------------------------------------------
// Arithmetic

impl<const W: usize, const L: usize> Add for &Word<W,L> {
  type Output = Word<W,L>;

  fn add(self, other: Self) -> Self::Output {
    Word(self.as_internal_uint().wrapping_add(other.as_internal_uint()))
  }
}

impl<const W: usize, const L: usize> Sub for &Word<W,L> {
  type Output = Word<W,L>;

  fn sub(self, other: Self) -> Self::Output {
    Word(self.as_internal_uint().wrapping_sub(other.as_internal_uint()))
  }
}

impl<const W: usize, const L: usize> Neg for &Word<W,L> {
  type Output = Word<W,L>;

  fn neg(self) -> Self::Output {
    &Word::<W,L>::zero() - self
  }
}

impl<const W: usize, const L: usize> Mul for &Word<W,L> {
  type Output = Word<W,L>;

  fn mul(self, other: Self) -> Self::Output {
    Word(self.to_uint().wrapping_mul(other.as_internal_uint()))
  }
}

impl<const W: usize, const L: usize> Div for &Word<W,L> {
  type Output = Word<W,L>;

  fn div(self, other: Self) -> Self::Output {
    let mut result =
            Word(self.as_internal_uint().wrapping_div(&other.to_uint()));
    result.fix_underflow();
    result
  }
}

impl<const W: usize, const L: usize> Rem for &Word<W,L> {
  type Output = Word<W,L>;

  // XXX: do we need both to_uint?
  fn rem(self, other: Self) -> Self::Output {
    Word::<W,L>::from(self.to_uint().wrapping_rem(&other.to_uint()))
  }
}

/// Raise the number to the given power
/// pow w 0 = 1
/// pow w (2*n) = pow (w * w) n
/// pow w (n + 1) = w * pow x n

impl<const W: usize, const L: usize> Word<W,L> {

  pub fn exp(&self, mut n: u64) -> Self {

    let mut base = *self;
    let mut res  = <_>::from(1_u64);
    while n > 0 {
      if n & 1 == 1 {
        res = &res * &base;
        n -= 1;
      } else {
        base = &base * &base;
        n = n >> 1;
      }
    }
    res
  }

}




// -----------------------------------------------------------------------------
// Append

impl<const WU: usize, const LU: usize> Word<WU,LU> {

  pub fn append
    < const WL: usize, const LL: usize
    , const WO: usize, const LO: usize
    >
    (&self, lower_bv: &Word<WL,LL>) -> Word<WO,LO> {

    let upper      = self.as_words();
    let lower      = lower_bv.as_words();
    let mut result = Word::<WO,LO>::zero();
    let out        = result.as_words_mut();

    let pad_upper = Word::<WU,LU>::PAD;
    let pad_lower = Word::<WL,LL>::PAD;
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

    result
  }
}



impl<const W: usize, const L: usize> Word<W,L> {

  /// Big endian.  The iterator should produce exactly (W / EACH) iterms.
  pub fn join<const EACH: usize, const EACH_L: usize>
    (xs: impl Iterator<Item = Word<EACH,EACH_L>>) -> Word<W,L> {

    let parts = W / EACH;

    assert_eq!(parts * EACH, W);
    let mut result = Word::<W,L>::zero();
    let out = result.as_words_mut();

    // Joining 0 bit vectors
    if EACH == 0 || W == 0 { return result; }

    let pad_el    = Word::<EACH,EACH_L>::PAD;
    let have_last = c::Limb::BITS - pad_el; // valid bits in 0 word


    if pad_el == 0 {

      // Copy full limbs
      for (i,bv) in xs.enumerate() {
        out[(L - (i+1) * EACH_L) .. (L - i * EACH_L)]
           .copy_from_slice(bv.as_words());
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


    result
  }


}



// -----------------------------------------------------------------------------
// Traversal

/// Traverse Word<W,L> as bits, starting from most significant.
pub struct TraverseBits<'a, const W: usize, const L: usize> {
  vec: &'a Word<W,L>,
  ix:  usize
}

impl<'a, const W: usize, const L: usize> Iterator for TraverseBits<'a,W,L> {
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

impl<'a, const W: usize, const L: usize> IntoIterator for &'a Word<W,L> {
  type Item     = bool;
  type IntoIter = TraverseBits<'a,W,L>;
  fn into_iter(self) -> Self::IntoIter { TraverseBits { vec: self, ix: 0 } }
}


/// Traverse Word<W,L> as words Word<W1,L1>
/// Starting from most significant end.
pub
struct TraverseWords <'a, const W: usize, const L: usize
                        , const EACH_W: usize, const EACH_L: usize> {
  vec:  &'a Word<W,L>,
  ix: usize,
}

impl<'a, const W:  usize, const L: usize
       , const EACH_W: usize, const EACH_L: usize>
   Iterator for TraverseWords<'a,W,L,EACH_W,EACH_L> {

  type Item = Word<EACH_W,EACH_L>;

  fn next(&mut self) -> Option<Self::Item> {
    let todo = if EACH_W == 0 { 0 } else { W / EACH_W };
    if self.ix >= todo { return None }
    let res = self.vec.slice_be(self.ix * EACH_W);
    self.ix += 1;
    Some(res)
  }
}

impl<const W: usize, const L: usize> Word<W,L> {

  pub fn split<'a, const EACH_W: usize, const EACH_L: usize>(&'a self) ->
    TraverseWords<'a, W, L, EACH_W, EACH_L> {

    assert!(if EACH_W == 0 { W == 0 } else { W % EACH_W == 0 });

    TraverseWords { vec: self, ix: 0 }
  }

}

// -----------------------------------------------------------------------------
// Formatting


impl<const W: usize, const L: usize> Binary for Word<W,L> {
  fn fmt(&self, f: &mut Formatter) -> Result {
    let mut s = String::new();
    if W == 0 {
      s.push('0'); // special case so that we see something.
    } else {
      for b in self.into_iter() {
        s.push(if b { '1' } else { '0' })
      }
    }
    f.pad_integral(true, "0b", &s)
  }
}

impl<const W: usize, const L: usize> Word<W,L> {
  fn fmt_hex(&self, f: &mut Formatter, table: [char; 16]) -> Result {
    let mut s = String::new();
    let extra = W % 4;
    let mut emit = |x| s.push(table[ x as usize ]);

    match extra {
      1 => emit(u8::from(&self.slice_be::<1,1>(0))),
      2 => emit(u8::from(&self.slice_be::<2,1>(0))),
      3 => emit(u8::from(&self.slice_be::<3,1>(0))),
      _ => ()
    }

    for i in 0 .. W / 4 {
      emit(u8::from(&self.slice_be::<4,1>(extra + 4 * i)))
    }

    f.pad_integral(true, "0x", &s)

  }
}

impl<const W: usize, const L: usize> UpperHex for Word<W,L> {
  fn fmt(&self, f: &mut Formatter) -> Result {
    self.fmt_hex(f, ['0','1','2','3','4','5','6','7','8','9'
                    ,'A','B','C','D','E','F'])
  }
}

impl<const W: usize, const L: usize> LowerHex for Word<W,L> {
  fn fmt(&self, f: &mut Formatter) -> Result {
    self.fmt_hex(f, ['0','1','2','3','4','5','6','7','8','9'
                    ,'a','b','c','d','e','f'])
  }
}

impl<const W: usize, const L: usize> Octal for Word<W,L> {
  fn fmt(&self, f: &mut Formatter) -> Result {
    let mut s = String::new();
    let extra = W % 3;
    let table = ['0','1','2','3','4','5','6','7'];
    let mut emit = |x| s.push(table[ x as usize ]);

    match extra {
      1 => emit(u8::from(&self.slice_be::<1,1>(0))),
      2 => emit(u8::from(&self.slice_be::<2,1>(0))),
      _ => ()
    }

    for i in 0 .. W / 3 {
      emit(u8::from(&self.slice_be::<3,1>(extra + 3 * i)))
    }

    f.pad_integral(true, "0o", &s)
  }
}


/// Base 10.  Base 16 is probably more useful, but the hex traits
/// cover those.
impl<const W: usize, const L: usize> Display for Word<W,L> {
  fn fmt(&self, f: &mut Formatter) -> Result {
    let s = num::BigUint::from(self).to_string();
    f.pad_integral(true, "", &s)
  }
}







// -----------------------------------------------------------------------------
// Macros

#[macro_export]
/// A convenient way to write a `Word` type without having to specify
/// the number of limbs.
macro_rules! Word {

  // XXX: we can add extra cases here to handle small bit-vectors
  // (e.g., <= 32)

  ($w:expr) => { $crate::word::Word
                       < {$w}
                       , {$crate::word::limbs_for_bits($w)}
                       >
  };
}

#[macro_export]
macro_rules! append {
  ($FRONT:expr,$BACK:expr,$xs:expr,$ys:expr) => { {
    const L2: usize = $crate::word::limbs_for_bits($BACK);
    const W3: usize = $FRONT + $BACK;
    const L3: usize = $crate::word::limbs_for_bits(W3);
    $xs.append::<{$BACK}, L2, W3, L3>($ys)
  } }
}

#[macro_export]
macro_rules! join {
  ($PARTS:expr,$EACH:expr,$xs:expr) => { {
    const EACH_L: usize = $crate::word::limbs_for_bits($EACH);
    const OUT_W:  usize = $PARTS * $EACH;
    const OUT_L:  usize = $crate::word::limbs_for_bits(OUT_W);
    $crate::word
        ::Word::<OUT_W,OUT_L>::join::<{$EACH},EACH_L>($xs)
  } }
}

#[macro_export]
macro_rules! split {
  ($PARTS:expr,$EACH:expr,$xs:expr) => { {
    const IN_W: usize = $PARTS * $EACH;
    const IN_L: usize = $crate::word::limbs_for_bits(IN_W);
    let xs : &$crate::word::Word<IN_W, IN_L> = $xs;

    const EACH_L: usize = $crate::word::limbs_for_bits($EACH);
    xs.split::<{$EACH},EACH_L>()
  } }
}



#[macro_export]
macro_rules! take {
  ($FRONT:expr, $BACK: expr, $xs:expr) => { {
    let xs : &$crate::Word!({$FRONT + $BACK}) = $xs;
    const L : usize = $crate::word::limbs_for_bits($FRONT);
    xs.take::<{$FRONT},L>()
  } }
}


#[macro_export]
macro_rules! drop {
  ($FRONT: expr, $BACK: expr, $xs: expr) => { {
    let xs : &$crate::Word!({$FRONT + $BACK}) = $xs;
    const L : usize = $crate::word::limbs_for_bits($BACK);
    xs.drop::<{$BACK},L>()
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
    let x0  = <Word!(0)>::from(0_u64);

    let v1 : u64 = 0b1;
    let x1  = <Word!(1)>::from(v1);

    let v5 : u64 = 0b10001;
    let x5  = <Word!(5)>::from(v5);

    let v64 : u64 = (1 << 63) + 1;
    let x64 = <Word!(64)>::from(v64);



    // small small
    assert_eq!(append!(0,0,&x0,&x0), x0);
    assert_eq!(append!(0,1,&x0,&x1), x1);
    assert_eq!(append!(1,0,&x1,&x0), x1);
    assert_eq!(append!(5,1,&x5,&x1),   <Word!(6)>::from(0b100011_u64));
    assert_eq!(append!(1,5,&x1,&x5),   <Word!(6)>::from(0b110001_u64));

    let m = (((v1 as u128) << 64) | (v64 as u128)).to_biguint().unwrap();
    assert_eq!(append!(1,64,&x1,&x64), <Word!(65)>::from(&m));

    let m1 = (((v64 as u128) << 1) | (v1 as u128)).to_biguint().unwrap();
    assert_eq!(append!(64,1,&x64,&x1), <Word!(65)>::from(&m1));

  }

  #[test]
  fn test_join() {
    let v64 : u64 = (1 << 63) + 3;
    let x64 = <Word!(64)>::from(v64);

    let v128 = ((v64 as u128) << 64) | (v64 as u128);
    let arr  = [x64.clone(),x64].into_iter();
    assert_eq!(join!(2,64,arr), <Word!(128)>::from(v128));

    let x1 = <Word!(1)>::from(1_u64);
    let arr2  = [x1.clone(),x1.clone(),x1].into_iter();
    assert_eq!(join!(3,1,arr2)
              , <Word!(3)>::from(0b111_u64));

    let ans = u32::MAX as u128;
    let arr3 = [ <Word!(32)>::from(ans); 3 ].into_iter();
    assert_eq!( join!(3,32, arr3)
              , <Word!(96)>::from((ans << 64) | (ans << 32) | ans)
              );

    let arr4 = [ <Word!(63)>::from(0x0123456789abcdef_u64); 3 ].into_iter();
    let ans = "6974557483762978536120337476781526266153645112264740335"
            .parse::<BigUint>().unwrap();
    assert_eq!(join!(3,63, arr4), <Word!(189)>::from(&ans));

  }

  #[test]
  fn test_index() {
    let a = <Word!(3)>::from(0b111_u64);
    assert_eq!(a.index_front(0), true);

    let x = <Word!(127)>::from(0b110_u64);
    assert_eq!(x.index_back(0), false);
    assert_eq!(x.index_back(1), true);
    assert_eq!(x.index_back(2), true);
    assert_eq!(x.index_front(2), false);

    let y = <Word!(127)>::from(0b110_u128 << 124);
    assert_eq!(y.index_front(0), true);
    assert_eq!(y.index_front(1), true);
    assert_eq!(y.index_front(2), false);
    assert_eq!(y.index_back(2), false);
  }


  #[test]
  fn test_traverse_bits() {
    let x = <Word!(3)>::from(0b111_u64);
    let mut count = 0;
    for i in x.into_iter() {
      count += 1;
      assert_eq!(i,true);
    }
    assert_eq!(count,3);
  }

  #[test]
  fn test_split() {
    let x = <Word!(64)>::from(0x_00_01_02_03_04_05_06_07_u64);
    assert_eq!( split!(16,4,&x)
                .collect::<Vec<Word!(4)>>()
              , vec![0_u64,0,0,1,0,2,0,3,0,4,0,5,0,6,0,7]
                .into_iter()
                .map(|v| <Word!(4)>::from(v))
                .collect::<Vec<Word!(4)>>()
                );
  }


  #[test]
  fn test_slice() {
    let x64 = <Word!(64)>::from(1_u64 << 63);
    assert_eq!(x64.slice_be(0), <Word!(1)>::from(1_u64));
    assert_eq!(x64.slice_be(1), <Word!(1)>::from(0_u64));
    assert_eq!(x64.slice_be(0), <Word!(2)>::from(10_u64));
    assert_eq!(x64.slice_be(1), <Word!(2)>::from(00_u64));
    assert_eq!(x64.slice_be(0), x64);
    assert_eq!(x64.slice_le(0), <Word!(1)>::from(0_u64));
    assert_eq!(x64.slice_le(63), <Word!(1)>::from(1_u64));
    assert_eq!(x64.slice_le(0), x64);

    let x123 = <Word!(123)>::from(1_u128 << 122);
    assert_eq!(x123.slice_be(0), <Word!(1)>::from(1_u64));
    assert_eq!(x123.slice_be(1), <Word!(1)>::from(0_u64));
    assert_eq!(x123.slice_be(0), <Word!(2)>::from(10_u64));
    assert_eq!(x123.slice_be(1), <Word!(2)>::from(00_u64));
    assert_eq!(x123.slice_be(0), x123);

    let x3 = <Word!(3)>::from(1_u64 << 2);
    assert_eq!(x3.slice_be(0), <Word!(1)>::from(1_u64));
    assert_eq!(x3.slice_be(1), <Word!(1)>::from(0_u64));
    assert_eq!(x3.slice_be(0), <Word!(2)>::from(10_u64));
    assert_eq!(x3.slice_be(1), <Word!(2)>::from(00_u64));
    assert_eq!(x3.slice_be(0), x3);
  }


  #[test]
  fn test_take_drop() {
    let x = <Word!(4)>::from(0b101_u64);
    assert_eq!(take!(2,2,&x), <Word!(2)>::from(0b01_u64));
    assert_eq!(drop!(2,2,&x), <Word!(2)>::from(0b01_u64));
    assert_eq!(drop!(1,3,&x), <Word!(3)>::from(0b101_u64));
  }

  #[test]
  fn test_from() {
    assert_eq!( <Word!(8)>::from(&<num::BigInt>::from(255_u64))
              , <Word!(8)>::from(255_u64)
              );
    assert_eq!( <Word!(8)>::from(&<num::BigInt>::from(-1))
              , <Word!(8)>::from(255_u64)
              );

    assert_eq!( <Word!(128)>::from(&<num::BigInt>::from(-1))
              , <Word!(128)>::from(&<num::BigUint>::from_slice(
                                                        &[ 0xFFFFFFFF; 4 ]))
              );
  }

  #[test]
  fn test_exp() {
    assert_eq!(<Word!(8)>::from(2_u64).exp(6), <Word!(8)>::from(64_u64));
    assert_eq!(<Word!(8)>::from(2_u64).exp(7), <Word!(8)>::from(128_u64));
    assert_eq!(<Word!(7)>::from(2_u64).exp(7), <Word!(7)>::from(0_u64));
    let x_7_129 = &<Word!(129)>::from(7_u64);
    assert_eq!(x_7_129.exp(11), &x_7_129.exp(3) * &x_7_129.exp(8));
  }

}





