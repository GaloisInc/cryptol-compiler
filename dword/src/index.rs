use crate::DWord;
use crate::core::Limb;

impl DWord {

  pub fn index_lsb(&self, i: usize) -> bool {
    assert!(i <= self.bits());
    let data = self.as_slice();
    let ix   = i + self.padding();
    let w    = data[ix / Limb::BITS];
    w & (1 << (ix % Limb::BITS)) != 0
  }

  pub fn index_msb(&self, i: usize) -> bool {
    assert!(i <= self.bits());
    self.index_lsb(self.bits() - i - 1)
  }

  pub fn iter_msb<'a> (&'a self) -> TraverseBits<'a> {
    TraverseBits { vec: self, ix: 0 }
  }

  /// Extract a sub-bitvector of the given length, starting at the given
  /// position. 0 is the most significant bit.
  pub fn slice_be(&self, sub_bits: usize, i: usize) -> DWord {
    assert!((i + sub_bits) <= self.bits());
    let ws = self.as_slice();
    let mut result = DWord::zero(sub_bits);
    let res_limbs = result.limbs();

    // Slice 0
    if self.bits() == 0 { return result }

    let out   = result.as_slice_mut();
    let start = self.limbs() - i / Limb::BITS;  // 1 bigger than where we read
    let off   = i % Limb::BITS;

    if off == 0 {
      // Slice aligned
      out.copy_from_slice(&ws[(start - res_limbs) .. start]);

    } else {
      // Slice unaligned
      let mut prev = ws[start - 1] << off;
      let other = Limb::BITS - off;
      for j in 1 .. res_limbs + 1 {
        let w = ws[start - j - 1];
        out[res_limbs - j] = prev | (w >> other);
        prev = w << off;
      }

    }

    result.fix_underflow();
    result
  }


}


/// Traverse DWord as bits, starting from most significant.
pub struct TraverseBits<'a> {
  vec: &'a DWord,
  ix:  usize
}


impl<'a> Iterator for TraverseBits<'a> {
  type Item = bool;
  fn next(&mut self) -> Option<Self::Item> {
    if self.ix >= self.vec.bits() {
      None
    } else {
      let i = self.ix;
      self.ix += 1;
      Some(self.vec.index_msb(i))
    }
  }
}






#[cfg(test)]
mod test {
  use crate::DWord;

  #[test]
  fn test_index() {
    assert_eq!(DWord::from_u64(64,1).index_lsb(0), true);
    assert_eq!(DWord::from_u64(64,1).index_lsb(1), false);
    assert_eq!(DWord::from_u64(65,1).index_lsb(0), true);
    assert_eq!(DWord::from_u64(65,1).index_lsb(1), false);
  }
}
