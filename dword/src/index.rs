use crate::{DWord,DWordRef};

impl<'a> DWordRef<'a> {

  /// Get a bit, starting at the most signifcant end of the word.
  pub fn index_be(self, i: usize) -> bool {
    assert!(i <= self.bits());
    self.index_le(self.bits() - i - 1)
  }

  /// Get a bit, starting at the least signifcant end of the word.
  pub fn index_le(self, i: usize) -> bool {
    assert!(i <= self.bits());
    let data = self.as_slice();
    let ix   = i + self.padding();
    let w    = data[ix / DWord::LIMB_BITS];
    w & (1 << (ix % DWord::LIMB_BITS)) != 0
  }

  /// Iterate over the bits, from big end to little end.
  pub fn iter_be(self) -> TraverseBits<'a, true> {
    TraverseBits { vec: self, ix: 0 }
  }

  /// Iterate over the bits, from little end to bit end.
  pub fn iter_le(self) -> TraverseBits<'a, false> {
    TraverseBits { vec: self, ix: 0 }
  }



  /// Extract a sub-bitvector of the given length, starting at the given
  /// bit position. 0 is the most significant bit.
  pub fn slice_be(self, sub_bits: usize, i: usize) -> DWord {

    assert!((i + sub_bits) <= self.bits());
    let ws = self.as_slice();
    let mut result = DWord::zero(sub_bits);

    // Slice 0
    if self.bits() == 0 { return result }

    let out       = result.as_slice_mut();
    let res_limbs = out.len();

    let start     = ws.len() - i / DWord::LIMB_BITS;
                                                 // 1 bigger than where we read
    let off       = i % DWord::LIMB_BITS;

    if off == 0 {
      // Slice aligned
      out.copy_from_slice(&ws[(start - res_limbs) .. start]);

    } else {
      // Slice unaligned
      let mut prev = ws[start - 1] << off;
      let other = DWord::LIMB_BITS - off;
      for j in 1 .. res_limbs + 1 {
        let w = ws[start - j - 1];
        out[res_limbs - j] = prev | (w >> other);
        prev = w << off;
      }

    }

    result.fix_underflow();
    result
  }

  /// Extract a sub-bitvector of the given length, starting at the given
  /// bit position. 0 is the most significant bit.
  pub fn slice_le(self, sub_bits: usize, i: usize) -> DWord {
    assert!((i + sub_bits) <= self.bits());
    self.slice_be(sub_bits, self.bits() - sub_bits - i)
  }




}

/// Traverse DWord as bits, starting from most significant.
pub struct TraverseBits<'a, const BE: bool> {
  vec: DWordRef<'a>,
  ix:  usize
}


impl<'a, const BE: bool> Iterator for TraverseBits<'a, BE> {
  type Item = bool;
  fn next(&mut self) -> Option<Self::Item> {
    if self.ix >= self.vec.bits() {
      None
    } else {
      let i = self.ix;
      self.ix += 1;
      Some(if BE { self.vec.index_be(i) } else { self.vec.index_le(i) } )
    }
  }
}






#[cfg(test)]
mod test {
  use crate::DWord;

  #[test]
  fn test_index() {
    let x64v = DWord::from_u64(64,1);
    let x65v = DWord::from_u64(65,1);
    let x64  = x64v.as_ref();
    let x65  = x65v.as_ref();

    assert_eq!(x64.index_le(0), true);
    assert_eq!(x64.index_le(1), false);
    assert_eq!(x65.index_le(0), true);
    assert_eq!(x65.index_le(1), false);
  }
}
