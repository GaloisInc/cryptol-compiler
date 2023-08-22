use std::marker::PhantomData;
use crate::core::LimbT;
use crate::{DWord,DWordRef};

/// Specify from which side of a word are we indexing.
pub enum IndexFrom {
  /// Index from the last signficatn side of the word.
  Lsb,

  /// Index from the most signficatn side of the word.
  Msb
}


/// Direction for indexing.
pub trait IndexDir {
  const DIR: IndexFrom;
}

/// Index from the last signficatn side of the word.
pub struct FromLSB;

/// Index from the most signficatn side of the word.
pub struct FromMSB;

impl IndexDir for FromLSB { const DIR: IndexFrom = IndexFrom::Lsb; }
impl IndexDir for FromMSB { const DIR: IndexFrom = IndexFrom::Msb; }




impl<'a> DWordRef<'a> {

  /// Extract a bit at the given index.
  pub fn index<INDEX: IndexDir>(self, index: usize) -> bool {
    assert!(index <= self.bits());

    // Turn into LSB index
    let i = match INDEX::DIR {
              IndexFrom::Msb => self.bits() - index - 1,
              IndexFrom::Lsb => index
            };

    let data = self.as_slice();
    let ix   = i + self.padding();
    let w    = data[ix / DWord::LIMB_BITS];
    w & (1 << (ix % DWord::LIMB_BITS)) != 0
  }

  /// Iterate over the bits.
  pub fn iter<INDEX: IndexDir>(self) -> TraverseBits<'a, INDEX> {
    TraverseBits { dir: PhantomData, vec: self, ix: 0 }
  }

  /// Iterate over the limbs, starting with the least signficant one.
  pub fn iter_limbs_lsb<'b>(&'b self) -> std::slice::Iter<'b,u64> {
    self.as_slice().iter()
  }

  /// Iterate over the limbs, starting with the most signficant one.
  pub fn iter_limbs_msb<'b>
    (&'b self) -> std::iter::Rev<std::slice::Iter<'b,u64>> {
    self.as_slice().iter().rev()
  }


  /// XXX: Maybe we don't need this
  /// Get a limb-sized sub-bitvector starting at the given bit offset.
  /// * The offset need not be aligned.
  /// * If the resulting bitvector is smaller than a limb,
  ///   the result is 0 extend on the side away from the direction
  ///   (i.e., [FromMSB] extends on LSB side, and [FromLSB] extends on
  ///   the MSB side)
  /// * The index refers to the first bit on the resepctive side
  ///   (i.e., [FromMSB] points to the most significant bit in the result
  ///   and cound from the most significant end of the word)
  pub fn get_limb_unaligned<INDEX: IndexDir>(self, index: usize) -> LimbT {
    if index >= self.bits() { return 0 }

    let ws  = self.as_slice();
    match INDEX::DIR {

       IndexFrom::Lsb => {
         let i      = index + self.padding();
         let w_off  = i / DWord::LIMB_BITS;
         let extra  = i % DWord::LIMB_BITS;
         let mut lower = ws[w_off];
         if extra == 0 { return lower }     // alligned

         let other  = DWord::LIMB_BITS - extra;
         lower = lower >> extra;
         if w_off + 1 == ws.len() { return lower }

         (ws[w_off + 1] << other) | lower
       },

       IndexFrom::Msb => {
          let w_off     = ws.len() - index / DWord::LIMB_BITS - 1;
          let extra     = index % DWord::LIMB_BITS;
          let mut upper = ws[w_off];
          if extra      == 0 { return upper }          // alligned

          let other = DWord::LIMB_BITS - extra;
          upper = upper << extra;
          if w_off == 0 { return upper }

          upper | (ws[w_off - 1] >> other)
       }
    }
  }

  /// Extract a sub-bitvector of the given length,
  /// starting at the given bit offset.
  pub fn sub_word<INDEX: IndexDir>
    (self, sub_bits: usize, index: usize) -> DWord {
    assert!(index + sub_bits <= self.bits());

    // Make into MSB index
    let i = match INDEX::DIR {
              IndexFrom::Msb => index,
              IndexFrom::Lsb => self.bits() - sub_bits - index
            };
    let ws = self.as_slice();
    let mut result = DWord::zero(sub_bits);

    // Slice 0
    if sub_bits == 0 { return result }

    let out       = result.as_slice_mut();
    let res_limbs = out.len();

    let start = ws.len() - i / DWord::LIMB_BITS; // 1 bigger than read
    let off   = i % DWord::LIMB_BITS;

    if off == 0 {
      // Slice aligned
      out.copy_from_slice(&ws[(start - res_limbs) .. start]);

    } else {
      for j in 0 .. res_limbs {
        out[res_limbs - 1 - j] =
          self.get_limb_unaligned::<FromMSB>(i - j * DWord::LIMB_BITS)
      }
    }

    result.fix_underflow();
    result
  }

}


/// Traverse DWord as bits, starting from most significant.
pub struct TraverseBits<'a, INDEX: IndexDir> {
  dir : PhantomData<INDEX>,
  vec: DWordRef<'a>,
  ix:  usize
}


impl<'a, INDEX: IndexDir> Iterator for TraverseBits<'a, INDEX> {
  type Item = bool;
  fn next(&mut self) -> Option<Self::Item> {
    if self.ix >= self.vec.bits() {
      None
    } else {
      let i = self.ix;
      self.ix += 1;
      Some(self.vec.index::<INDEX>(i))
    }
  }
}





#[cfg(test)]
mod test {
  use crate::{DWord,FromLSB,FromMSB};

  #[test]
  fn test_index() {
    let x64v = DWord::from_u64(64,1);
    let x65v = DWord::from_u64(65,1);
    let x64  = x64v.as_ref();
    let x65  = x65v.as_ref();

    assert_eq!(x64.index::<FromMSB>(0), false);
    assert_eq!(x64.index::<FromLSB>(0), true);
    assert_eq!(x64.index::<FromLSB>(1), false);
    assert_eq!(x65.index::<FromLSB>(0), true);
    assert_eq!(x65.index::<FromLSB>(1), false);
  }
}
