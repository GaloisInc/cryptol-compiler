use std::marker::PhantomData;
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

  /// Extract a sub-word starting at the given bit offset.
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

    let mut cur = ws.len() - i / DWord::LIMB_BITS; // 1 bigger than read
    let off     = i % DWord::LIMB_BITS;

    if off == 0 {
      // Slice aligned
      out.copy_from_slice(&ws[(cur - res_limbs) .. cur]);

    } else {
      // Slice unaligned

      cur           -= 1;
      let mut w      = ws[cur] << off;
      let have_bits  = DWord::LIMB_BITS - off;
      for j in 1 .. res_limbs {
        cur -= 1;
        let w1 = ws[cur];
        out[res_limbs - j] = w | (w1 >> have_bits);
        w = w1 << off;
      }

      if cur > 0 {
        w = w | (ws[cur - 1] >> have_bits)
      }
      out[0] = w;
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
  use crate::{DWord,FromLSB,FromMSB,IndexDir,IndexFrom};
  use crate::proptest::*;

  #[test]
  fn index_lsb() {
    do_test(word_and::<usize>, |(x,i0) : (DWord,usize) | {
      if x.bits() == 0 { return Some(true) }
      let i = i0 % x.bits();
      let (xr,a) = x.sem();
      return Some(xr.index::<FromLSB>(i) == a.bit(i as u64))
    })
  }

  #[test]
  fn index_msb() {
    do_test(word_and::<usize>, |(x,i0) : (DWord,usize) | {
      if x.bits() == 0 { return Some(true) }
      let i = i0 % x.bits();
      let j = (x.bits() - 1 - i) as u64;
      let (xr,a) = x.sem();
      return Some(xr.index::<FromMSB>(i) == a.bit(j))
    })
  }

  fn sub_word<INDEX: IndexDir>() {
    do_test(word_and2::<usize,usize>, |(x,i0,w0): (DWord,usize,usize)|{
      let i      = i0 % (x.bits() + 1);
      let have   = x.bits() - i;
      let w      = w0 % (have + 1);
      let (xr,a) = x.sem();

      let lhs    = xr.sub_word::<INDEX>(w,i);
      let amt    = match INDEX::DIR {
                    IndexFrom::Msb => x.bits() - i - w,
                    IndexFrom::Lsb => i
                  };
      Some(lhs == DWord::from_uint(w, &(a >> amt)))
    })
  }

  #[test]
  fn sub_word_lsb() { sub_word::<FromLSB>() }
  #[test]
  fn sub_word_msb() { sub_word::<FromMSB>() }

}
