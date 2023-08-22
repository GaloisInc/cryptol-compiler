use crate::{DWord, DWordRef, IndexDir, IndexFrom, FromLSB, FromMSB};
use crate::core::limbs_for_size;

/*
impl DWordRef<'_> {
  pub fn shift_toward<INDEX: IndexDir>(self, amt: usize) -> DWord {
    if amt == 0 { return self.clone_word() }

    let mut result = DWord::zero(self.bits());
    if amt >= self.bits() { return result }

    let todo_bits  = self.bits() - amt;
    let todo_limbs = limbs_for_size(todo_bits);
    let ws = result.as_slice_mut();

    match INDEX::DIR {

      IndexFrom::Msb => {
        let last = self.limbs() - 1;
        for i in 0 .. todo_limbs {
          ws[last - i] = self.get_limb::<FromMSB>(amt + i * DWord::LIMB_BITS);
        }
      },

      IndexFrom::Lsb => {
        let last = todo_limbs - 1;
        let bit_ix = last * DWord::LIMB_BITS - self.padding();
        for i in 0 .. todo_limbs {
          ws[last - i] =
             self.get_limb::<FromLSB>(amt + bit_ix - i * DWord::LIMB_BITS);
        }
      }
    }

    result
  }
}

impl std::ops::Shl<usize> for DWordRef<'_> {
  type Output = DWord;

  fn shl(self, amt: usize) -> Self::Output {
    self.shift_toward::<FromMSB>(amt)
  }
}


impl std::ops::Shr<usize> for DWordRef<'_> {
  type Output = DWord;

  fn shr(self, amt: usize) -> Self::Output {
    self.shift_toward::<FromLSB>(amt)
  }

}

*/
