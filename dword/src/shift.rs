use crate::{DWord, DWordRef};
use crate::core::LimbT;

impl DWord {

  /// Shift by whole limbs. `amt` is in units of limbs.
  pub fn shift_limbs_left(&mut self, amt: usize) {
    let ws   = self.as_slice_mut();
    assert!(amt < ws.len());

    let tot  = ws.len();
    for i in amt .. tot {
      ws[i] = ws[i - amt];
    }
    for w in &mut ws[0 .. amt] { *w = 0 }
  }

  /// Shift by whole limbs.  `amt` is in units of limbs.
  /// Does not fix underflow.
  pub fn shift_limbs_right(&mut self, amt: usize) {
    let ws   = self.as_slice_mut();
    assert!(amt < ws.len());

    let tot  = ws.len();
    let todo = tot - amt;
    for i in 0 .. todo {
      ws[i] = ws[i + amt];
    }
    for w in &mut ws[todo .. tot] { *w = 0 }
  }


  /// Shift by less than a limb.
  pub fn shift_bits_left(&mut self, amt: usize) {
    assert!(amt < DWord::LIMB_BITS);

    let other = DWord::LIMB_BITS - amt;
    let mut acc : LimbT = 0;
    for w in self.as_slice_mut() {
      let x = *w;
      *w = (x << amt) | acc;
      acc = x >> other;
    }
  }

  /// Shift by less than a limb. Does not fix underflow.
  pub fn shift_bits_right(&mut self, amt: usize) {
    assert!(amt < DWord::LIMB_BITS);

    let other = DWord::LIMB_BITS - amt;
    let mut acc : LimbT = 0;
    for w in self.as_slice_mut().into_iter().rev() {
      let x = *w;
      *w = acc | (x >> amt);
      acc = x << other;
    }
  }

}


impl std::ops::ShlAssign<usize> for DWord {

  fn shl_assign(&mut self, amt: usize) {
    if amt >= self.bits() {
      self.assign_zero();
      return
    }
    if amt == 0 { return }

    let limbs = amt / DWord::LIMB_BITS;
    let extra = amt % DWord::LIMB_BITS;
    self.shift_limbs_left(limbs);
    if extra != 0 { self.shift_bits_left(extra) }
  }
}

impl std::ops::ShrAssign<usize> for DWord {

  fn shr_assign(&mut self, amt: usize) {
    if amt >= self.bits() {
      self.assign_zero();
      return
    }
    if amt == 0 { return }

    let limbs = amt / DWord::LIMB_BITS;
    let extra = amt % DWord::LIMB_BITS;
    self.shift_limbs_right(limbs);
    if extra != 0 { self.shift_bits_right(extra) }
    self.fix_underflow();
  }
}

impl std::ops::Shl<usize> for DWordRef<'_> {
  type Output = DWord;

  fn shl(self, amt: usize) -> Self::Output {
    let mut result = self.clone_word();
    result <<= amt;
    result
  }
}


impl std::ops::Shr<usize> for DWordRef<'_> {
  type Output = DWord;

  fn shr(self, amt: usize) -> Self::Output {
    let mut result = self.clone_word();
    result >>= amt;
    result
  }

}
