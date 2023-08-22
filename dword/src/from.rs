use crate::{DWord,DWordRef};
use crate::core::{LimbT};

impl DWord {

  pub fn from_u8(bits: usize, value: u8) -> DWord {
    Self::from_u64(bits, value as u64)
  }

  pub fn from_u16(bits: usize, value: u16) -> DWord {
    Self::from_u64(bits, value as u64)
  }

  pub fn from_u32(bits: usize, value: u32) -> DWord {
    Self::from_u64(bits, value as u64)
  }

  pub fn from_u64(bits: usize, value: u64) -> DWord {
    let mut result = DWord::zero(bits);
    if bits == 0 { return result }


    if result.padding() > 0 {
      result.as_slice_mut()[0] = value << result.padding();
      if bits > DWord::LIMB_BITS {
        result.as_slice_mut()[1] = (value >> result.not_padding()) as LimbT;
      }
    } else {
      result.as_slice_mut()[0] = value;
    }
    result
  }

}

/// Get the 8 least significant bits.
impl<'a> From<DWordRef<'a>> for u8 {
  fn from(x: DWordRef<'a>) -> Self {
    let pad   = x.padding();
    let have  = x.not_padding();
    let ws    = x.as_slice();
    let mut w = ws[0] >> pad;
    if x.bits() >= 8 && have < 8 {
      w |= ws[1] << have;
    }
    w as u8
  }
}


