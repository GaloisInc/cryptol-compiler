use crate::DWord;
use crate::core::{Limb,LimbT};

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

    result.as_slice_mut()[0] = value << result.padding();
    if bits > Limb::BITS {
      result.as_slice_mut()[1] = (value >> result.last_used_bits()) as LimbT;
    }
    result
  }

}


/// Get the least significant bits
impl From<&DWord> for u8 {
  fn from(x: &DWord) -> Self {
    let pad   = x.padding();
    let have  = x.last_used_bits();
    let ws    = x.as_slice();
    let mut w = ws[0] >> pad;
    if x.bits() >= 8 && have < 8 {
      w |= ws[1] << have;
    }
    w as u8
  }
}


