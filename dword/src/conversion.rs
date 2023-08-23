use crate::{DWord,DWordRef};
use crate::core::{LimbT, BigLimbT};

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

impl DWordRef<'_> {

  /// Convert to a vector of digits in base 2^32.
  /// Least significant digit first.
  /// Convenient for conversion to bignum.
  pub fn as_vec_u32(self) -> Vec<u32> {
    let mut result = Vec::<u32>::with_capacity(self.limbs() / 2);
    if self.bits() == 0 { return result }


    let ws  = self.as_slice();
    let pad = self.padding();

    // we assume that DWord::LIMB_BITS >= 32
    let mut w    = (ws[0] >> pad) as BigLimbT;
    let mut have = self.not_padding();

    while have >= 32 {
      result.push(w as u32);
      w = w >> 32;
      have -= 32;
    };

    for v in &ws[1..] {
      w = ((*v as BigLimbT) << have) | w;
      have += DWord::LIMB_BITS;
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


