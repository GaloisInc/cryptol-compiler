use crate::{DWord,DWordRef};
use crate::core::{LimbT, BigLimbT,limbs_for_size};

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

  pub fn from_uint(bits: usize, value: &num::BigUint) -> DWord {
    let limb_num = limbs_for_size(bits);
    let mut limbs = Vec::<LimbT>::with_capacity(limb_num);
    limbs.extend(value.iter_u64_digits()
                      .chain(std::iter::repeat(0))
                      .take(limb_num));
    let mut result = DWord::from_limbs(bits, limbs);
    let pad = result.padding();
    if pad > 0 {
      result.shift_bits_left(pad);
    }
    result
  }

  pub fn from_int(bits: usize, n: &num::BigInt) -> DWord {
    let limb_num = limbs_for_size(bits);
    let mut limbs  = Vec::<LimbT>::with_capacity(limb_num);

    // Use this to extend in the most significant limbs.
    let ext: u8    = match n.sign() {
                       num::bigint::Sign::Minus => 255,
                       _ => 0
                     };

    let mut bytes  = n.to_signed_bytes_le().into_iter();
    for _ in 0 .. limb_num {
      let mut w: LimbT = 0;
      for z in 0 .. DWord::LIMB_BYTES {
        let b = match bytes.next() {
                  None    => ext,
                  Some(x) => x
                };
        w |= (b as LimbT) << (8 * z);
      }
      limbs.push(w);
    }
    let mut result = DWord::from_limbs(bits, limbs);
    let pad = result.padding();
    if pad > 0 { result.shift_bits_left(pad) }
    result
  }

}




// -----------------------------------------------------------------------------
// Export

// To u8
/// Get the 8 least significant bits.
impl From<DWordRef<'_>> for u8 {
  fn from(x: DWordRef<'_>) -> Self {
    let have  = x.not_padding();
    let ws    = x.as_slice();
    let mut w = ws[0] >> x.padding();
    if x.bits() >= 8 && have < 8 {
      w |= ws[1] << have;
    }
    w as u8
  }
}


impl DWordRef<'_> {

  /// Convert to a vector of digits in base 2^32.
  /// Least significant digit first.
  /// Convenient for conversion to bignum.
  pub fn as_vec_u32(self) -> Vec<u32> {
    let mut result = Vec::<u32>::with_capacity(self.limbs() * 2);
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


// To num::BigUint
impl From<DWordRef<'_>> for num::BigUint {
  fn from(x: DWordRef<'_>) -> Self {
    Self::new(x.as_vec_u32())
  }
}

// To num::BigInt
impl From<DWordRef<'_>> for num::BigInt {
  fn from(x: DWordRef<'_>) -> Self { <_>::from(num::BigUint::from(x)) }
}



