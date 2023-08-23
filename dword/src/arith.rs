use crate::{DWord,DWordRef};
use crate::core::{LimbT, BigLimbT};


impl std::ops::AddAssign<DWordRef<'_>> for DWord {
  fn add_assign(&mut self, rhs: DWordRef<'_>) {
    assert_eq!(self.bits(), rhs.bits());

    if self.is_small() {
      let x = self.as_ref().limb0();
      let y = rhs.limb0();
      self.as_slice_mut()[0] = x.wrapping_add(y);
      return
    }

    let ws = self.as_slice_mut();
    let mut acc: BigLimbT = 0;
    for (out,&limb) in self.as_slice_mut()
                           .iter_mut().zip(rhs.iter_limbs_lsb()) {
      acc += *out as BigLimbT;
      acc += limb as BigLimbT;
      *out = acc as LimbT;
      acc  = acc >> DWord::LIMB_BITS;
    }
  }
}

impl std::ops::Add<DWordRef<'_>> for DWordRef<'_> {
  type Output = DWord;

  #[inline(always)]
  fn add(self, other: DWordRef<'_>) -> Self::Output {
    let mut result = self.clone_word();
    result += other;
    result
  }
}

impl std::ops::Neg for DWordRef<'_> {
  type Output = DWord;

  fn neg(self) -> Self::Output {
    if self.is_small() {
      return DWord::small_from_limb(self.bits(), self.limb0().wrapping_neg())
    }

    let mut result = DWord::zero(self.bits());

    let mut acc: BigLimbT = 1;
    for (out,&limb) in result.as_slice_mut().iter_mut()
                             .zip(self.iter_limbs_lsb()) {
      acc += (!limb) as BigLimbT;
      *out = acc as LimbT;
      acc  = acc >> DWord::LIMB_BITS;
    }
    result
  }
}

impl std::ops::SubAssign <DWordRef<'_>> for DWord {

  #[inline(always)]
  fn sub_assign(&mut self, other: DWordRef<'_>) {
    *self += (-other).as_ref()
  }
}

impl std::ops::Sub <DWordRef<'_>> for DWordRef<'_> {
  type Output = DWord;

  #[inline(always)]
  fn sub(self, other: DWordRef<'_>) -> Self::Output {
    let mut result = -other;
    result += self;
    result
  }
}

impl std::ops::Mul <DWordRef<'_>> for DWordRef<'_> {
  type Output = DWord;

  fn mul(self, other: DWordRef<'_>) -> Self::Output {
    let bits = self.bits();
    assert_eq!(bits, other.bits());

    if self.is_small() {
      let x = self.limb0();
      let y = other.limb0_norm();
      return DWord::small_from_limb(bits, x.wrapping_mul(y))
    }

    let mut clone: DWord;
    let padding = self.padding();
    let r =
      if padding > 0 {
        clone = self.clone_word();
        clone.shift_bits_right(padding);
        clone.as_ref()
      } else {
        self
      };
    let ws1 = r.as_slice();
    let ws2 = other.as_slice();
    let mut out = Vec::with_capacity(self.limbs());

    let tot = ws1.len();
    let mut acc: BigLimbT = 0;
    for i in 0 .. tot {
      let mut digit = acc;
      acc = 0;
      for j in 0 ..= i {
        let term = (ws1[j] as BigLimbT) * (ws2[i-j] as BigLimbT);
        digit += (term as LimbT) as BigLimbT;
        acc   += term >> DWord::LIMB_BITS;
      }
      out.push(digit as LimbT);
      acc += digit >> DWord::LIMB_BITS;
    }

    DWord::from_limbs(self.bits(), out)
  }
}


impl std::ops::Div <DWordRef<'_>> for DWordRef<'_> {
  type Output = DWord;
  fn div(self, rhs: DWordRef<'_>) -> Self::Output {
    let bits = self.bits();
    assert_eq!(bits, rhs.bits());

    if self.is_small() {
      let x = self.limb0();
      let y = rhs.limb0_norm();
      let mut result = DWord::small_from_limb(bits, x / y);
      result.fix_underflow();
      return result
    }

    let x: num::BigUint = self.into();
    let y: num::BigUint = rhs.into();
    DWord::from_uint(bits, &(x / y))
  }
}

impl std::ops::Rem <DWordRef<'_>> for DWordRef<'_> {
  type Output = DWord;
  fn rem(self, rhs: DWordRef<'_>) -> Self::Output {
    let bits = self.bits();
    assert_eq!(bits, rhs.bits());

    if self.is_small() {
      let x = self.limb0_norm();
      let y = self.limb0_norm();
      return DWord::from_u64(bits, x % y);
    }

    let x: num::BigUint = self.into();
    let y: num::BigUint = self.into();
    DWord::from_uint(bits, &(x % y))
  }
}


impl DWordRef<'_> {

  /// Raise the number to the given power.
  pub fn pow(self, exp: u32) -> DWord {
    let bits = self.bits();

    if self.is_small() {
      let x = self.limb0_norm();
      return DWord::from_u64(bits, x.wrapping_pow(exp))
    }

    let x: num::BigUint = self.into();
    DWord::from_uint(bits, &(x.pow(exp)))

  }

}


#[cfg(test)]
mod test {
  use crate::DWord;

  #[test]
  fn test_add() {
    assert!( DWord::from_u64(7,1).as_ref() + DWord::from_u64(7,2).as_ref()
              == DWord::from_u64(7,3)
              );
    assert!( DWord::from_u64(7,127).as_ref() + DWord::from_u64(7,2).as_ref()
              == DWord::from_u64(7,1)
              );

    assert!(-DWord::from_u64(7,1).as_ref() == DWord::from_u64(7,127))



  }

}
