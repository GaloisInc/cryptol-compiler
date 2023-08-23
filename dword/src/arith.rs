use crate::{DWord,DWordRef};
use crate::core::{LimbT, BigLimbT};


impl std::ops::AddAssign<DWordRef<'_>> for DWord {
  fn add_assign(&mut self, rhs: DWordRef<'_>) {
    assert_eq!(self.bits(), rhs.bits());

    let mut acc: BigLimbT = 0;
    for (out,&limb) in self.as_slice_mut().iter_mut()
                           .zip(rhs.iter_limbs_lsb()) {
      acc += *out as BigLimbT;
      acc += limb as BigLimbT;
      *out = acc as LimbT;
      acc  = acc >> DWord::LIMB_BITS;
    }
  }
}

impl std::ops::Add<DWordRef<'_>> for DWordRef<'_> {
  type Output = DWord;
  fn add(self, other: DWordRef<'_>) -> Self::Output {
    let mut result = self.clone_word();
    result += other;
    result
  }
}

impl std::ops::Neg for DWordRef<'_> {
  type Output = DWord;

  fn neg(self) -> Self::Output {
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
  fn sub_assign(&mut self, other: DWordRef<'_>) {
    *self += (-other).as_ref()
  }
}

impl std::ops::Sub <DWordRef<'_>> for DWordRef<'_> {
  type Output = DWord;
  fn sub(self, other: DWordRef<'_>) -> Self::Output {
    let mut result = -other;
    result += self;
    result
  }
}

impl std::ops::Mul <DWordRef<'_>> for DWordRef<'_> {
  type Output = DWord;

  fn mul(self, other: DWordRef<'_>) -> Self::Output {
    assert_eq!(self.bits(), other.bits());

    if self.bits() == 0 { return DWord::zero(0) }

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
