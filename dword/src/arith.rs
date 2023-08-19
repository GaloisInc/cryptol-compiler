use crate::{DWord,DWordRef};
use crate::core::LimbT;


impl std::ops::AddAssign<DWordRef<'_>> for DWord {
  fn add_assign(&mut self, rhs: DWordRef<'_>) {
    let mut acc = 0 as u128;
    for (out,&limb) in self.as_slice_mut().iter_mut()
                           .zip(rhs.iter_limbs_le()) {
      acc += *out as u128;
      acc += limb as u128;
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

    let mut acc = 1 as u128;
    for (out,&limb) in result.as_slice_mut().iter_mut()
                             .zip(self.iter_limbs_le()) {
      acc += (!limb) as u128;
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
