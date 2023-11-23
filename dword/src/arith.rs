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
        clone.shift_bits_right(padding,false);
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
      let y = rhs.limb0_norm();
      return DWord::from_u64(bits, x % y);
    }

    let x: num::BigUint = self.into();
    let y: num::BigUint = rhs.into();
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
    let mut lim: num::BigUint = num::zero();
    lim.set_bit(self.bits() as u64, true);
    DWord::from_uint(bits, &(x.modpow(&(exp.into()),&lim)))

  }

}


#[cfg(test)]
pub mod test {
  use crate::{DWord};
  use crate::proptest::*;

  #[test]
  fn add() {
    do_test(binary, |(x,y) : (DWord,DWord)| {
      let (xr,a) = x.sem();
      let (yr,b) = y.sem();
      Some(xr + yr == DWord::from_uint(x.bits(), &(&a + &b)))
    })
  }

  #[test]
  pub fn neg() {
    do_test(unary, |x: DWord| {
      let (xr,a) = x.sem();
      let lim = num::BigUint::from(2_u64).pow(x.bits() as u32);
      let expect = (&lim - &a) % &lim;
      Some(-xr == DWord::from_uint(x.bits(), &expect))
    })
  }

  #[test]
  fn sub() {
    do_test(binary, |(x,y) : (DWord,DWord)| {
      let (xr,a) = x.sem();
      let (yr,b) = y.sem();
      let mut lim = num::BigUint::from(2_u64);
      lim = lim.pow(x.bits() as u32);
      let expect = if a >= b { a - b } else { a + lim - b };
      Some(xr - yr == DWord::from_uint(x.bits(), &expect))
    })
  }

  #[test]
  pub fn mul() {
    do_test(binary, |(x,y): (DWord,DWord)| {
      let (xr,a) = x.sem();
      let (yr,b) = y.sem();
      Some(xr * yr == DWord::from_uint(x.bits(), &(&a * &b)))
    })
  }

  #[test]
  pub fn div() {
    do_test(binary, |(x,y): (DWord,DWord)| {
      let (xr,a) = x.sem();
      let (yr,b) = y.sem();
      if yr.is_zero() {
        return if yr.bits() == 0 { Some(true) } else { None };
      }
      Some(xr / yr == DWord::from_uint(x.bits(), &(&a / &b)))
    })
  }

  #[test]
  pub fn rem() {
    do_test(binary, |(x,y): (DWord,DWord)| {
      let (xr,a) = x.sem();
      let (yr,b) = y.sem();
      if yr.is_zero() {
        return if yr.bits() == 0 { Some(true) } else { None };
      }
      Some(xr % yr == DWord::from_uint(x.bits(), &(&a % &b)))
    })
  }

  #[test]
  pub fn pow() {
    do_test(word_and::<u32>, |(x,p): (DWord,u32)| {
      let (xr,a) = x.sem();
      let lim = num::BigUint::from(2_u64).pow(x.bits() as u32);
      let expect = a.modpow(&p.into(),&lim);
      Some(xr.pow(p) == DWord::from_uint(x.bits(), &expect))
    })
  }



}
