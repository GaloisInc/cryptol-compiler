use apint::ApInt;
use num::{self};

// -- basic types -------------------------------------------------------------

type Bit = bool;
type BitVector = ApInt;
type Integer = num::BigInt;
type Natural = num::BigUint;
type Rational = num::BigRational;
#[derive(Clone, Copy)]
enum Size {
  Finite(usize),
  Infinite
}

// -- tools to help normalize representations ---------------------------------
// maybe this is word specific?
trait Bits {
  fn mod2n(&self, n: u32) -> Self;
}

impl Bits for u8 {
  fn mod2n(&self, n: u32) -> Self {
    self >> u8::BITS.saturating_sub(n)
  }
}

impl Bits for u64 {
  fn mod2n(&self, n: u32) -> Self {
    self >> u64::BITS.saturating_sub(n)
  }
}

impl Bits for BitVector {
  fn mod2n(&self, n: u32) -> Self {
      let mut r = self.clone();
      r.zero_resize(n as usize);
      r
  }
}


// -- Literals ----------------------------------------------------------------
// TODO: how much of Literals is really relevant?

// -- Logic -------------------------------------------------------------------

trait Logic {
  fn land(&self, rhs: &Self) -> Self;
  fn lor(&self, rhs: &Self) -> Self;
  fn lxor(&self, rhs: &Self) -> Self;
  fn lnot(&self, rhs: &Self) -> Self;
}

impl Logic for u8 {
  fn land(&self, rhs: &Self) -> Self {
    self & *rhs
  }

  fn lor(&self, rhs: &Self) -> Self {
    self | *rhs
  }

  fn lxor(&self, rhs: &Self) -> Self {
    self ^ *rhs
  }

  fn lnot(&self, rhs: &Self) -> Self {
    !self
  }
}

impl Logic for u64 {
  fn land(&self, rhs: &Self) -> Self {
    self & *rhs
  }

  fn lor(&self, rhs: &Self) -> Self {
    self | *rhs
  }

  fn lxor(&self, rhs: &Self) -> Self {
    self ^ *rhs
  }

  fn lnot(&self, rhs: &Self) -> Self {
    !self
  }
}

// -- ring ------------------------------------------------------------------
trait Ring {
  fn from_integer(i: &Integer) -> Self;
  fn radd(&self, rhs: &Self) -> Self;
  fn rmul(&self, rhs: &Self) -> Self;
  fn rsub(&self, rhs: &Self) -> Self;
  fn rneg(&self) -> Self;
}

impl Ring for u8 {
  fn from_integer(i: &Integer) -> Self { todo!() }
  fn radd(&self, rhs: &Self) -> Self { self.wrapping_add(*rhs) }
  fn rmul(&self, rhs: &Self) -> Self { self.wrapping_mul(*rhs) }
  fn rsub(&self, rhs: &Self) -> Self { self.wrapping_sub(*rhs) }
  fn rneg(&self) -> Self { self.wrapping_neg() }
}

impl Ring for BitVector {
    fn radd(&self, rhs: &Self) -> Self {
      self + rhs
    }

    fn rmul(&self, rhs: &Self) -> Self {
      self * rhs
    }

    fn rsub(&self, rhs: &Self) -> Self {
      self - rhs
    }

    fn rneg(&self) -> Self {
      -self
    }

    fn from_integer(i: &Integer) -> Self {
        todo!()
    }
}

// -- Integral ----------------------------------------------------------------

trait Integral: Sized {
  type RangeIter: Array<Self>;
  fn idiv(&self, rhs: &Self) -> Self;
  fn irem(&self, rhs: &Self) -> Self;
  fn to_integer(&self) -> Integer;
  fn ipow(&self, rhs: &Self) -> Self;
  fn inf_from(&self) -> Self::RangeIter;
  fn int_from_then(&self, second: &Self) -> Self::RangeIter;
}

// -- Field -------------------------------------------------------------------

trait Field {
  fn recip(&self) -> Self;
  fn fdiv(&self, rhs: &Self) -> Self;
}

// -- Round -------------------------------------------------------------------

trait Round {
  fn ceil(&self) -> Self;
  fn floor(&self) -> Self;
  fn trunc(&self) -> Self;
  fn round_away(&self) -> Self;
  fn round_to_even(&self) -> Self;
}

// -- Eq and Cmp can be handled with the builtin classes for Eq, Ord?
trait SignedCmp {
  fn slt(&self, rhs: &Self) -> Bit;
  fn sgt(&self, rhs: &Self) -> Bit {
    rhs.slt(self)
  }
  fn slte(&self, rhs: &Self) -> Bit {
    !rhs.sgt(self)
  }
  fn sgte(&self, rhs: &Self) -> Bit {
    !self.slt(rhs)
  }
}

// -- Bit can use builtin &&, ||, with implications as !a || b

// -- BitVector ---------------------------------------------------------------

trait BV {
  fn sdiv(&self, rhs: &Self) -> Self;
  fn srem(&self, rhs: &Self) -> Self;
  fn carry(&self, rhs: &Self) -> Bit;
  fn scarry(&self, rhs: &Self) -> Bit;
  fn sborrow(&self, rhs: &Self) -> Bit;
  fn zext(&self, len: usize) -> Self;
  fn sext(&self, len: usize) -> Self;
  fn ashr(&self, i: impl Integral) -> Self;
  fn lg2(&self) -> Self;
  fn to_signed_integer(&self) -> Integer;
}

// -- Rational ----------------------------------------------------------------

fn ratio(numer: Integer, denom: Integer) -> Rational {
  Rational::new(numer, denom)
}

// -- Zn ----------------------------------------------------------------------
// no relevant operations, fromZ will likely just be `mod` on Integer

// -- arrays ------------------------------------------------------------------

// TODO: placeholder
trait Array<E> {
  fn length(&self) -> Size;
}
