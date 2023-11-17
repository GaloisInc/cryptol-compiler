use crate::traits::*;
use crate::type_traits::*;
use dword::*;

impl Type for DWord {
  type Arg<'a> = DWordRef<'a>;
  type Length = usize;
  fn as_arg(&self) -> Self::Arg<'_> { self.as_ref() }
}

impl CloneArg for DWordRef<'_> {
  type Owned = DWord;
  fn clone_arg(self) -> Self::Owned { self.clone_word() }
}

impl Zero for DWord {
  fn zero(bits: Self::Length) -> Self { DWord::zero(bits) }
}

impl Ring for DWord {
  fn negate(x: Self::Arg<'_>) -> Self { -x }
  fn mul(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x * y }
  fn sub(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x - y }
  fn add(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x + y }
  fn from_integer(bits: Self::Length, x: &num::BigInt) -> Self {
    DWord::from_int(bits, x)
  }

  fn exp(x: Self::Arg<'_>, y: usize) -> Self {
    assert!(y <= (u32::MAX as usize));
    x.pow(y as u32)
  }
}


impl Integral for DWord {

  fn to_usize(x: Self::Arg<'_>) -> usize { x.into() }
  fn to_integer(x: Self::Arg<'_>) -> num::BigInt { x.into() }
  fn div(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x / y }
  fn modulo(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x % y }
}


impl Literal for DWord {
  fn number_usize(n: Self::Length, x: usize) -> Self { Self::from_usize(n,x) }
  fn number_int(n: Self::Length, x: &num::BigUint) -> Self {
    Self::from_uint(n,x)
  }
}


crate::derive_display!(DWordRef<'_>);
crate::derive_display!(DWord);
