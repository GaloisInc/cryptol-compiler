use crate::traits::*;
use crate::type_traits::*;
use dword::*;
use dword::iter_bits::*;
use dword::split::*;


crate::derive_display!(DWordRef<'_>);
crate::derive_display!(DWord);

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

  fn exp_usize(x: Self::Arg<'_>, y: usize) -> Self {
    assert!(y <= (u32::MAX as usize));
    x.pow(y as u32)
  }
}


impl Integral for DWord {

  fn to_usize(x: Self::Arg<'_>) -> usize { x.into() }
  fn to_usize_maybe(x: Self::Arg<'_>) -> Option<usize> { x.into() }
  fn to_integer(x: Self::Arg<'_>) -> num::BigInt { x.into() }
  fn div(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x / y }
  fn modulo(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x % y }
}


impl Literal for DWord {
  fn number_usize(n: Self::Length, x: usize) -> Self { Self::from_usize(n,x) }
  fn number_uint(n: Self::Length, x: &num::BigUint) -> Self {
    Self::from_uint(n,x)
  }
}

impl Logic for DWord {
  fn complement(x: Self::Arg<'_>) -> Self { !x }
  fn xor(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x ^ y }
  fn and(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x & y }
  fn or (x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x | y }
}


impl Sequence for DWordRef<'_> {
  type Item = bool;

  fn length(self) -> usize { self.bits() }

  fn shift_right(self, _: (), amt: usize) -> Self::Owned {
    self >> amt
  }

  fn shift_right_signed(self, amt: usize) -> Self::Owned {
    // Note that this is not recursive, we are calling the
    // inherent method with the same name on DWordRef
    self.shift_right_signed(amt)
  }

  fn shift_left(self, _: (), amt: usize) -> Self::Owned { self << amt }

  fn rotate_right(self, amt: usize) -> Self::Owned { self.rotate_right(amt) }

  fn rotate_left(self, amt: usize) -> Self::Owned { self.rotate_left(amt) }

  fn index(self, i: usize) -> Self::Item { self.index_msb(i) }

}



impl<INDEX: IndexDir> CloneArg for TraverseBitsOwned<INDEX> {
  type Owned = Self;
  fn clone_arg(self) -> Self::Owned { self }
}

impl<INDEX: IndexDir> Type for TraverseBitsOwned<INDEX> {
  type Arg<'a> = Self where Self: 'a;
  type Length  = ();    // XXX: ???
  fn as_arg(&self) -> Self::Arg<'_> { self.clone() }
}

impl<INDEX: IndexDir> Stream<bool> for TraverseBitsOwned<INDEX>
{}


impl<INDEX: IndexDir> CloneArg for TraverseWordsOwned<INDEX> {
  type Owned = Self;
  fn clone_arg(self) -> Self::Owned { self }
}

impl<INDEX: IndexDir> Type for TraverseWordsOwned<INDEX> {
  type Arg<'a> = Self where Self: 'a;
  type Length  = ();    // XXX: ???
  fn as_arg(&self) -> Self::Arg<'_> { self.clone() }
}

impl<INDEX: IndexDir> Stream<DWord> for TraverseWordsOwned<INDEX>
{}





