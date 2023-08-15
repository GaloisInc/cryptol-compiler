// Words of dynamic sizes

use crate::type_traits::*;
use crate::traits::*;

// It might be better to use a vector of limbs, similar to how word works.
// Then we could have common code between the two that operators on slices.
#[derive(Debug,Clone)]
pub struct DWord {
  pub value: num::BigUint,
  pub bits:  u64
}

impl Type for DWord {
  type Arg<'a> = &'a DWord;
  type Length  = u64;

  fn as_owned(arg: Self::Arg<'_>) -> Self { arg.clone() }

  fn as_arg(&self) -> Self::Arg<'_> { self }
}

impl Zero for DWord {
  fn zero(bits: Self::Length) -> Self {
    DWord { value: <num::BigUint as num::Zero>::zero(), bits: bits }
  }
}




