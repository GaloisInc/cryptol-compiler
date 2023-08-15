use crate::traits::*;
use crate::PrimType;

PrimType!(bool);

impl Zero for bool {
  fn zero(_:Self::Length) -> Self { false }
}

