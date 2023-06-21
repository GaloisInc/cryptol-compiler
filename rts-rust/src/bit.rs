use crate::traits::*;

impl Zero for bool {
  type Length = ();
  fn zero(_:Self::Length) -> Self { false }
}

