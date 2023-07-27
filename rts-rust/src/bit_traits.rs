use crate::traits::*;

impl Length for bool {
  type Length = ();
}

impl Zero for bool {
  fn zero(_:Self::Length) -> Self { false }
}

