use std::fmt;
use crate::traits::*;
use crate::display::*;
use crate::PrimType;

PrimType!(bool);

impl<const BASE: usize, const UPPER: bool> Base<BASE, UPPER> for bool {
  fn format(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "{}", if *self { "True" } else { "False" })
  }
}

impl Zero for bool {
  fn zero(_:Self::Length) -> Self { false }
}

impl Logic for bool {
  fn complement(x: Self::Arg<'_>) -> Self { !x }
  fn xor(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x ^ y }
  fn and(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x & y }
  fn or (x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self { x | y }
}


