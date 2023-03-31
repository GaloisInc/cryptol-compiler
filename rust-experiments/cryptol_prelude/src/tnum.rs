use num;
use num::ToPrimitive;

pub enum TNum {
  Inf,
  FinSize(usize),
  Fin(num::BigInt)
}

impl TNum {
  pub fn from_usize(x: usize) -> Self { TNum::FinSize(x) }
}

impl PartialEq for TNum {

  fn eq(&self, other: &TNum) -> bool {
    match (self, other) {
      (TNum::Inf,TNum::Inf) => true,
      (_,TNum::Inf) | (TNum::Inf,_) => false,

      (TNum::FinSize(x), TNum::FinSize(y)) => x == y,
      (TNum::Fin(x), TNum::Fin(y)) => x == y,

      (TNum::FinSize(x), TNum::Fin(y)) |
        (TNum::Fin(y), TNum::FinSize(x)) => Some(*x) == y.to_usize()
    }
  }
}

impl Eq for TNum {}