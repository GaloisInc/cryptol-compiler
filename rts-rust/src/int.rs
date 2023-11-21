use crate::traits::*;
use crate::RefType;

RefType!(num::BigInt);

impl Zero for num::BigInt {
  fn zero(_ : ()) -> Self { <Self as num::Zero>::zero() }
}

impl Literal for num::BigInt {
  fn number_usize(_: (), x: usize)       -> Self { Self::from(x) }
  fn number_uint(_: (), x: &num::BigUint) -> Self {
    Self::from(x.clone())
  }
}

impl Integral for num::BigInt {

  fn to_usize(x: &Self) -> usize {
    assert!(x.sign() != num::bigint::Sign::Minus);
    let mut it = x.iter_u64_digits();
    let mut res: u64 = 0;
    match it.next() {
      None => return res as usize,
      Some(x) => res = x
    };
    if let Some(_) = it.next() { assert!(false) }
    res as usize
  }

  fn to_integer(x: &Self)       -> num::BigInt { x.clone() }

  fn div   (x: &Self, y: &Self) -> Self        { x / y }
  fn modulo(x: &Self, y: &Self) -> Self        { x % y }
}

impl Ring for num::BigInt {
  fn negate      (x: &Self)           -> Self { -x }
  fn add         (x: &Self, y: &Self) -> Self { x + y }
  fn mul         (x: &Self, y: &Self) -> Self { x * y }
  fn sub         (x: &Self, y: &Self) -> Self { x - y }
  fn exp         (x: &Self, y: usize) -> Self {
    assert!(y <= (u32::MAX as usize));
    Self::pow(x,y as u32)
  }
  fn from_integer(_: Self::Length, x: &num::BigInt)    -> Self { x.clone() }
}

crate::derive_display!(num::BigUint);
crate::derive_display!(num::BigInt);
