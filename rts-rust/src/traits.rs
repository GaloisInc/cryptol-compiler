use crate::type_traits::*;

pub trait Zero: Type {

  /// The value tha acts like 0.
  fn zero(n : Self::Length) -> Self;
}

// We reuse Rust's Eq and Ord for equality

pub trait Logic: Type {
  fn complement(x: Self::Arg<'_>) -> Self;
  fn xor(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self;
  fn and(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self;
  fn or (x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self;
}


pub trait Ring: Type {

  /// Negate a value
  fn negate(x: Self::Arg<'_>) -> Self;

  /// x * y
  fn mul(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self;

  /// x - y
  fn sub(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self;

  /// x + y
  fn add(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self;

  /// Convert an integer to a value of the given type.
  /// Produces a result even if the type does not fit
  /// (e.g., wrap around for bit types)
  fn from_integer(n: Self::Length, x: &num::BigInt) -> Self;

  /// Raise `x` to the `y` power.
  /// Note that the Cryptol the power is polymorphic,
  /// but here we use `usize`, so we don't support raising
  /// things to value that do not fit in `usize`.
  fn exp(x: Self::Arg<'_>, y: usize) -> Self;
}


pub trait Integral: Type {

  /// This is not a standard Cryptol primitives, but we use it
  /// in places where `Integral` is used for idexing into things or exponents.
  fn to_usize(x: Self::Arg<'_>) -> usize;

  /// Convert something to an integer.
  fn to_integer(x: Self::Arg<'_>) -> num::BigInt;

  /// Integral division
  fn div(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self;

  /// Modulo
  fn modulo(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self;
}


pub trait Literal: Type {
  fn number_usize(n: Self::Length, x: usize) -> Self;
  fn number_uint(n: Self::Length, x: &num::BigUint) -> Self;
}

pub trait LiteralNumber<T> : Literal {
  fn number(n: Self::Length, x:T) -> Self;
}

impl<T : Literal> LiteralNumber<usize> for T {
  fn number(n: Self::Length, x:usize) -> Self { Self::number_usize(n,x) }
}

impl<T : Literal> LiteralNumber<&num::BigUint> for T {
  fn number(n: Self::Length, x:&num::BigUint) -> Self { Self::number_uint(n,x) }
}

