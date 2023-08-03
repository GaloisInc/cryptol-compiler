/// Non-standard Cryptol trait ranging over sequence representations
pub trait Sequence {
  type Item : Clone;

  /// Length of this sequence
  fn length(&self) -> usize;

  /// Shift a sequence to the right.
  /// New elements on the left will be filled using the `Zero` trait.
  ///   * `n`   - length, if the elements are seuqneces of dynamic size or ()
  ///   * `xs`  - sequence
  ///   * `amt` - how much to shift by
  fn shift_right(&self, n: <Self::Item as Length>::Length, amt: usize) -> Self
    where Self::Item : Zero;

  /// Shift a sequence to the right.
  /// New elements would be copies of most significant element.
  ///   * `xs`  - sequence
  ///   * `amt` - how much to shift by
  fn shift_right_signed(&self, amt: usize) -> Self;

  /// Rotate the elements of a sequence to right.
  ///   * `xs`  - sequence
  ///   * `amt` - how much to rotate by
  fn rotate_right(&self, amt: usize) -> Self;

  /// Shift a sequence to the left.
  /// New elements on the right will be filled using the `Zero` trait.
  ///   * `n`   - length, if the elements are seuqneces of dynamic size or ()
  ///   * `xs`  - sequence
  ///   * `amt` - how much to shift by
  fn shift_left(&self, n: <Self::Item as Length>::Length, amt: usize) -> Self
    where Self::Item : Zero;

  /// Rotate the elements of a sequence to left.
  ///   * `xs`  - sequence
  ///   * `amt` - how much to rotate by
  fn rotate_left(&self, amt: usize) -> Self;


  /// Get the element at a certain index.
  /// Assert: `i < lengt()`.
  fn index(&self, i: usize) -> Self::Item;

}

/// The type of an extra parameter specifying the length of thing.
/// This is used when we want to make a new value whose size is
/// not statically known (e.g., a `Vec`)
pub trait Length {

  /// Extra information for allocating sequences.
  type Length : Copy;
}


pub trait Zero : Length {

  /// The value tha acts like 0.
  fn zero(n : Self::Length) -> Self;
}

pub trait Logic {
  fn complement(x: &Self) -> Self;
  fn xor(x: &Self, y: &Self) -> Self;
  fn and(x: &Self, y: &Self) -> Self;
  fn or(x: &Self, y: &Self) -> Self;
}


pub trait Ring {

  /// Negate a value
  fn negate(x: &Self) -> Self;

  /// x * y
  fn mul(x: &Self, y: &Self) -> Self;

  /// x - y
  fn sub(x: &Self, y: &Self) -> Self;

  /// x + y
  fn add(x: &Self, y: &Self) -> Self;

  /// Convert an integer to a value of the given type.
  /// Produces a result even if the type does not fit
  /// (e.g., wrap around for bit types)
  fn from_integer(x: &num::BigInt) -> Self;

  /// Raise `x` to the `y` power.
  /// Note that the Cryptol the power is polymorphic,
  /// but here we use `usize`, so we don't support raising
  /// things to value that do not fit in `usize`.
  fn exp(x: &Self, y: u64) -> Self;
}


pub trait Integral {

  /// This is not a standard Cryptol primitives, but we use it
  /// in places where `Integral` is used for idexing into things or exponents.
  /// Assert: `x` fits in `usize`
  fn to_u64(x: &Self) -> u64;

  /// Convert something to an integer.
  fn to_integer(x: &Self) -> num::BigInt;

  /// Integral division
  fn div(x: &Self, y: &Self) -> Self;

  /// Modulo
  fn modulo(x: &Self, y: &Self) -> Self;
}


pub trait Literal : Length {
  fn number_u64(n: Self::Length, x: u64) -> Self;
  fn number_int(n: Self::Length, x: &num::BigUint) -> Self;
}

pub trait LiteralNumber<T> : Literal {
  fn number(n: Self::Length, x:T) -> Self;
}

impl<T : Literal> LiteralNumber<u64> for T {
  fn number(n: Self::Length, x:u64) -> Self { Self::number_u64(n,x) }
}

impl<T : Literal> LiteralNumber<&num::BigUint> for T {
  fn number(n: Self::Length, x:&num::BigUint) -> Self { Self::number_int(n,x) }
}

