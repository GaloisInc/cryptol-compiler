



/// Non-standard Cryptol trait ranging over sequence representations
pub trait Sequence {
  type Item : Clone;

  /// Length of this sequence
  fn cry_length(&self) -> usize;

  /// Shift a sequence to the right.
  /// New elements on the left will be filled using the `Zero` trait.
  ///   * `n`   - length, if the elements are seuqneces of dynamic size or ()
  ///   * `xs`  - sequence
  ///   * `amt` - how much to shift by
  fn cry_shift_right(&self, n: <Self::Item as Zero>::Length, amt: usize) -> Self
    where Self::Item : Zero;

  /// Shift a sequence to the right.
  /// New elements would be copies of most significant element.
  ///   * `xs`  - sequence
  ///   * `amt` - how much to shift by
  fn cry_shift_right_signed(&self, amt: usize) -> Self;

  /// Rotate the elements of a sequence to right.
  ///   * `xs`  - sequence
  ///   * `amt` - how much to rotate by
  fn cry_rotate_right(&self, amt: usize) -> Self;

  /// Shift a sequence to the left.
  /// New elements on the right will be filled using the `Zero` trait.
  ///   * `n`   - length, if the elements are seuqneces of dynamic size or ()
  ///   * `xs`  - sequence
  ///   * `amt` - how much to shift by
  fn cry_shift_left(&self, n: <Self::Item as Zero>::Length, amt: usize) -> Self
    where Self::Item : Zero;

  /// Rotate the elements of a sequence to left.
  ///   * `xs`  - sequence
  ///   * `amt` - how much to rotate by
  fn cry_rotate_left(&self, amt: usize) -> Self;


  /// Get the element at a certain index.
  /// Assert: `i < lengt()`.
  fn cry_index(&self, i: usize) -> Self::Item;

}


pub trait Zero {

  /// Extra information for allocating sequences.
  type Length : Copy;

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
  fn exp(x: &Self, y: usize) -> Self;
}


pub trait Integral {

  /// This is not a standard Cryptol primitives, but we use it
  /// in places where `Integral` is used for idexing into things or exponents.
  /// Assert: `x` fits in `usize`
  fn to_usize(x: &Self) -> usize;

  /// Convert something to an integer.
  fn to_integer(x: &Self) -> num::BigInt;

  /// Integral division
  fn div(x: &Self, y: &Self) -> Self;

  /// Modulo
  fn modulo(x: &Self, y: &Self) -> Self;
}


pub trait Literal {
  type Length;

  fn number_u64(n: Self::Length, x: u64) -> Self;
  fn number_integer(n: Self::Length, x: &num::BigInt) -> Self;
}


