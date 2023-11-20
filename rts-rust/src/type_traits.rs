use crate::traits::*;

/// All types supported by the compiler should implement this.
pub trait Type : Clone {

  /// The type to use when passing Self values as arugments.
  type Arg<'a> : CloneArg<Owned=Self>
    where Self : 'a;

  /// Extra information to pass for types that need dynamic information
  /// to create a value.
  type Length : Copy;

  /// Turn a reference to an owned value into a function argument.
  fn as_arg(&self) -> Self::Arg<'_>;
}


pub trait CloneArg : Clone {
  type Owned;

  /// Turn an argument form into an owned value, possibly by cloning.
  fn clone_arg(self) -> Self::Owned;
}


crate::PrimType!{u64}

/// All finite sequence representations should support these operations.
pub trait Sequence : CloneArg {
  type Item : Type;

  /// Length of this sequence
  fn length(self) -> usize;

  /// Shift a sequence to the right.
  /// New elements on the left will be filled using the `Zero` trait.
  ///   * `n`   - length, if the elements are seuqneces of dynamic size or ()
  ///   * `xs`  - sequence
  ///   * `amt` - how much to shift by
  fn shift_right(self, n: <Self::Item as Type>::Length, amt: usize) -> Self::Owned
    where Self::Item : Zero;

  /// Shift a sequence to the right.
  /// New elements would be copies of most significant element.
  ///   * `xs`  - sequence
  ///   * `amt` - how much to shift by
  fn shift_right_signed(self, amt: usize) -> Self::Owned;

  /// Rotate the elements of a sequence to right.
  ///   * `xs`  - sequence
  ///   * `amt` - how much to rotate by
  fn rotate_right(self, amt: usize) -> Self::Owned;

  /// Shift a sequence to the left.
  /// New elements on the right will be filled using the `Zero` trait.
  ///   * `n`   - length, if the elements are seuqneces of dynamic size or ()
  ///   * `xs`  - sequence
  ///   * `amt` - how much to shift by
  fn shift_left(self, n: <Self::Item as Type>::Length, amt: usize) -> Self::Owned
    where Self::Item : Zero;

  /// Rotate the elements of a sequence to left.
  ///   * `xs`  - sequence
  ///   * `amt` - how much to rotate by
  fn rotate_left(self, amt: usize) -> Self::Owned;


  /// Get the element at a certain index.
  /// Assert: `i < length()`.
  fn index(self, i: usize) -> Self::Item;

}


/// All word representaitons should support these operations.
pub trait Word : Sequence<Item=bool> {
}


// All stream representaitons should support these operations.
pub trait Stream<T:Type> : Type + CloneArg<Owned=Self> + Iterator<Item=T> {
}



