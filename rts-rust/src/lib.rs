pub mod type_traits;
pub mod traits;
pub mod display;

pub mod cry_dword;
pub mod vec;
pub mod stream;
pub mod tuple;

pub mod bit;
pub mod int;
pub mod range;
pub mod transpose;

pub use type_traits::*;
pub use traits::*;
pub use display::*;
pub use dword::DWord;
pub use dword::DWordRef;
pub use stream::*;
pub use range::*;
pub use transpose::*;
pub use tuple::*;

pub mod trait_methods {
  pub use crate::display::Display as _;
  pub use crate::display::Base as _;

  pub use crate::type_traits::Type as _;
  pub use crate::type_traits::CloneArg as _;
  pub use crate::type_traits::Sequence as _;
  pub use crate::type_traits::Word as _;
  pub use crate::type_traits::Stream as _;

  pub use crate::traits::Zero as _;
  pub use crate::traits::Literal as _;
  pub use crate::traits::LiteralNumber as _;
  pub use crate::traits::Integral as _;
  pub use crate::traits::Ring as _;
  pub use crate::traits::Logic as _;
  /* XXX: Add other traits */
}


// XXX: Placeholder type for functions
pub struct Fun;


#[macro_export]
/// Generate the `Type` instance for a type that is passed by value (Copy),
/// and has no interesting lenght.
macro_rules! PrimType {

  ($ty:ty) => {

    impl $crate::type_traits::Type for $ty {
      type Length  = ();
      type Arg<'a> = Self;
      fn as_arg(&self) -> Self::Arg<'_> { self.clone() }
    }

    impl $crate::type_traits::CloneArg for $ty {
      type Owned = $ty;
      fn clone_arg(self) -> Self::Owned { self }
    }
  };
}



#[macro_export]
/// Generate the `Type` instance for a type that is passed by reference,
/// and has no interesting lenght.
macro_rules! RefType {

  ($ty:ty) => {

    impl $crate::type_traits::Type for $ty {
      type Length = ();
      type Arg<'a> = &'a Self;
      fn as_arg(&self) -> Self::Arg<'_> { self }
    }

    impl $crate::type_traits::CloneArg for &'_ $ty {
      type Owned = $ty;
      fn clone_arg(self) -> Self::Owned { self.clone() }
    }
  };
}



