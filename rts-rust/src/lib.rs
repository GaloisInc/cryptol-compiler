pub mod type_traits;
pub mod traits;
pub mod display;

pub mod word;

pub mod word_traits;
pub mod array;
pub mod vec;

pub mod bit;
pub mod int;

// XXX
pub mod prim_bitvec;

pub use type_traits::*;
pub use traits::*;
pub use display::*;
pub use dword::DWord;

pub mod trait_methods {
  pub use crate::display::Display as _;
  pub use crate::display::Base as _;

  pub use crate::type_traits::Type as _;
  pub use crate::type_traits::Sequence as _;
  pub use crate::type_traits::Word as _;

  pub use crate::traits::Zero as _;
  pub use crate::traits::Literal as _;
  pub use crate::traits::LiteralNumber as _;
  pub use crate::traits::Integral as _;
  pub use crate::traits::Ring as _;
  /* XXX: Add other traits */
}


// XXX: Placeholder type for functions
pub struct Fun;


#[macro_export]
/// Generate the `Type` instance for a type that is passed by value (Copy),
/// and has no interesting lenght.
macro_rules! PrimType {

  ($ty:ty) => {

    impl crate::type_traits::Type for $ty {
      type Length  = ();
      type Arg<'a> = Self;
      fn as_owned(arg: Self::Arg<'_>) -> Self { arg }
      fn as_arg(&self) -> Self::Arg<'_> { *self }
    }
  };
}



#[macro_export]
/// Generate the `Type` instance for a type that is passed by reference,
/// and has no interesting lenght.
macro_rules! RefType {

  ($ty:ty) => {

    impl crate::type_traits::Type for $ty {
      type Length = ();
      type Arg<'a> = &'a Self;
      fn as_owned(arg: Self::Arg<'_>) -> Self { arg.clone() }
      fn as_arg(&self) -> Self::Arg<'_> { self }
    }
  };
}


