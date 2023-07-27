pub mod traits;
pub mod word;
pub mod prim_bitvec;
pub mod array_traits;
pub mod bit_traits;
pub mod word_traits;
pub mod vec_traits;

pub use traits::*;

pub mod trait_methods {
  pub use crate::traits::Zero as _;
  pub use crate::traits::Literal as _;
  pub use crate::traits::LiteralNumber as _;
  pub use crate::traits::Integral as _;
  pub use crate::traits::Ring as _;
  /* XXX: Add other traits */
}


