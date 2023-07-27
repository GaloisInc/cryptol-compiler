pub mod traits;
pub mod prim_bitvec;
pub mod vec;
pub mod array;
pub mod bit;
pub mod bitvec_fixed;
pub mod bitvec_fixed_impls;

pub use traits::*;

pub mod trait_methods {
  pub use crate::traits::Zero as _;
  pub use crate::traits::Literal as _;
  pub use crate::traits::LiteralNumber as _;
  pub use crate::traits::Integral as _;
  pub use crate::traits::Ring as _;
  /* XXX: Add other traits */
}


