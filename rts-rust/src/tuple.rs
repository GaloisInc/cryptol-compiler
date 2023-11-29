use crate::type_traits::*;
use crate::display::*;
use std::fmt;

impl<A: Type, B: Type> Type for (A,B) {
  type Arg<'a> = &'a (A, B) where Self: 'a;
  type Length = (A::Length, B::Length);
  fn as_arg(&self) -> Self::Arg<'_> { self }
}

impl<A: Type, B: Type> CloneArg for &(A,B) {
  type Owned = (A,B);
  fn clone_arg(self) -> Self::Owned { self.clone() }
}


impl<const BASE: usize, const UPPER: bool, A,B> Base<BASE, UPPER> for (A,B)
  where A: Base<BASE, UPPER>, B: Base<BASE, UPPER> {
  fn format(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt,"(")?;
    Base::<BASE, UPPER>::format(&self.0,fmt)?;
    write!(fmt,",")?;
    Base::<BASE, UPPER>::format(&self.1,fmt)?;
    write!(fmt,")")
  }
}


