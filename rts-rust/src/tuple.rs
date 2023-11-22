use crate::type_traits::*;

impl<A: Type, B: Type> Type for (A,B) {
  type Arg<'a> = &'a (A, B) where Self: 'a;
  type Length = (A::Length, B::Length);
  fn as_arg(&self) -> Self::Arg<'_> { self }
}

impl<A: Type, B: Type> CloneArg for &(A,B) {
  type Owned = (A,B);
  fn clone_arg(self) -> Self::Owned { self.clone() }
}




