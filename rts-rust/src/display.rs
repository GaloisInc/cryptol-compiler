use std::fmt;

pub trait Base<const BASE: usize> {
  fn format(&self, fmt: &mut fmt::Formatter) -> fmt::Result;
}

impl<const BASE: usize, T: Base<BASE>> Base<BASE> for &[T] {
  fn format(&self, fmt: &mut fmt::Formatter) -> fmt::Result
  {
    write!(fmt,"[")?;
    let mut xs = self.into_iter();
    match xs.next() {
      Some(fst) => Base::<BASE>::format(fst,fmt)?,
      None      => { return write!(fmt,"]") }
    }

    for i in xs {
      write!(fmt,", ")?;
      Base::<BASE>::format(i,fmt)?;
    }
    write!(fmt, "]")
  }
}

pub struct Displayable<'a,T: ?Sized>(pub &'a T);

/// Used to display types using the Formatting trait above.
pub trait Display {
  fn display<'a>(&'a self) -> Displayable<'a,Self>;
}

impl<T> Display for T {
  fn display<'a>(&'a self) -> Displayable<'a,T> { Displayable(self) }
}

impl<'a, T: Base<10>> fmt::Display for Displayable<'a,T> {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
   Base::<10>::format(self.0, fmt)
  }
}

impl<'a, T: Base<2>> fmt::Binary for Displayable<'a,T> {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    Base::<2>::format(self.0,fmt)
  }
}

impl<'a, T: Base<8>> fmt::Octal for Displayable<'a,T> {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    Base::<8>::format(self.0,fmt)
  }
}

impl<'a, T: Base<16>> fmt::UpperHex for Displayable<'a,T> {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    Base::<16>::format(self.0,fmt)
  }
}





