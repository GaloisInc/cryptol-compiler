use std::fmt;

pub trait Display {
  fn display(&self, base: usize, fmt: &mut fmt::Formatter) -> fmt::Result;
}

impl<T: Display> Display for &[T] {
  fn display(&self, base: usize, fmt: &mut fmt::Formatter) -> fmt::Result
  {
    write!(fmt,"[")?;
    let mut xs = self.into_iter();
    match xs.next() {
      Some(fst) => fst.display(base,fmt)?,
      None      => { return write!(fmt,"]") }
    }

    for i in xs {
      write!(fmt,", ")?;
      i.display(base,fmt)?;
    }
    write!(fmt, "]")
  }
}


/// Display using standard Rust functions
pub fn std_display<T: fmt::Display + fmt::Binary + fmt::Octal + fmt::UpperHex>
  (x: &T, base: usize, fmt: &mut fmt::Formatter<'_>) -> Result<(),fmt::Error> {

  match base {
    2  => <T as fmt::Binary>::fmt(x,fmt),
    8  => <T as fmt::Octal>::fmt(x,fmt),
    10 => <T as fmt::Display>::fmt(x,fmt),
    16 => <T as fmt::UpperHex>::fmt(x,fmt),
    _  => Err(fmt::Error)
  }
}


pub struct Displayable<'a,T: ?Sized>(pub &'a T);

/// Used for types that are not directly displayable (e.g., array).
/// The type parameter `T` is a wrappar type that implements the display traits.
pub trait StdDisplay {
  fn displayable<'a>(&'a self) -> Displayable<'a,Self>;
}

impl<T> StdDisplay for T {
  fn displayable<'a>(&'a self) -> Displayable<'a,T> { Displayable(self) }
}

impl<'a,T:Display> fmt::Display for Displayable<'a,T> {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    self.0.display(10,fmt)
  }
}

impl<'a,T:Display> fmt::Binary for Displayable<'a,T> {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    self.0.display(2,fmt)
  }
}

impl<'a,T:Display> fmt::Octal for Displayable<'a,T> {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    self.0.display(8,fmt)
  }
}

impl<'a,T:Display> fmt::UpperHex for Displayable<'a,T> {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    self.0.display(16,fmt)
  }
}







