use std::fmt;

pub trait Base<const BASE: usize> {
  fn format(&self, fmt: &mut fmt::Formatter) -> fmt::Result;
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

#[macro_export]
macro_rules! derive_display {
  ($t:ty) =>  {

    impl $crate::display::Base<2> for $t {
      fn format(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        <$t as std::fmt::Binary>::fmt(self, fmt)
      }
    }

    impl $crate::display::Base<8> for $t {
      fn format(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        <$t as std::fmt::Octal>::fmt(self, fmt)
      }
    }

    impl $crate::display::Base<10> for $t {
      fn format(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        <$t as std::fmt::Display>::fmt(self, fmt)
      }
    }

    impl $crate::display::Base<16> for $t {
      fn format(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        <$t as std::fmt::UpperHex>::fmt(self, fmt)
      }
    }

  };
}



