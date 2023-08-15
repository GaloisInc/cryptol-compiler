use std::array::from_fn;
use std::fmt as fmt;
use crate::traits::*;
use crate::display::Base;


impl<const N: usize, T: Type> Type for [T;N]  {
  type Length = T::Length;
  type Arg<'a> = &'a[T] where T: 'a;
  fn as_owned(arg: Self::Arg<'_>) -> Self { from_fn(|i| arg[i].clone()) }
  fn as_arg(&self) -> Self::Arg<'_> { self }
}


/* Lenght and Zero */

impl<const N: usize, T: Zero> Zero for [T;N] {
  fn zero(n: Self::Length) -> Self { from_fn(|_i| T::zero(n)) }
}



/* Sequence operations */

impl<T : Type, const N: usize> Sequence for [T;N] {
  type Item = T;

  fn length(&self) -> usize { N }

  fn index(&self, i: usize) -> T { self[i].clone() }

  fn shift_right(&self, n: T::Length, amt: usize) -> Self
    where T : Zero
  {
    from_fn(|i| if i < amt { T::zero(n) } else { self.index(i-amt) })
  }

  fn shift_right_signed(&self, amt: usize) -> Self {
    from_fn(|i| self.index(if i < amt { 0 } else { i - amt }))
  }

  fn rotate_right(&self, amt: usize) -> Self {
    if N == 0 { return self.clone() };
    let a = amt % N;
    from_fn(|i| self.index((N + i - a) % N))
  }


  fn shift_left(&self, n: T::Length, amt: usize) -> Self
    where T : Zero
  {
    from_fn(|i| {
      let j = amt + i;
      if j >= N { T::zero(n) } else { self.index(j) } })
  }

  fn rotate_left(&self, amt: usize) -> Self {
    from_fn(|i| self.index((amt + i) % N))
  }
}


/* Formatting */
impl<const BASE: usize, const N: usize, T: Base<BASE>> Base<BASE> for [T;N] {
  fn format(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    Base::<BASE>::format(&self.as_slice(),fmt)
  }
}


#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_basics() {
    let x = [1_u8,2,3];
    assert_eq!(x.length(), 3);
    assert_eq!(x.index(2), 3);

    assert_eq!(x.shift_left((),1), [2,3,0]);
    assert_eq!(x.shift_left((),0), [1,2,3]);
    assert_eq!(x.shift_left((),10), [0,0,0]);

    assert_eq!(x.shift_right((),1), [0,1,2]);
    assert_eq!(x.shift_right((),0), [1,2,3]);
    assert_eq!(x.shift_right((),10), [0,0,0]);

    assert_eq!(x.shift_right_signed(1), [1,1,2]);
    assert_eq!(x.shift_right_signed(0), [1,2,3]);
    assert_eq!(x.shift_right_signed(10), [1,1,1]);

    assert_eq!(x.rotate_left(1), [2,3,1]);
    assert_eq!(x.rotate_left(0), [1,2,3]);
    assert_eq!(x.rotate_left(11), [3,1,2]);

    assert_eq!(x.rotate_right(1), [3,1,2]);
    assert_eq!(x.rotate_right(0), [1,2,3]);
    assert_eq!(x.rotate_right(11), [2,3,1]);

  }


}

