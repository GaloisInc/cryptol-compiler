use crate::traits::*;
use std::fmt as fmt;

#[derive(Debug,PartialEq,Eq,PartialOrd,Ord,Copy,Clone)]
pub struct Array<const N: usize, T>(pub [T;N]);


/* Basic Conversions */

impl<const N: usize, T> From<[T;N]> for Array<N,T> {
  fn from(x: [T;N]) -> Self { Array(x) }
}

impl<const N: usize, T> From<Array<N,T>> for [T;N] {
  fn from(x: Array<N,T>) -> Self { x.0 }
}

impl<'a, const N: usize, T> From<&'a Array<N,T>> for &'a [T] {
  fn from(x: &'a Array<N,T>) -> Self { &x.0 }
}

impl<const N: usize, T> Array<N,T> {
  pub fn from_fn<F: FnMut(usize) -> T>(f: F) -> Self {
    Self::from(std::array::from_fn(f))
  }
}



/* Indexing */

impl<const N: usize, T> std::ops::Index<usize> for Array<N,T> {
  type Output = T;
  fn index(&self, i: usize) -> &T {
    &self.0[i]
  }
}


/* Iterators */

impl<const N: usize, T> std::iter::IntoIterator for Array<N,T> {
  type Item = T;
  type IntoIter = <[T;N] as std::iter::IntoIterator>::IntoIter;
  fn into_iter(self) -> Self::IntoIter { self.0.into_iter() }
}

impl<'a, const N: usize, T> std::iter::IntoIterator for &'a Array<N,T> {
  type Item = &'a T;
  type IntoIter = std::slice::Iter<'a,T>;
  fn into_iter(self) -> Self::IntoIter { self.0.iter() }
}






/* Lenght and Zero */

impl<const N: usize, T: Length> Length for Array<N,T> {
  type Length = T::Length;
}

impl<const N: usize, T: Zero> Zero for Array<N,T> {
  fn zero(n: Self::Length) -> Self { Self::from_fn(|_i| T::zero(n)) }
}



/* Sequence operations */

impl<T : Clone, const N: usize> Sequence for Array<N,T> {
  type Item = T;

  fn length(&self) -> usize { N }

  fn index(&self, i: usize) -> T { self[i].clone() }

  fn shift_right(&self, n: <T as Length>::Length, amt: usize) -> Self
    where T : Zero + Length
  {
    Self::from_fn(|i| if i < amt { T::zero(n) } else { self.index(i-amt) })
  }

  fn shift_right_signed(&self, amt: usize) -> Self {
    Self::from_fn(|i| self.index(if i < amt { 0 } else { i - amt }))
  }

  fn rotate_right(&self, amt: usize) -> Self {
    if N == 0 { return self.clone() };
    let a = amt % N;
    Self::from_fn(|i| self.index((N + i - a) % N))
  }


  fn shift_left(&self, n: <T as Length>::Length, amt: usize) -> Self
    where T : Zero
  {
    Self::from_fn(|i| {
      let j = amt + i;
      if j >= N { T::zero(n) } else { self.index(j) } })
  }

  fn rotate_left(&self, amt: usize) -> Self {
    Self::from_fn(|i| self.index((amt + i) % N))
  }
}


/* Formatting */

macro_rules! ArrayFormatter {
  ( $($trait:ident),*) => { $(
    impl<const N: usize, T: fmt::$trait> fmt::$trait for Array<N,T> {
      fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        write!(f,"[")?;
        let mut xs = self.into_iter();
        match xs.next() {
          Some(x) => fmt::$trait::fmt(x,f)?,
          None    => { return write!(f,"]") }
        }

        for i in xs {
          write!(f,", ")?;
          fmt::$trait::fmt(i,f)?;
        }
        write!(f, "]")
      }
    }
    )*
  }
}

ArrayFormatter! { Display, Binary, Octal, UpperHex, LowerHex }












#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_basics() {
    let x = <Array<3,u8>>::from([1,2,3]);
    assert_eq!(x.length(), 3);
    assert_eq!(x.index(2), 3);

    assert_eq!(x.shift_left((),1), <_>::from([2,3,0]));
    assert_eq!(x.shift_left((),0), <_>::from([1,2,3]));
    assert_eq!(x.shift_left((),10), <_>::from([0,0,0]));

    assert_eq!(x.shift_right((),1), <_>::from([0,1,2]));
    assert_eq!(x.shift_right((),0), <_>::from([1,2,3]));
    assert_eq!(x.shift_right((),10), <_>::from([0,0,0]));

    assert_eq!(x.shift_right_signed(1), <_>::from([1,1,2]));
    assert_eq!(x.shift_right_signed(0), <_>::from([1,2,3]));
    assert_eq!(x.shift_right_signed(10), <_>::from([1,1,1]));

    assert_eq!(x.rotate_left(1), <_>::from([2,3,1]));
    assert_eq!(x.rotate_left(0), <_>::from([1,2,3]));
    assert_eq!(x.rotate_left(11), <_>::from([3,1,2]));

    assert_eq!(x.rotate_right(1), <_>::from([3,1,2]));
    assert_eq!(x.rotate_right(0), <_>::from([1,2,3]));
    assert_eq!(x.rotate_right(11), <_>::from([2,3,1]));

  }


}

