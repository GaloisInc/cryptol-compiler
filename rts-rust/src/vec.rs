use crate::traits::*;
use std::fmt as fmt;

#[derive(Debug,PartialEq,Eq,PartialOrd,Ord,Clone)]
pub struct Vector<T>(pub Vec<T>);

/* Basic Conversions */

impl<T> From<Vec<T>> for Vector<T> {
  fn from(x: Vec<T>) -> Self { Vector(x) }
}

impl<T> From<Vector<T>> for Vec<T> {
  fn from(x: Vector<T>) -> Self { x.0 }
}

impl<'a, T> From<&'a Vector<T>> for &'a Vec<T> {
  fn from(x: &'a Vector<T>) -> Self { &x.0 }
}

impl<const N: usize, T> From<[T;N]> for Vector<T> {
  fn from(x: [T;N]) -> Self { Vector(x.into()) }
}

impl<T> Vector<T> {
  pub fn from_fn<F: FnMut(usize) -> T>(n: usize, mut f: F) -> Self {
    let mut result = Vec::with_capacity(n);
    for i in 0 .. n {
      result.push(f(i))
    }
    result.into()
  }
}





/* Indexing */

impl<T> std::ops::Index<usize> for Vector<T> {
  type Output = T;
  fn index(&self, i: usize) -> &T {
    &self.0[i]
  }
}


/* Iterators */

impl<T> std::iter::IntoIterator for Vector<T> {
  type Item = T;
  type IntoIter = std::vec::IntoIter<T>;
  fn into_iter(self) -> Self::IntoIter { self.0.into_iter() }
}

impl<'a, T> std::iter::IntoIterator for &'a Vector<T> {
  type Item = &'a T;
  type IntoIter = std::slice::Iter<'a,T>;
  fn into_iter(self) -> Self::IntoIter { self.0.iter() }
}



/* Lenght and Zero */

impl<T: Length> Length for Vector<T> {
  type Length = (usize,T::Length);
}


impl<T: Zero> Zero for Vector<T> {
  fn zero((vec_len, el_len): Self::Length) -> Self {
    Self::from_fn(vec_len, |_| T::zero(el_len))
  }
}


/* Sequence operations */

impl<T : Clone> Sequence for Vector<T> {
  type Item = T;

  fn length(&self) -> usize { self.0.len() }

  fn index(&self, i: usize) -> T { self[i].clone() }

  fn shift_right(&self, n: <T as Length>::Length, amt: usize) -> Self
    where T : Zero + Length
  {
    Self::from_fn( self.length()
                 , |i| if i < amt { T::zero(n) } else { self.index(i-amt) })
  }

  fn shift_right_signed(&self, amt: usize) -> Self {
    Self::from_fn( self.length()
                 , |i| self.index(if i < amt { 0 } else { i - amt }))
  }

  fn rotate_right(&self, amt: usize) -> Self {
    let n = self.length();
    if n == 0 { return self.clone() };
    let a = amt % n;
    Self::from_fn(n, |i| self.index((n + i - a) % n))
  }


  fn shift_left(&self, n: <T as Length>::Length, amt: usize) -> Self
    where T : Zero
  {
    let vec_len = self.length();
    Self::from_fn(vec_len, |i| {
      let j = amt + i;
      if j >= vec_len { T::zero(n) } else { self.index(j) } })
  }

  fn rotate_left(&self, amt: usize) -> Self {
    let n = self.length();
    Self::from_fn(n, |i| self.index((amt + i) % n))
  }
}



/* Formatting */

macro_rules! VectorFormatter {
  ( $($trait:ident),*) => { $(
    impl<T: fmt::$trait> fmt::$trait for Vector<T> {
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

VectorFormatter! { Display, Binary, Octal, UpperHex, LowerHex }








#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_basics() {
    let x = Vector::<u8>::from([1,2,3]);
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

