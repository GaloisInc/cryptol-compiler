use std::fmt;
use crate::traits::*;
use crate::type_traits::*;
use crate::display::Base;

// Convenince for making vectors from a function.
pub trait FromFn<T> {
  fn from_fn<F>(n: usize, f: F) -> Self
    where F: FnMut(usize) -> T;
}

impl<T> FromFn<T> for Vec<T>{
  fn from_fn<F>(n: usize, mut f: F) -> Self
    where F: FnMut(usize) -> T
    { let mut i = 0;
      std::iter::from_fn(|| if i < n {
                              let j = i;
                              i += 1;
                              Some (f(j))
                            } else { None }
                       ).collect()
    }
}

/* Crytpol type */
impl<T: Type> Type for Vec<T> {
  type Length = (usize, T::Length);
  type Arg<'a> = &'a [T] where T: 'a;
  fn as_arg(&self) -> Self::Arg<'_> { &self[..] }
}

impl<T: Type> CloneArg for &[T] {
  type Owned = Vec<T>;
  fn clone_arg(self) -> Self::Owned {
    let mut result = Vec::with_capacity(self.len());
    result.extend_from_slice(self);
    result
  }
}

impl<T:Zero> Zero for Vec<T> {
  fn zero((vec_len,elem_len): Self::Length) -> Self {
    Self::from_fn(vec_len as usize,|_| T::zero(elem_len))
  }
}

/* Sequence operations */

impl<T: Type> Sequence for &[T] {
  type Item = T;

  fn length(self) -> usize { self.len() }

  fn index(self, i: usize) -> T { self[i].clone() }

  fn shift_right(self, n: T::Length, amt: usize) -> Self::Owned
    where T : Zero
  {
    Vec::<T>::from_fn( self.length()
                 , |i| if i < amt { T::zero(n) } else { self.index(i-amt) })
  }


  fn shift_right_signed(self, amt: usize) -> Self::Owned {
    Vec::<T>::from_fn( self.length()
                 , |i| self.index(if i < amt { 0 } else { i - amt }))
  }

  fn rotate_right(self, amt: usize) -> Self::Owned {
    let n = self.length();
    if n == 0 { return self.clone_arg() };
    let a = amt % n;
    Vec::<T>::from_fn(n, |i| self.index((n + i - a) % n))
  }


  fn shift_left(self, n: T::Length, amt: usize) -> Self::Owned
    where T : Zero
  {
    let vec_len = self.length();
    Vec::<T>::from_fn(vec_len, |i| {
      let j = amt + i;
      if j >= vec_len { T::zero(n) } else { self.index(j) } })
  }

  fn rotate_left(self, amt: usize) -> Self::Owned {
    let n = self.length();
    Vec::<T>::from_fn(n, |i| self.index((amt + i) % n))
  }
}


impl<const BASE: usize, const UPPER: bool, T: Base<BASE, UPPER>> Base<BASE, UPPER> for Vec<T> {
  fn format(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    Base::<BASE, UPPER>::format(&self.as_slice(),fmt)
  }
}

impl<const BASE: usize, const UPPER: bool, T: Base<BASE, UPPER>> Base<BASE, UPPER> for &[T] {
  fn format(&self, fmt: &mut fmt::Formatter) -> fmt::Result
  {
    write!(fmt,"[")?;
    let mut xs = self.into_iter();
    match xs.next() {
      Some(fst) => Base::<BASE, UPPER>::format(fst,fmt)?,
      None      => { return write!(fmt,"]") }
    }

    for i in xs {
      write!(fmt,", ")?;
      Base::<BASE, UPPER>::format(i,fmt)?;
    }
    write!(fmt, "]")
  }
}

pub fn reverse<T:Type>(mut xs: Vec<T>) -> Vec<T> {
  xs.reverse();
  xs
}


impl<T: Logic> Logic for Vec<T> {
  fn complement(x: Self::Arg<'_>) -> Self {
    x.iter().map(|a| T::complement(a.as_arg())).collect()
  }
  fn xor(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self {
    x.iter().zip(y.iter()).map(|(a,b)| T::xor(a.as_arg(),b.as_arg())).collect()
  }
  fn and(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self {
    x.iter().zip(y.iter()).map(|(a,b)| T::and(a.as_arg(),b.as_arg())).collect()
  }
  fn or (x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self {
    x.iter().zip(y.iter()).map(|(a,b)| T::or(a.as_arg(),b.as_arg())).collect()
  }
}


impl<T: Ring> Ring for Vec<T> {

  fn negate(x: Self::Arg<'_>) -> Self {
    x.iter().map(|a| T::negate(a.as_arg())).collect()
  }

  fn mul(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self {
    x.iter().zip(y.iter()).map(|(a,b)| T::mul(a.as_arg(),b.as_arg())).collect()
  }

  fn sub(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self {
    x.iter().zip(y.iter()).map(|(a,b)| T::sub(a.as_arg(),b.as_arg())).collect()
  }

  fn add(x: Self::Arg<'_>, y: Self::Arg<'_>) -> Self {
    x.iter().zip(y.iter()).map(|(a,b)| T::add(a.as_arg(),b.as_arg())).collect()
  }

  fn from_integer(n: Self::Length, x: &num::BigInt) -> Self {
    let r = T::from_integer(n.1, x);
    Self::from_fn(n.0, |_i| r.clone())
  }

  fn exp_usize(x: Self::Arg<'_>, y: usize) -> Self {
    x.iter().map(|a| T::exp_usize(a.as_arg(), y)).collect()
  }

}





#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_basics() {
    let x = vec![1_u8,2,3];
    assert_eq!(x.length(), 3);
    assert_eq!(x.index(2), 3);

    assert_eq!(x.shift_left((),1), vec![2,3,0]);
    assert_eq!(x.shift_left((),0), vec![1,2,3]);
    assert_eq!(x.shift_left((),10), vec![0,0,0]);

    assert_eq!(x.shift_right((),1), vec![0,1,2]);
    assert_eq!(x.shift_right((),0), vec![1,2,3]);
    assert_eq!(x.shift_right((),10), vec![0,0,0]);

    assert_eq!(x.shift_right_signed(1), vec![1,1,2]);
    assert_eq!(x.shift_right_signed(0), vec![1,2,3]);
    assert_eq!(x.shift_right_signed(10), vec![1,1,1]);

    assert_eq!(x.rotate_left(1), vec![2,3,1]);
    assert_eq!(x.rotate_left(0), vec![1,2,3]);
    assert_eq!(x.rotate_left(11), vec![3,1,2]);

    assert_eq!(x.rotate_right(1), vec![3,1,2]);
    assert_eq!(x.rotate_right(0), vec![1,2,3]);
    assert_eq!(x.rotate_right(11), vec![2,3,1]);

  }
}
