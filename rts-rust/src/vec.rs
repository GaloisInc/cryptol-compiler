use crate::traits::*;
use std::fmt;

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



/* Lenght and Zero */
impl<T:Length> Length for Vec<T> {
  type Length = (u64, T::Length);
}

impl<T:Zero> Zero for Vec<T> {
  fn zero((vec_len,elem_len): Self::Length) -> Self {
    Self::from_fn(vec_len as usize,|_| T::zero(elem_len))
  }
}

/* Sequence operations */

impl<T : Clone> Sequence for Vec<T> {
  type Item = T;

  fn length(&self) -> usize { self.len() }

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


impl<T: crate::Display> crate::Display for Vec<T> {
  fn display(&self, base: usize, fmt: &mut fmt::Formatter) -> fmt::Result
  { self.as_slice().display(base,fmt) }
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
