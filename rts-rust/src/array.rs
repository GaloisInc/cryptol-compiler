use crate::traits::*;

impl<T : Clone, const N: usize> Sequence for [T; N] {
  type Item = T;

  fn cry_length(&self) -> usize { N }

  fn cry_index(&self, i: usize) -> T { self[i].clone() }

  fn cry_shift_right(&self, n: <T as Zero>::Length, amt: usize) -> Self
    where T : Zero
  {
    std::array::from_fn(|i| if i < amt { T::zero(n) }
                            else { self.cry_index(i-amt) })
  }


  fn cry_shift_right_signed(&self, amt: usize) -> Self {
    std::array::from_fn(|i| self.cry_index(if i < amt { 0 } else { i - amt }))
  }

  fn cry_rotate_right(&self, amt: usize) -> Self {
    if N == 0 { return self.clone() };
    let a = amt % N;
    std::array::from_fn(|i| self.cry_index((N + i - a) % N))
  }


  fn cry_shift_left(&self, n: <T as Zero>::Length, amt: usize) -> Self
    where T : Zero
  {
    std::array::from_fn(|i| {
      let j = amt + i;
        if j >= N { T::zero(n) } else { self.cry_index(j) } })
  }

  fn cry_rotate_left(&self, amt: usize) -> Self {
    std::array::from_fn(|i| self.cry_index((amt + i) % N))
  }
}


#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_basics() {
    let x : [u8;3] = [1,2,3];
    assert_eq!(x.cry_length(), 3);
    assert_eq!(x.cry_index(2), 3);

    assert_eq!(x.cry_shift_left((),1), [2,3,0]);
    assert_eq!(x.cry_shift_left((),0), [1,2,3]);
    assert_eq!(x.cry_shift_left((),10), [0,0,0]);

    assert_eq!(x.cry_shift_right((),1), [0,1,2]);
    assert_eq!(x.cry_shift_right((),0), [1,2,3]);
    assert_eq!(x.cry_shift_right((),10), [0,0,0]);

    assert_eq!(x.cry_shift_right_signed(1), [1,1,2]);
    assert_eq!(x.cry_shift_right_signed(0), [1,2,3]);
    assert_eq!(x.cry_shift_right_signed(10), [1,1,1]);

    assert_eq!(x.cry_rotate_left(1), [2,3,1]);
    assert_eq!(x.cry_rotate_left(0), [1,2,3]);
    assert_eq!(x.cry_rotate_left(11), [3,1,2]);

    assert_eq!(x.cry_rotate_right(1), [3,1,2]);
    assert_eq!(x.cry_rotate_right(0), [1,2,3]);
    assert_eq!(x.cry_rotate_right(11), [2,3,1]);

  }



}
