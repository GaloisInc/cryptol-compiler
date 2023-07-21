use crate::traits::*;

impl<T:Length> Length for Vec<T> {
  type Length = (u64, T::Length);
}

impl<T:Zero> Zero for Vec<T> {
  fn zero((vec_len,elem_len): Self::Length) -> Self {
    let mut todo = vec_len as usize;
    std::iter::from_fn(|| if todo > 0 {
                            todo -= 1;
                            Some (T::zero(elem_len))
                          } else { None }
                       ).collect()
  }
}
