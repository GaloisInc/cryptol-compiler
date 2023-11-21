
pub fn from_to_usize<T: crate::Literal>
  (len: T::Length, from: usize, to: usize) -> impl crate::Stream<T> {
  (from .. to+1).map(move |x| <T>::number_usize(len,x))
}

#[derive(Clone)]
struct FromTo {
  current: num::BigUint,
  last:    num::BigUint
}

impl Iterator for FromTo {
  type Item = num::BigUint;
  fn next(&mut self) -> Option<Self::Item> {
    if self.current > self.last { return None }
    let result = self.current.clone();
    self.current += 1u64;
    Some(result)
  }
}

pub fn from_to_uint<T: crate::Literal>
  (len: T::Length, from: num::BigUint, to: num::BigUint) ->
    impl crate::Stream<T> {
  FromTo { current: from, last: to }.map(move |x| <T>::number_uint(len,&x))
}


