use crate::traits::*;
use crate::type_traits::*;
use crate::*;

// -----------------------------------------------------------------------------
// fromTo

pub fn from_to_usize<T: Literal>
  (len: T::Length, from: usize, to: usize) -> impl Stream<T> {
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

pub fn from_to_uint<T: Literal>
  (len: T::Length, from: num::BigUint, to: num::BigUint) -> impl Stream<T> {
  FromTo { current: from, last: to }.map(move |x| <T>::number_uint(len,&x))
}


// -----------------------------------------------------------------------------
// infFrom and infFromThen


#[derive(Clone)]
struct InfFromThen {
  current: num::BigInt,
  step:    num::BigInt
}

impl Iterator for InfFromThen {
  type Item = num::BigInt;
  fn next(&mut self) -> Option<num::BigInt> {
    let result = self.current.clone();
    self.current += &self.step;
    Some(result)
  }
}

pub fn inf_from_then<T:Integral>
  (len: T::Length, start: T::Arg<'_>, next: T::Arg<'_>) -> impl Stream<T> {
  let start_i = T::to_integer(start);
  let step   = T::to_integer(next) - &start_i;
  InfFromThen { current: start_i, step: step }
    .map(move |x| <T>::from_integer(len, &x))
}

pub fn inf_from<T:Integral>
  (len: T::Length, start: T::Arg<'_>) -> impl Stream<T> {

  InfFromThen {
    current: T::to_integer(start),
    step: 1u8.into()
  }.map(move |x| <T>::from_integer(len, &x))
}





// ------------------------------------------------------------------------------
// Join helpers

pub fn join_words(xs: impl Stream<DWord>) -> impl Stream<bool> {
  xs.flat_map(move |w| w.into_iter_msb())
}

pub fn join_vecs<T: Type>(xs: impl Stream<Vec<T>>) -> impl Stream<T> {
  xs.flat_map(move |w| w.into_iter())
}
