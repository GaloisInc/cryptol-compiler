use crate::traits::*;
use crate::type_traits::*;
use crate::*;

// Iterator for going up
#[derive(Clone)]
struct FromTo<const CLO: bool> {
  current: num::BigUint,
  step:    num::BigUint,
  last:    num::BigUint
}

impl<const CLO: bool> Iterator for FromTo<CLO> {
  type Item = num::BigUint;
  fn next(&mut self) -> Option<Self::Item> {
    let end = if CLO { self.current > self.last } else
                     { self.current >= self.last };
    if end { return None }
    let result = self.current.clone();
    self.current += &self.step;
    Some(result)
  }
}


// Iterator for going down
#[derive(Clone)]
struct FromDownTo<const CLO: bool> {
  current: num::BigUint,
  step:    num::BigUint,
  last:    num::BigUint
}

// We assume that both `current` and `last` start off incremented by a `step` 
impl<const CLO: bool> Iterator for FromDownTo<CLO> {
  type Item = num::BigUint;
  fn next(&mut self) -> Option<Self::Item> {
    let end = if CLO { self.current < self.last }
                else { self.current <= self.last };
    if end { return None }
    self.current -= &self.step;
    Some(self.current.clone())
  }
}



// -----------------------------------------------------------------------------
// fromTo and friends (usize versions)
//
pub fn from_to_usize<T: Literal>
  (len: T::Length, from: usize, to: usize) -> impl Stream<T> {
  (from ..= to).map(move |x| <T>::number_usize(len,x))
}

pub fn from_to_less_than_usize<T: Literal>
  (len: T::Length, from: usize, to: usize) -> impl Stream<T> {
  (from .. to).map(move |x| <T>::number_usize(len,x))
}

pub fn from_to_by_usize<T: Literal>
  (len: T::Length, from: usize, to: usize, step: usize) -> impl Stream<T> {
  (from ..= to).step_by(step).map(move |x| <T>::number_usize(len,x))
}

pub fn from_to_by_less_than_usize<T: Literal>
  (len: T::Length, from: usize, to: usize, step: usize) -> impl Stream<T> {
  (from .. to).step_by(step).map(move |x| <T>::number_usize(len,x))
}

// XXX: functions for going down?





// -----------------------------------------------------------------------------
// fromTo and friends (big num versions)

pub fn from_to_uint<T: Literal>
  (len: T::Length, from: num::BigUint, to: num::BigUint) -> impl Stream<T> {
  FromTo::<true> { current: from, step: 1u8.into(), last: to }
  .map(move |x| <T>::number_uint(len,&x))
}

pub fn from_to_less_than_uint<T: Literal>
  (len: T::Length, from: num::BigUint, to: num::BigUint) -> impl Stream<T> {
  FromTo::<false> { current: from, step: 1u8.into(), last: to }
  .map(move |x| <T>::number_uint(len,&x))
}

pub fn from_to_by_uint<T: Literal>
  (len: T::Length, from: num::BigUint, to: num::BigUint, step: num::BigUint) ->
                                                              impl Stream<T> {
  FromTo::<true> { current: from, step: step, last: to }
  .map(move |x| <T>::number_uint(len,&x))
}

pub fn from_to_by_less_than_uint<T: Literal>
  (len: T::Length, from: num::BigUint, to: num::BigUint, step: num::BigUint) ->
                                                              impl Stream<T> {
  FromTo::<false> { current: from, step: step, last: to }
  .map(move |x| <T>::number_uint(len,&x))
}

pub fn from_to_down_by_uint<T: Literal>
  (len: T::Length, from: num::BigUint, to: num::BigUint, step: num::BigUint) ->
                                                              impl Stream<T> {
  let start = from + &step;
  let end   = to   + &step;
  FromDownTo::<true> { current: start, step: step, last: end }
  .map(move |x| <T>::number_uint(len,&x))
}

pub fn from_to_down_by_greater_than_uint<T: Literal>
  (len: T::Length, from: num::BigUint, to: num::BigUint, step: num::BigUint) ->
                                                              impl Stream<T> {
  let start = from + &step;
  let end   = to   + &step;
  FromDownTo::<false> { current: start, step: step, last: end }
  .map(move |x| <T>::number_uint(len,&x))
}


// XXX: from_then_to
// This one is tricky, as above we use different types of iterator for
// incrementing or decrementing, but this one needs to make the decision
// based on a dynamic value.








// -----------------------------------------------------------------------------
// infFrom and infFromThen
// Note that these are ont Integers (signed)

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
  xs.flat_map(move |w| w.into_iter_bits_msb())
}

pub fn join_vecs<T: Type>(xs: impl Stream<Vec<T>>) -> impl Stream<T> {
  xs.flat_map(move |w| w.into_iter())
}
