use crate::*;

fn front_index<I:Integral>(x: I::Arg<'_>) -> usize { I::to_usize(x) }

fn back_index<I:Integral>(len: usize, x: I::Arg<'_>) -> usize {
  len - 1 - front_index::<I>(x)
}

pub fn index_word<I>(xs: DWordRef<'_>, i: I::Arg<'_>) -> bool
  where I: Integral
{
  xs.index_msb(front_index::<I>(i))
}

pub fn index_word_back<I>(xs: DWordRef<'_>, i: I::Arg<'_>) -> bool
  where I: Integral
{
  xs.index_msb(back_index::<I>(xs.bits(),i))
}

pub fn index_vec<T,I>(xs: Vec<T>, i: I::Arg<'_>) -> T
  where T: Type, I: Integral
{
  xs.index(front_index::<I>(i))
}

pub fn index_vec_back<T,I>(xs: Vec<T>, i: I::Arg<'_>) -> T
  where T: Type, I: Integral
{
  xs.index(back_index::<I>(xs.len(),i))
}

fn stream_index<T: Type>(xs: impl Stream<T>, i: usize) -> T {
  xs.skip(i).next().unwrap()
}

pub fn index_stream<T,I>(xs: impl Stream<T>, i: I::Arg<'_>) -> T
  where T: Type, I: Integral
{
  stream_index(xs, front_index::<I>(i))
}


pub fn index_stream_back<T,I>
  (len: usize, xs: impl Stream<T>, i: I::Arg<'_>) -> T
  where T: Type, I: Integral
{
  stream_index(xs, back_index::<I>(len,i))
}


