use crate::*;

pub fn split<T:Type>(each: usize, xs: impl Stream<T>) -> impl Stream<Vec<T>> {

  #[derive(Clone)]
  struct S<I> { each: usize, xs: I, }

  impl<T, I:Iterator<Item=T>> Iterator for S<I> {
    type Item = Vec<T>;
    fn next (&mut self) -> Option<Self::Item> {
      let mut result = Vec::with_capacity(self.each);
      for _ in 0 .. self.each {
        result.push(self.xs.next()?)
      }
      Some(result)
    }
  }

  impl<I:Clone> CloneArg for S<I> {
    type Owned = Self;
    fn clone_arg(self) -> Self::Owned { self }
  }

  impl<I:Clone> Type for S<I> {
    type Arg<'a> = Self where Self: 'a;
    type Length = ();
    fn as_arg(&self) -> Self { self.clone() }
  }

  impl<T:Type,I:Clone+Iterator<Item=T>> Stream<Vec<T>> for S<I> {}

  S { each: each, xs: xs }
}

pub fn split_bits(each: usize, xs: impl Stream<bool>) -> impl Stream<DWord> {
  split(each,xs).map(move |v| DWord::from_stream_msb(each, v.into_iter()))
}
