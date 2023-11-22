use crate::type_traits::*;


#[macro_export]
macro_rules! stream {
  ( forall  = [ $( $t:ident : [ $($trait:path),* ] ),* ]
  , element = $elT:ty
  , history = $history:literal
  , capture = [ $( $field:ident : $type:ty = $field_value:expr),* ]
  , init    = $init:expr
  , step    = |$self:ident| $next:expr
  ) => {
    {
      #[derive(Clone)]
      struct S<$($t,)*>
        where
        $($t : $crate::type_traits::Type,)*
      {
        index:    usize,
        history:  [ $elT; $history ],
        $( $field: $type, )*
      }

      impl<$($t,)*>  S<$($t,)*>
        where
        $( $( $t : $trait ,)* )* {
        fn get_history(&self, i: usize) -> $elT {
          self.history[ (self.index - i - 1) % $history ].clone()
        }
      }


      impl <$($t,)*> Iterator for S<$($t,)*>
        where
        $( $( $t : $trait ,)* )*
      {
        type Item = $elT;
        fn next(&mut self) -> Option<Self::Item> {
          if self.index < $history {
            self.index += 1;
            Some(self.history[self.index-1].clone())
          } else {
            let $self = &mut (*self);
            let result: $elT = $next;
            self.history[self.index % $history] = result.clone();
            self.index += 1;
            Some(result)
          }
        }
      }

      impl<$($t,)*> $crate::type_traits::Type for S<$($t,)*>
        where $( $( $t : $trait ,)* )*
      { type Arg<'a> = Self where Self: 'a;
        type Length  = ();    // XXX: ???
        fn as_arg(&self) -> Self::Arg<'_> { self.clone() }
      }

      impl<$($t,)*> $crate::type_traits::CloneArg for S<$($t,)*>
        where $( $( $t : $trait ,)* )*
      {
        type Owned = Self;
        fn clone_arg(self) -> Self::Owned { self }
      }

      impl<$($t,)*> $crate::type_traits::Stream<$elT> for S<$($t,)*>
        where
        $( $( $t : $trait ,)* )*
      { }


      S { index: 0
        , history: $init.try_into().ok().unwrap()
        , $($field: $field_value,)* }
    }
  };
}



impl<T: Type> Stream<T> for std::vec::IntoIter<T> {}

impl<T: Type> Type for std::vec::IntoIter<T> {
  type Arg<'a> = Self where Self: 'a;
  type Length  = ();    // XXX: ???
  fn as_arg(&self) -> Self::Arg<'_> { self.clone() }
}

impl<T:Clone> CloneArg for std::vec::IntoIter<T> {
  type Owned = Self;
  fn clone_arg(self) -> Self::Owned { self }
}

impl<T: Type, I, F> Stream<T> for std::iter::Map<I,F>
  where
  I: Clone + Iterator,
  F: Clone + FnMut(<I as Iterator>::Item) -> T,
  {}

impl<I:Clone,F:Clone> Type for std::iter::Map<I,F> {
  type Arg<'a> = Self where Self: 'a;
  type Length  = ();    // XXX: ???
  fn as_arg(&self) -> Self::Arg<'_> { self.clone() }
}

impl<I:Clone,F:Clone> CloneArg for std::iter::Map<I,F> {
  type Owned = Self;
  fn clone_arg(self) -> Self::Owned { self }
}


impl<I,J> Stream<(<I as Iterator>::Item,<J as Iterator>::Item)>
  for std::iter::Zip<I,J>
  where
  <I as Iterator>::Item : Type,
  <J as Iterator>::Item : Type,
  I: Clone + Iterator,
  J: Clone + Iterator
  {}

impl<I:Clone,J:Clone> Type for std::iter::Zip<I,J> {
  type Arg<'a> = Self where Self: 'a;
  type Length  = ();    // XXX: ???
  fn as_arg(&self) -> Self::Arg<'_> { self.clone() }
}

impl<I:Clone,J:Clone> CloneArg for std::iter::Zip<I,J> {
  type Owned = Self;
  fn clone_arg(self) -> Self::Owned { self }
}



pub
fn cry_map<'a, A,B,F,I>(f: F, xs: I) -> impl Stream<B> + 'a
  where
  A: Type,
  B: Type,
  F: Fn(A::Arg<'_>) -> B,
  F: Clone,
  F: 'a,
  I: Stream<A>,
  I: 'a
{
  xs.map(move |v| { f(v.as_arg()) })
}



