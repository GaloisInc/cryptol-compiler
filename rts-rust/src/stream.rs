use crate::type_traits::*;


#[macro_export]
#[allow(bad_style)]
macro_rules! stream {
  ( forall  = [ $( $t:ident : [ $($trait:path),* ] ),* ]
  , element = $elT:ty
  , history = $history:literal
  , init    = $init:expr
  , capture = [ $( $field:ident : $type:ty = $field_value:expr),* ]
  , step    = |$self:ident| $next:expr
  ) => {
    {
      #[derive(Clone)]
      #[allow(non_snake_case)]
      struct S<$($t,)*> where $($t: $crate::type_traits::Type),*
      {
        index:    usize,
        history:  [ $elT; $history ],
        $( $field: $type, )*
        $( $t: std::marker::PhantomData::<$t>, )*
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
        , $($field: $field_value,)*
        $( $t: std::marker::PhantomData, )*
        }
    }
  };

  ( forall  = [ $( $t:ident : [ $($trait:path),* ] ),* ]
  , element = $elT:ty
  , capture = [ $( $field:ident : $type:ty = $field_value:expr),* ]
  , step    = |$self:ident| $next:expr
  ) => {
    {
      #[derive(Clone)]
      #[allow(non_snake_case)]
      struct S<$($t,)*> where $($t: $crate::type_traits::Type),*
      {
          $( $field: $type, )*
          $( $t: std::marker::PhantomData::<$t>, )*
      }

      impl <$($t,)*> Iterator for S<$($t,)*>
        where
        $( $( $t : $trait ,)* )*
      {
        type Item = $elT;
        fn next(&mut self) -> Option<Self::Item> {
          let $self = &mut (*self);
          Some($next)
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

      S { $($field: $field_value,)*
          $( $t: std::marker::PhantomData, )*
        }
    }
  };
}




/* -----------------------------------------------------------------------------
IntoIter
----------------------------------------------------------------------------- */

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


/* -----------------------------------------------------------------------------
Chain
----------------------------------------------------------------------------- */
impl<I:Clone,J:Clone> CloneArg for std::iter::Chain<I,J> {
  type Owned = Self;
  fn clone_arg(self) -> Self::Owned { self }
}

impl<I:Clone,J:Clone> Type for std::iter::Chain<I,J> {
  type Arg<'a> = Self where Self: 'a;
  type Length  = ();    // XXX: ???
  fn as_arg(&self) -> Self::Arg<'_> { self.clone() }
}

impl<T: Type, I, J> Stream<T> for std::iter::Chain<I,J>
  where
    I: Clone + Iterator<Item=T>,
    J: Clone + Iterator<Item=T>
    {}




/* -----------------------------------------------------------------------------
Map
----------------------------------------------------------------------------- */


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


// -----------------------------------------------------------------------------
// Take
// -----------------------------------------------------------------------------

impl <T,I> Stream<T> for std::iter::Take<I>
  where T: Type, I: Clone + Iterator<Item=T>
  {}

impl <I:Clone> Type for std::iter::Take<I> {
  type Arg<'a> = Self where Self: 'a;
  type Length = ();
  fn as_arg(&self) -> Self::Arg<'_> { self.clone() }
}

impl <I:Clone> CloneArg for std::iter::Take<I> {
  type Owned = Self;
  fn clone_arg(self) -> Self::Owned { self }
}


// -----------------------------------------------------------------------------
// Skip
// -----------------------------------------------------------------------------

impl <T,I> Stream<T> for std::iter::Skip<I>
  where T: Type, I: Clone + Iterator<Item=T>
  {}

impl <I:Clone> Type for std::iter::Skip<I> {
  type Arg<'a> = Self where Self: 'a;
  type Length = ();
  fn as_arg(&self) -> Self::Arg<'_> { self.clone() }
}

impl <I:Clone> CloneArg for std::iter::Skip<I> {
  type Owned = Self;
  fn clone_arg(self) -> Self::Owned { self }
}





/* -----------------------------------------------------------------------------
Zip
----------------------------------------------------------------------------- */

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



/* -----------------------------------------------------------------------------
FlatMap
----------------------------------------------------------------------------- */


impl<I, U, F> Stream<<U as Iterator>::Item> for std::iter::FlatMap<I,U,F>
  where
  I: Clone + Iterator,
  U: Clone + Iterator,
  <U as Iterator>::Item: Type,
  F: Clone + FnMut(<I as Iterator>::Item) -> U,
  {}

impl<I,U,F> Type for std::iter::FlatMap<I,U,F>
  where
  I: Clone,
  U: Clone + Iterator,
  F: Clone
{
  type Arg<'a> = Self where Self: 'a;
  type Length  = ();    // XXX: ???
  fn as_arg(&self) -> Self::Arg<'_> { self.clone() }
}

impl<I,U,F> CloneArg for std::iter::FlatMap<I,U,F>
  where
  I: Clone,
  U: Clone + Iterator,
  F: Clone
{
  type Owned = Self;
  fn clone_arg(self) -> Self::Owned { self }
}

pub
fn cry_flat_map<'a, A,B,F,I,J>(f: F, xs: I) -> impl Stream<B> + 'a
  where
  A: Type,
  B: Type + 'a,
  F: Fn(A) -> J,
  F: Clone + 'a,
  I: Stream<A> + 'a,
  J: Stream<B> + 'a,
{
  xs.flat_map(move |v| { f(v) })
}




