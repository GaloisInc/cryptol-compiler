


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



impl<T: crate::Type> crate::Stream<T> for std::vec::IntoIter<T> {}

impl<T: crate::Type> crate::Type for std::vec::IntoIter<T> {
  type Arg<'a> = Self where Self: 'a;
  type Length  = ();    // XXX: ???
  fn as_arg(&self) -> Self::Arg<'_> { self.clone() }
}

impl<T:crate::Type> crate::type_traits::CloneArg for std::vec::IntoIter<T> {
  type Owned = Self;
  fn clone_arg(self) -> Self::Owned { self }
}


