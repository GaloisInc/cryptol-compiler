use crate::type_traits::*;
use crate::display::*;
use std::fmt;

#[macro_export]
macro_rules! tuple_type {
  ($($t:ident),*) => {

    impl<$($t: Type),*> Type for ($($t),*) {
      type Arg<'a> = &'a ($($t),*) where Self: 'a;
      type Length = ($(<$t>::Length),*);
      fn as_arg(&self) -> Self::Arg<'_> { self }
    }

    impl<$($t: Type),*> CloneArg for &($($t),*) {
      type Owned = ($($t),*);
      fn clone_arg(self) -> Self::Owned { self.clone() }
    }

    impl<const BASE: usize, const UPPER: bool, $($t),*>
      Base<BASE, UPPER> for ($($t),*)
      where $($t: Base<BASE, UPPER>),* {

      #[allow(unused)]
      fn format(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt,"(")?;
        let mut is_first = true;

        #[allow(bad_style)]
        let ($($t),*) = self;

        $(
          if is_first { is_first = false } else { write!(fmt,",")? };
          Base::<BASE,UPPER>::format($t,fmt)?;
        )*

        write!(fmt,")")
      }
   }


  };
}

/*  2 */ tuple_type!(A,B);
/*  3 */ tuple_type!(A,B,C);
/*  4 */ tuple_type!(A,B,C,D);
/*  5 */ tuple_type!(A,B,C,D,E);
/*  6 */ tuple_type!(A,B,C,D,E,F);
/*  7 */ tuple_type!(A,B,C,D,E,F,G);
/*  8 */ tuple_type!(A,B,C,D,E,F,G,H);
/*  9 */ tuple_type!(A,B,C,D,E,F,G,H,I);
/* 10 */ tuple_type!(A,B,C,D,E,F,G,H,I,J);
/* 11 */ tuple_type!(A,B,C,D,E,F,G,H,I,J,K);
/* 12 */ tuple_type!(A,B,C,D,E,F,G,H,I,J,K,L);
/* 13 */ tuple_type!(A,B,C,D,E,F,G,H,I,J,K,L,M);
/* 14 */ tuple_type!(A,B,C,D,E,F,G,H,I,J,K,L,M,N);
/* 15 */ tuple_type!(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O);
/* 16 */ tuple_type!(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P);

