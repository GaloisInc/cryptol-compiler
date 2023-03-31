use std::borrow::Cow;

use crate::{word::Word, tnum::TNum, stream::Stream};

pub enum Seq<A> {
  Vector(Vec<A>),
  Word(Word),
  Stream(Stream<A>)
}

impl<A> Seq<A> {
  pub fn len (&self) -> TNum {
    match self {
      Seq::Vector(v)  => TNum::from_usize(v.len()),
      Seq::Word(w)    => TNum::from_usize(w.len()),
      Seq::Stream(_)  => TNum::Inf
    }
  }
}


impl Seq<bool> {
  pub fn pack (&self) -> Cow<Word> {
    match self {
      Seq::Vector(v)  => Cow::Owned(Word::from_vec(v)),
      Seq::Word(w)    => Cow::Borrowed(w),
      Seq::Stream(_)  => panic!("try_pack on Stream")
    }
  }
}

fn as_word<A : SeqElem>(xs: &Seq<A>) -> Option< Cow<Word> > {
  Some (A::as_bool_seq_ref(xs)?.pack())
}

impl<A:SeqElem> std::ops::Add for &Seq<A> {
  type Output = Seq<A>;

  fn add(self, y: Self) -> Self::Output {

    match as_word(self) {
      Some(xw) => {
        let x : &Word = &xw;
        let y : &Word = &as_word(y).unwrap();
        Seq::Word(x + y)
      }

      None => {
        match self {
          Seq::Vector(xs) => todo!(),
          Seq::Stream(xs) => todo!(),
          Seq::Word(_) => panic!("seq_add word when element is not `bool`")
        }
      }

    }
  }


}


// ----------------------------------------------------------------------------

pub trait SeqElem : Sized {
  const IS_BOOL : bool;
  fn as_bool_seq(xs: Seq<Self>) -> Option<Seq<bool>>;
  fn as_bool_seq_ref(xs: & Seq<Self>) -> Option< &Seq<bool> >;
}

macro_rules! not_bool_body {
  {} => {
    const IS_BOOL : bool = false;
    fn as_bool_seq(xs: Seq<Self>) -> Option<Seq<bool>> { None }
    fn as_bool_seq_ref(xs: &Seq<Self>) -> Option< &Seq<bool> > { None }
  }
}

macro_rules! not_bool {
  ($t:ident<$a:ident>) => { impl<$a> SeqElem for $t<$a> { not_bool_body!{} } };
  ($t:ty) => { impl SeqElem for $t { not_bool_body!{} } }
}

impl SeqElem for bool  {
  const IS_BOOL : bool = true;
  fn as_bool_seq(xs: Seq<Self>) -> Option<Seq<bool>> { Some(xs) }
  fn as_bool_seq_ref(xs: &Seq<Self>) -> Option< &Seq<bool> > { Some(xs) }
}

not_bool!(Word);
not_bool!(());
not_bool!(Seq<A>);