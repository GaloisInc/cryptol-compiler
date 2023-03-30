use apint::{BitWidth,Width,ApInt};
use num;
use num::ToPrimitive;
use std::borrow::Cow;

pub enum TNum {
  Inf,
  FinSize(usize),
  Fin(num::BigInt)
}

impl TNum {
  pub fn from_usize(x: usize) -> Self { TNum::FinSize(x) }
}

impl PartialEq for TNum {

  fn eq(&self, other: &TNum) -> bool {
    match (self, other) {
      (TNum::Inf,TNum::Inf) => true,
      (_,TNum::Inf) | (TNum::Inf,_) => false,

      (TNum::FinSize(x), TNum::FinSize(y)) => x == y,
      (TNum::Fin(x), TNum::Fin(y)) => x == y,

      (TNum::FinSize(x), TNum::Fin(y)) |
        (TNum::Fin(y), TNum::FinSize(x)) => Some(*x) == y.to_usize()
    }
  }
}

impl Eq for TNum {}

//------------------------------------------------------------------------------

#[derive(Clone,PartialEq,Eq)]
pub enum Word {
  Empty,
  Bits(ApInt)
}


impl Word {

  pub fn from_u8(x : u8) -> Self {
    Word::Bits(ApInt::from_u8(x))
  }

  pub fn len(&self) -> usize {
    match self {
      Word::Empty => 0,
      Word::Bits(xs) => xs.width().to_usize()
    }
  }

  fn as_apint(&self) -> &ApInt {
    match self {
      Word::Bits(w) => w,
      Word::Empty   => panic!("as_bits: called on Empty")
    }
  }

  pub fn iter(&self) -> WordIterator {
    WordIterator { word: self, index: self.len() }
  }

}


pub struct WordIterator<'a> {
  word:  &'a Word,
  index: usize
}

impl<'a> Iterator for WordIterator<'a> {
  type Item = bool;
  fn next(&mut self) -> Option<Self::Item> {
    if self.index == 0 { return None }
    self.index -= 1;
    Some(self.word.as_apint().get_bit_at(self.index).unwrap().to_bool())
  }
}


impl std::ops::Add for &Word {
  type Output = Word;
  fn add(self, rhs: &Word) -> Word {
    match self {
      Word::Empty    => Word::Empty,
      Word::Bits(wx) => Word::Bits(wx + rhs.as_apint())
    }
  }
}


// -----------------------------------------------------------------------------
// Types that may be stored in sequences.

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


//------------------------------------------------------------------------------

pub trait CryStream<A> {
  fn iter(&self) -> Box<dyn Iterator<Item=A>>;
}


pub enum Seq<A> {
  Vector(Vec<A>),
  Word(Word),
  Stream(Box<dyn CryStream<A>>)
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

fn pack_bool_vec (xs : &Vec<bool>) -> Word {
  match BitWidth::new(xs.len()) {
    Err(_) => Word::Empty,
    Ok(w) => {
      let mut res = ApInt::zero(w);
      let n = w.to_usize();
      for i in 0 .. n {
        if xs[n - i - 1] { res.set_bit_at(i); }
      }
      Word::Bits(res)
    }
  }
}

impl Seq<bool> {
  pub fn pack (&self) -> Cow<Word> {
    match self {
      Seq::Vector(v)  => Cow::Owned(pack_bool_vec(v)),
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




#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_length() {
    let xs = vec![false,true];
    let s  = Seq::Vector(xs);
    let w  = s.pack();
    for x in w.iter() {
      eprint!("{}",x);
    }
  }

}


// -----------------------------------------------------------------------------

pub struct CryNats {
  pub start: u8
}

struct CryNatsIterator {
  x: u8
}

impl Iterator for CryNatsIterator {
  type Item = u8;
  fn next(&mut self) -> Option<Self::Item> {
    let res = self.x;
    self.x = self.x.wrapping_add(1);
    Some(res)
  }
}

impl CryStream<u8> for CryNats {
  fn iter(&self) -> Box<dyn Iterator<Item=u8>> {
    Box::new(CryNatsIterator { x: self.start })
  }
}








