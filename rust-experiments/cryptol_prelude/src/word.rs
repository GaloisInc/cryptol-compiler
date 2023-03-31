use apint::{ApInt, Width, BitWidth};


#[derive(Clone,PartialEq,Eq)]
pub enum Word {
  Empty,
  Bits(ApInt)
}

impl Word {

  pub fn from_u8(x : u8) -> Self {
    Word::Bits(ApInt::from_u8(x))
  }

  pub fn from_vec(xs: &Vec<bool>) -> Self {
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
