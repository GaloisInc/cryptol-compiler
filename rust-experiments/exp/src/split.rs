enum SeqLength {
  WordSized(usize),
  FiniteArbitrary,
  Infinite
}

trait Seq<T> : IntoIterator<Item=T> {
  fn len(&self) -> SeqLength;
}

struct SmallWord {
  bits: u64,
  len: u8
}

impl Seq<bool> for SmallWord {

}

