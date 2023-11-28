use crate::*;

/// [x][y]a -> [y][x]a
pub fn transpose_word(x: usize, y: usize, v: &[DWord]) -> Vec<DWord> {
  let mut result = Vec::with_capacity(y);
  for i in 0 .. y {
    struct S<'a> {
      v: &'a [DWord],
      i: usize,
      j: usize
    }
    impl<'a> Iterator for S<'a> {
      type Item = bool;
      fn next(&mut self) -> Option<bool> {
        Some(self.v[self.j].as_ref().index_msb(self.i))
      }
    }
    result.push(DWord::from_stream_msb(x, S{v: v, i: i, j: 0}))
  }
  result
}


/// [x][y]a -> [y][x]a
pub fn transpose_vec<T:Type>(x: usize, y: usize, v: &[Vec<T>]) -> Vec<Vec<T>> {
  let mut result = Vec::with_capacity(y);
  for i in 0 .. y {
    let mut element = Vec::with_capacity(x);
    for j in 0 .. x {
      element.push(v[j][i].clone())
    }
    result.push(element)
  }
  result
}
