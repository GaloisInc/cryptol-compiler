
use crate::traits::*;

impl Length for u8   { type Length = (); }
impl Length for u16  { type Length = (); }
impl Length for u32  { type Length = (); }
impl Length for u64  { type Length = (); }
impl Length for u128 { type Length = (); }


impl Zero for u8   { fn zero(_: ()) -> Self { 0 } }
impl Zero for u16  { fn zero(_: ()) -> Self { 0 } }
impl Zero for u32  { fn zero(_: ()) -> Self { 0 } }
impl Zero for u64  { fn zero(_: ()) -> Self { 0 } }
impl Zero for u128 { fn zero(_: ()) -> Self { 0 } }

impl Sequence for u8 {
  type Item = bool;
  fn cry_length(&self) -> usize     { u8::BITS as usize }
  fn cry_index(&self, i: usize) -> Self::Item {
    ((self >> (self.cry_length() - i - 1)) & 1) == 1
  }

  fn cry_shift_left(&self,_:(),i:usize) -> Self {
    if i >= self.cry_length() { 0 } else { self << i }
  }

  fn cry_shift_right(&self,_:(),i:usize) -> Self {
    if i >= self.cry_length() { 0 } else { self >> i }
  }

  fn cry_shift_right_signed(&self,i:usize) -> Self {
    let j = *self as i8;
    if i >= self.cry_length() { if j < 0 { u8::MAX } else { 0 } }
    else { (j >> i) as u8 }
  }

  fn cry_rotate_left(&self, amt: usize) -> Self {
    self.rotate_left(amt as u32)
  }

  fn cry_rotate_right(&self, amt: usize) -> Self {
    self.rotate_right(amt as u32)
  }
}


#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test() {
    let x : u8 = 0b_1000_0110;
    assert_eq!(x.cry_length(), 8);
    assert_eq!(x.cry_index(6), true);
    assert_eq!(x.cry_index(7), false);
    assert_eq!(x.cry_shift_left((),1),  0b_0000_1100);
    assert_eq!(x.cry_shift_left((),8),  0b_0000_0000);
    assert_eq!(x.cry_shift_right((),2), 0b_0010_0001);
    assert_eq!(x.cry_shift_right_signed(2), 0b_1110_0001);
    assert_eq!(x.cry_shift_right_signed(8), 0b_1111_1111);
    assert_eq!(x.cry_rotate_left(2), 0b_0001_1010);
    assert_eq!(x.cry_rotate_left(80), x);
    assert_eq!(x.cry_rotate_right(2), 0b_1010_0001);
    assert_eq!(x.cry_rotate_right(80), x);
  }
}


