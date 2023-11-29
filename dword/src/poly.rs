use crate::{DWordRef,DWord};

// [u] -> [1+v] -> [v]
impl DWordRef<'_> {
  pub fn pmod(self, m: DWordRef<'_>) -> DWord {

    let u  = self.bits();            // width of 1st argument
    let v1 = m.bits();
    assert!(v1 > 0);
    let v = v1 - 1;

    let n = m.leading_zeros();
    assert!(n < v1);                 // otherise, division by 0
    let degree = v1 - n - 1;         // lsb index of most signficant bit.

    let mut result =
      if u > degree { self.sub_word_lsb(degree, 0) } else { self.clone_word() };

    if result.bits() < v {
      result = DWord::zero(v - result.bits()).as_ref().append(result.as_ref())
    }


    let m1     = m.sub_word_lsb(v, 0);
    let mut p  = m1.as_ref().clone_word();
    p.set_bit_lsb(degree,false);

    for i in degree .. u {
      //println!("i = {}, result = {}, p = {}", i, result, p);
      if self.index_lsb(i) { result ^= p.as_ref() }
      p <<= 1;
      if p.as_ref().index_lsb(degree) {
        //print!("reducing {} ~>",p);
        p ^= m1.as_ref();
        println!("{}",p);
      }
    }
    result
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_pmod() {
    let mut x: DWord = 12345678_u32.into();
    let mut y: DWord = 123_u8.into();
    let mut z: DWord = DWord::from_u64(7, 0x14);
    assert_eq!(x.as_ref().pmod(y.as_ref()), z);

    let mut n = num::BigUint::parse_bytes(b"1092312983128903910831",10).unwrap();
    x = DWord::from_uint(128, &n);
    n = num::BigUint::parse_bytes(b"203947283734879",10).unwrap();
    y = DWord::from_uint(76, &n);
    n = num::BigUint::parse_bytes(b"0000000076c35d397a8",16).unwrap();
    z = DWord::from_uint(75, &n);
    assert_eq!(x.as_ref().pmod(y.as_ref()), z);
  }


}
