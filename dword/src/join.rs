use crate::{DWord,DWordRef};

impl DWordRef<'_> {

  pub fn append(self, lower_bv: DWordRef<'_>) -> DWord {
    if self.bits() == 0 { return lower_bv.clone_word() }
    if lower_bv.bits() == 0 { return self.clone_word() }

    let upper     = self.as_slice();
    let upper_len = upper.len();
    let lower     = lower_bv.as_slice();
    let lower_len = lower.len();
    let pad_upper = self.padding();
    let pad_lower = lower_bv.padding();

    let mut result = DWord::zero(self.bits() + lower_bv.bits());
    let out_len    = result.limbs();
    let mid        = out_len - upper_len;

    let out        = result.as_slice_mut();

    // First we copy the upper word into the output.
    out[mid ..].copy_from_slice(upper);

    if pad_upper == 0 { out[0 .. lower_bv.limbs()].copy_from_slice(lower); }
    else {

      let have      = DWord::LIMB_BITS - pad_upper;
      let mut out_v = upper[0];
      for i in 0 .. lower_len {
        let w  = lower[lower_len - i - 1];
        out[mid - i] = out_v | (w >> have);
        out_v        = w << pad_upper;
      }
      if have > pad_lower { out[0] = out_v; }
    }

    result
  }
}


#[cfg(test)]
mod test {
  use crate::DWord;
  use crate::proptest::*;

  #[test]
  fn append() {
    do_test2(two_words, |(x,y): (DWord,DWord)| {
      let (xr,a) = x.sem();
      let (yr,b) = y.sem();
      //let expect = (a << b.bits()) + b;
      let expect = (a << y.bits()) + b;
      assert_eq!( xr.append(yr)
                , DWord::from_uint(x.bits() + y.bits(), &expect));
      Some (true)
    })
  }
}


