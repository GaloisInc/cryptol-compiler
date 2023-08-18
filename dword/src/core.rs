use std::marker::PhantomData;

pub type LimbT = u64;

impl DWord {

  /// The number of bits in a libm.
  pub const LIMB_BITS: usize = LimbT::BITS as usize;
}



/// The data for a dynamic word.
#[derive(Copy,Clone)]
union DWordData {
  small: LimbT,
  large: *mut LimbT
}

/// The data for a dynamic word, together with the word size in bits.
#[derive(Copy,Clone)]
struct DWordPtr {
  bits: usize,
  data: DWordData
}

/// A word.
pub struct DWord {
  data: DWordPtr
}

/// A read-only reference to a word.
/// This is similar to &DWord, but is passed by value, thus avoiding
/// an additional indirection.
#[derive(Copy,Clone)]
pub struct DWordRef<'a> {
  scope:  PhantomData<&'a [LimbT]>,
  data:   DWordPtr
}


// -----------------------------------------------------------------------------
// Limbs & is_small

/// How many limbs we need to represent the given number of bits.
fn limbs_for_size(bits: usize) -> usize {
  (bits + DWord::LIMB_BITS - 1) / DWord::LIMB_BITS
}

/// Is this number of bits sufficiently small to fit in the small
/// representation.
fn is_small_size(bits: usize) -> bool { bits <= DWord::LIMB_BITS }

impl DWordPtr {

  /// How many limbs we need to represent the given number of bits.
  /// When creating from a vector, this is how many entries should
  /// be in the vector.
  #[inline(always)]
  fn limbs(&self) -> usize { limbs_for_size(self.bits) }

  /// Are we using the single limb representation.
  #[inline(always)]
  fn is_small(&self) -> bool { is_small_size(self.bits) }
}







// -----------------------------------------------------------------------------
// Access to underlying representation.


impl DWordPtr {

  /// Read access to the underlying representation of the word.
  /// The resulting slice is always at least 1 limb.
  #[inline(always)]
  pub fn as_slice(&self) -> &[LimbT] {
    unsafe {
      if self.is_small() {
        std::slice::from_ref(&self.data.small)
      } else {
        std::slice::from_raw_parts(self.data.large, self.limbs())
      }
    }
  }

  /// Mutable access to the underlying representation of the word.
  /// The resulting slice is always at least 1 limb.
  #[inline(always)]
  pub fn as_slice_mut(&mut self) -> &mut [LimbT] {
    unsafe {
      if self.is_small() {
        std::slice::from_mut(&mut self.data.small)
      } else {
        std::slice::from_raw_parts_mut(self.data.large, self.limbs())
      }
    }
  }
}


impl DWord {

  /// Get a read reference to the word.
  #[inline(always)]
  pub fn as_ref(&self) -> DWordRef {
    DWordRef { scope: PhantomData, data: self.data }
  }

  /// Read access to the underlying representation of the word.
  /// The resulting slice is always at least 1 limb.
  #[inline(always)]
  pub fn as_slice(&self) -> &[LimbT] { self.data.as_slice() }

  /// Mutable access to the underlying representation of the word.
  /// The resulting slice is always at least 1 limb.
  #[inline(always)]
  pub fn as_slice_mut(&mut self) -> &mut [LimbT] { self.data.as_slice_mut() }
}


impl<'a> DWordRef<'a> {

  /// Read access to the underlying representation of the word.
  /// The resulting slice is always at least 1 limb.
  #[inline(always)]
  pub fn as_slice(&'a self) -> &'a [LimbT] { self.data.as_slice() }

}


// -----------------------------------------------------------------------------
// Allocation, Cloning, Deallocation

impl DWordPtr {

  #[inline(always)]
  fn from_vec(bits: usize, data: Vec<LimbT>) -> DWordPtr {
    if is_small_size(bits) {
      DWordPtr {
        bits: bits,
        data:
          DWordData { small: if bits > 0 { data[0] } else { 0 } }
      }
    } else {
      DWordPtr {
        bits: bits,
        data:
          DWordData { large: Box::<_>::into_raw(data.into_boxed_slice()).cast()}
      }
    }
  }

  #[inline(always)]
  pub fn copy(self) -> DWordPtr {
    Self::from_vec(self.bits, Vec::from(self.as_slice()))
  }

  #[inline(always)]
  fn free(self) {
    if self.is_small() { return }
    unsafe {
      let s = std::ptr::slice_from_raw_parts_mut(self.data.large, self.limbs());
      drop(Box::<[LimbT]>::from_raw(s))
    }
  }

}

impl<'a> DWordRef<'a> {
  pub fn clone_word(self) -> DWord { DWord { data: self.data.copy() } }
}


impl DWord {

  /// Create a DWord from the given limbs.
  /// The vector should contain the correct number of limbs for the bits.
  pub fn from_vec(bits: usize, data: Vec<LimbT>) -> DWord {
    assert_eq!(data.len(), limbs_for_size(bits));
    DWord { data: DWordPtr::from_vec(bits,data) }
  }

  /// Create a 0 initialized word of the given size.
  pub fn zero(bits: usize) -> DWord {
    Self::from_vec(bits, vec![0; limbs_for_size(bits)])
  }

}


impl Clone for DWord {
  fn clone(&self) -> Self { self.as_ref().clone_word() }
}

impl Drop for DWord {
  fn drop(&mut self) { self.data.free() }
}




// -----------------------------------------------------------------------------

impl DWordPtr {

  /// The size of the word in bits.
  #[inline(always)]
  fn bits(&self) -> usize { self.bits }

  /// The number of 0s in the least significant position.
  #[inline(always)]
  fn padding(&self) -> usize {
    if self.bits() == 0 { return DWord::LIMB_BITS }
    self.limbs() * DWord::LIMB_BITS - self.bits()
  }

  /// The number of bits that are used in the least significant limb.
  #[inline(always)]
  fn not_padding(&self) -> usize {
    DWord::LIMB_BITS - self.padding()
  }
}

impl<'a> DWordRef<'a> {

  /// The size of the word in bits.
  #[inline(always)]
  pub fn bits(self)        -> usize { self.data.bits() }

  #[inline(always)]
  /// The number of limbs in the representation of the word.
  pub fn limbs(&self)       -> usize { self.data.limbs() }

  /// The number of 0s in the least significant of least signficant limb.
  #[inline(always)]
  pub fn padding(self)     -> usize { self.data.padding() }

  /// The number of bits that are used in the least significant limb.
  #[inline(always)]
  pub fn not_padding(self) -> usize { self.data.not_padding() }
}

impl DWord {

  #[inline(always)]
  pub fn bits(&self)        -> usize { self.data.bits() }

  #[inline(always)]
  pub fn limbs(&self)       -> usize { self.data.limbs() }

  #[inline(always)]
  pub fn padding(&self)     -> usize { self.data.padding() }

  #[inline(always)]
  pub fn not_padding(&self) -> usize { self.data.not_padding() }

  /// Clear up any bits in the least significant position of the full Uint.
  #[inline(always)]
  pub fn fix_underflow(&mut self) {
    let pad = self.padding();
    if pad == 0 { return; }
    let buf = self.as_slice_mut();
    buf[0] &= !((1 << pad) - 1);
  }
}



#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_padding() {
    assert_eq!(DWord::zero(0).padding(), DWord::LIMB_BITS);
    assert_eq!(DWord::zero(1).padding(), DWord::LIMB_BITS - 1);
    assert_eq!(DWord::zero(DWord::LIMB_BITS).padding(), 0);
  }

}
