use std::marker::PhantomData;

pub type LimbT = u64;
pub struct Limb();

impl Limb {
  /// The number of bits in a libm.
  pub const BITS: usize = LimbT::BITS as usize;
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
#[derive(Copy,Clone)]
pub struct DWordRef<'a> {
  scope:  PhantomData<&'a [LimbT]>,
  data:   DWordPtr
}


// -----------------------------------------------------------------------------
// limbs & is_small

/// How many limbs we need to represent the given number of bits.
fn limbs_for_size(bits: usize) -> usize {
  (bits + Limb::BITS - 1) / Limb::BITS
}

/// Is this number of bits sufficiently small to fit in the small
/// representation.
fn is_small_size(bits: usize) -> bool { bits <= Limb::BITS }

impl DWordPtr {

  #[inline(always)]
  fn limbs(&self) -> usize { limbs_for_size(self.bits) }

  #[inline(always)]
  fn is_small(&self) -> bool { is_small_size(self.bits) }
}







// -----------------------------------------------------------------------------
// References and Slices


impl DWordPtr {

  /// Gain access to the underlying representation of the word.
  ///   * The less significant parts of the word are stored in the elements
  ///     with lower indexes (little endian).
  ///   * For word sizes that are not a multiple of Limb::BITS, the
  ///     actual word data is stored in the most significatn parts of the word.
  ///     In that case, least significant bits of the first limb
  ///     (index 0, least significant) are going to be all 0.
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

  /// Gain access to the underlying representation of the word.
  ///   * The less significant parts of the word are stored in the elements
  ///     with lower indexes (little endian).
  ///   * For word sizes that are not a multiple of Limb::BITS, the
  ///     actual word data is stored in the most significatn parts of the word.
  ///     In that case, least significant bits of the first limb
  ///     (index 0, least significant) are going to be all 0.
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

  #[inline(always)]
  pub fn as_ref<'a>(&'a self) -> DWordRef<'a> {
    DWordRef { scope: PhantomData, data: self.data }
  }

  #[inline(always)]
  pub fn as_slice(&self) -> &[LimbT] { self.data.as_slice() }

  #[inline(always)]
  pub fn as_slice_mut(&mut self) -> &mut [LimbT] { self.data.as_slice_mut() }
}


impl<'a> DWordRef<'a> {

  #[inline(always)]
  pub fn as_slice(&'a self) -> &'a [LimbT] { self.data.as_slice() }

}


// -----------------------------------------------------------------------------
// Allocation, Cloning, Deallocation

impl DWord {

  /// Layout for the large representation
  fn layout(bits: usize) -> std::alloc::Layout {
    std::alloc::Layout::array::<LimbT>(limbs_for_size(bits)).unwrap()
  }

  /// Create a 0 initialized word of the given size.
  pub fn zero(bits: usize) -> DWord {
    DWord {
      data:
        if is_small_size(bits) {
          DWordPtr { bits: bits, data: DWordData { small: 0 } }
        } else {
          let ptr = unsafe { std::alloc::alloc_zeroed(Self::layout(bits)) };
          DWordPtr { bits: bits, data: DWordData { large: ptr.cast() } }
        }
    }
  }

}

impl Drop for DWord {
  fn drop(&mut self) {
    unsafe {
      if !self.data.is_small() {
        std::alloc::dealloc( self.data.data.large.cast()
                           , Self::layout(self.data.bits)
                           )
      }
    }
  }
}

impl DWordPtr {
  pub fn clone(self) -> DWordPtr {
    let mut result = self;
    if !self.is_small() {
      let data = self.as_slice();
      result.data.large =
        unsafe { std::alloc::alloc(DWord::layout(self.bits)).cast() };
      result.as_slice_mut().copy_from_slice(data);
    }
    result
  }
}


impl<'a> DWordRef<'a> {
  pub fn clone(self) -> DWord { DWord { data: self.data.clone() } }
}

impl Clone for DWord {
  fn clone(&self) -> Self { self.as_ref().clone() }
}


// -----------------------------------------------------------------------------

impl DWordPtr {

  /// The size of the word in bits.
  #[inline(always)]
  fn bits(&self) -> usize { self.bits }

  /// The number of 0s in the least significant position.
  #[inline(always)]
  fn padding(&self) -> usize {
    if self.bits() == 0 { return Limb::BITS }
    self.limbs() * Limb::BITS - self.bits()
  }

  /// The number of bits that are used in the least significant limb.
  #[inline(always)]
  fn not_padding(&self) -> usize {
    Limb::BITS - self.padding()
  }

  /// Clear up any bits in the least significant position of the full Uint.
  #[inline(always)]
  fn fix_underflow(&mut self) {
    let pad = self.padding();
    if pad == 0 { return; }
    let buf = self.as_slice_mut();
    buf[0] &= !((1 << pad) - 1);
  }
}

impl<'a> DWordRef<'a> {
  pub fn bits(self)        -> usize { self.data.bits() }
  pub fn padding(self)     -> usize { self.data.padding() }
  pub fn not_padding(self) -> usize { self.data.not_padding() }
}

impl DWord {
  pub fn bits(&self)        -> usize { self.data.bits() }
  pub fn padding(&self)     -> usize { self.data.padding() }
  pub fn not_padding(&self) -> usize { self.data.not_padding() }
  pub fn fix_underflow(&mut self) { self.data.fix_underflow() }
}



#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_padding() {
    assert_eq!(DWord::zero(0).padding(), Limb::BITS);
    assert_eq!(DWord::zero(1).padding(), Limb::BITS - 1);
    assert_eq!(DWord::zero(Limb::BITS).padding(), 0);
  }

}

