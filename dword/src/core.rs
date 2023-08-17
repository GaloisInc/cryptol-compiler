// Words of dynamic sizes

pub type LimbT = u64;
pub struct Limb();

impl Limb {
  /// The number of bits in a libm.
  pub const BITS: usize = LimbT::BITS as usize;
}

union DWordData {
  small: LimbT,
  large: *mut LimbT
}

/// An arbitrary sized word.
pub struct DWord {
  value: DWordData,
  bits:  usize
}

impl Drop for DWord {
  fn drop(&mut self) {
    if self.is_small() { return }
    unsafe {
      std::alloc::dealloc(self.value.large.cast(), DWord::layout(self.bits))
    }
  }
}

impl Clone for DWord {
  fn clone(&self) -> Self {
    let data = self.as_slice();
    if self.is_small() {
      return DWord { bits: self.bits(), value: DWordData { small: data[0] } }
    }

    let ptr    = unsafe { std::alloc::alloc(Self::layout(self.bits)) };
    let mut result =
      DWord { bits: self.bits(), value: DWordData { large: ptr.cast() } };
    result.as_slice_mut().copy_from_slice(data);
    result
  }
}

fn limbs_for_size(bits: usize) -> usize {
  (bits + Limb::BITS - 1) / Limb::BITS
}

fn is_small_size(bits: usize) -> bool { bits <= Limb::BITS }


impl DWord {

  /// Is this value represented with a single limb.
  fn is_small(&self) -> bool { is_small_size(self.bits) }

  /// How many limbs in the large representation.
  pub fn limbs(&self) -> usize { limbs_for_size(self.bits) }

  /// The size of the word in bits.
  pub fn bits(&self) -> usize { self.bits }

  /// Layout for the large representation
  pub fn layout(bits: usize) -> std::alloc::Layout {
    std::alloc::Layout::array::<LimbT>(limbs_for_size(bits)).unwrap()
  }

  /// Create a 0 initialized word of the given size.
  pub fn zero(bits: usize) -> DWord {
    if is_small_size(bits) {
      DWord { bits: bits, value: DWordData { small: 0 } }
    } else {
      let ptr = unsafe { std::alloc::alloc_zeroed(Self::layout(bits)) };
      DWord { bits: bits, value: DWordData { large: ptr.cast() } }
    }
  }

  /// Gain access to the underlying representation of the word.
  ///   * The less significant parts of the word are stored in the elements
  ///     with lower indexes (little endian).
  ///   * For word sizes that are not a multiple of Limb::BITS, the
  ///     actual word data is stored in the most significatn parts of the word.
  ///     In that case, least significant bits of the first limb
  ///     (index 0, least significant) are going to be all 0.
  pub fn as_slice(&self) -> &[LimbT] {
    unsafe {
      if self.is_small() {
        std::slice::from_ref(&self.value.small)
      } else {
        std::slice::from_raw_parts(self.value.large, self.limbs())
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
  pub fn as_slice_mut(&mut self) -> &mut [LimbT] {
    unsafe {
      if self.is_small() {
        std::slice::from_mut(&mut self.value.small)
      } else {
        std::slice::from_raw_parts_mut(self.value.large, self.limbs())
      }
    }
  }

  /// The number of 0s in the least significant position.
  pub fn padding(&self) -> usize {
    if self.bits() == 0 { return Limb::BITS }
    self.limbs() * Limb::BITS - self.bits()
  }

  /// The number of bits that are used in the least significant limb.
  pub fn last_used_bits(&self) -> usize {
    Limb::BITS - self.padding()
  }

  /// Clear up any bits in the least significant position of the full Uint.
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
    assert_eq!(DWord::zero(0).padding(), Limb::BITS);
    assert_eq!(DWord::zero(1).padding(), Limb::BITS - 1);
    assert_eq!(DWord::zero(Limb::BITS).padding(), 0);
  }

}


