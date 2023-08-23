use std::cmp::Ordering;
use crate::{DWord,DWordRef};


impl DWordRef<'_> {

  /// Returns `true` if the number is 0.
  pub fn is_zero(self) -> bool {
    for &w in self.as_slice() {
      if w != 0 { return false }
    }
    return true
  }
}



impl<'a> DWordRef<'a> {

  pub fn equal(self, other: DWordRef<'_>) -> bool {
    if self.bits() != other.bits() { return false }
    for (lhs,rhs) in self.iter_limbs_lsb().zip(other.iter_limbs_lsb()) {
      if lhs != rhs { return false }
    }
    true
  }

  pub fn compare(self, other: DWordRef<'_>) -> Option<Ordering> {
    if self.bits() != other.bits() { return None }
    for (lhs,rhs) in self.iter_limbs_msb().zip(other.iter_limbs_msb()) {
      if lhs == rhs { continue }
      return Some(if lhs < rhs { Ordering::Less } else { Ordering::Greater })
    }
    Some(Ordering::Equal)
  }

}

impl PartialEq for DWordRef<'_> {
  fn eq(&self, other: &DWordRef<'_>) -> bool { self.equal(*other) }
}

impl PartialOrd for DWordRef<'_> {
  fn partial_cmp(&self, other: &DWordRef<'_>) -> Option<Ordering> {
    self.compare(*other)
  }
}

impl PartialEq for DWord {
  fn eq(&self, other: &Self) -> bool { self.as_ref() == other.as_ref() }
}

impl PartialOrd for DWord {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    self.as_ref().partial_cmp(&other.as_ref())
  }
}

impl Eq for DWordRef<'_> {}
impl Eq  for DWord {}

impl Ord for DWordRef<'_> {
  fn cmp(&self, other: &DWordRef<'_>) -> Ordering {
    self.partial_cmp(other).unwrap()
  }
}

impl Ord for DWord {
  fn cmp(&self, other: &Self) -> Ordering {
    self.as_ref().cmp(&other.as_ref())
  }
}



#[cfg(test)]
mod test {
  use crate::{DWord,DWordRef};

  #[test]
  fn test_eq() {
    let x_7_100 = DWord::from_u64(7, 100);
    let x_7_101 = DWord::from_u64(7, 101);

    let x_64_100 = DWord::from_u64(64, 100);
    let x_64_101 = DWord::from_u64(64, 101);

    let x_67_100 = DWord::from_u64(67, 100);
    let x_67_101 = DWord::from_u64(67, 101);

  }
}
