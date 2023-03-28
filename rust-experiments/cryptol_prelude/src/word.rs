

trait Ring {
  fn radd(&self, rhs: Self) -> Self;
  fn rmul(&self, rhs: Self) -> Self;
  fn rsub(&self, rhs: Self) -> Self;
  fn rneg(&self) -> Self;
}

impl Ring for u8 {
    fn radd(&self, rhs: Self) -> Self { self.wrapping_add(rhs) }
    fn rmul(&self, rhs: Self) -> Self { self.wrapping_mul(rhs) }
    fn rsub(&self, other: Self) -> Self { self.wrapping_sub(rhs) }
    fn rneg(&self) -> Self { self.wrapping_neg() }
}

impl Ring for u64 {
  fn radd(&self, rhs: Self) -> Self { self.wrapping_add(rhs) }
  fn rmul(&self, rhs: Self) -> Self { self.wrapping_mul(rhs) }
  fn rsub(&self, other: Self) -> Self { self.wrapping_sub(rhs) }
  fn rneg(&self) -> Self { self.wrapping_neg() }
}


trait Bits: Copy + std::ops::Shr<u32> + std::ops::BitAnd {
  const BIT_COUNT: u32;
  const ONES: Self;
}

impl Bits for u8 {
  const BIT_COUNT: u32 = u8::BITS;
  const ONES: Self = u8::MAX;
}

impl Bits for u64 {
  const BIT_COUNT: u32 = u64::BITS;
  const ONES: Self = u8::MAX;
}

// ------------------------------------------------------------------

fn mod_mask<T: Bits>(n: u32) -> T {
  T::ONES & (T::BIT_COUNT - n)
}

fn add_mod<T>(i: T, j: T, n: u32) -> T
  where T: Bits + Ring
{
  i.radd(j) & mod_mask(n)
}

fn sub_mod<T>(i: T, j: T, n: u32) -> T
  where T: Bits + Ring
{
  i.radd(j) & mod_mask(n)
}