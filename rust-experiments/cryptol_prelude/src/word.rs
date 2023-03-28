

trait Ring {
  fn radd(&self, rhs: Self) -> Self;
  fn rmul(&self, rhs: Self) -> Self;
  fn rsub(&self, rhs: Self) -> Self;
  fn rneg(&self) -> Self;
}

impl Ring for u8 {
    fn radd(&self, rhs: Self) -> Self { self.wrapping_add(rhs) }
    fn rmul(&self, rhs: Self) -> Self { self.wrapping_mul(rhs) }
    fn rsub(&self, rhs: Self) -> Self { self.wrapping_sub(rhs) }
    fn rneg(&self) -> Self { self.wrapping_neg() }
}

impl Ring for u64 {
  fn radd(&self, rhs: Self) -> Self { self.wrapping_add(rhs) }
  fn rmul(&self, rhs: Self) -> Self { self.wrapping_mul(rhs) }
  fn rsub(&self, rhs: Self) -> Self { self.wrapping_sub(rhs) }
  fn rneg(&self) -> Self { self.wrapping_neg() }
}


trait Bits: Copy {
  fn mod2n(&self, n: u32) -> Self;
}

impl Bits for u8 {
  fn mod2n(&self, n: u32) -> Self {
    self >> u8::BITS.saturating_sub(n)
  }
}

impl Bits for u64 {
  fn mod2n(&self, n: u32) -> Self {
    self >> u64::BITS.saturating_sub(n)
  }
}

// ------------------------------------------------------------------

fn add_mod<T>(i: T, j: T, n: u32) -> T
  where T: Bits + Ring
{
  i.radd(j).mod2n(n)
}

fn sub_mod<T>(i: T, j: T, n: u32) -> T
  where T: Bits + Ring
{
  i.radd(j).mod2n(n)
}