use crate::{DWord,DWordRef};
use crate::core::{LimbT};

macro_rules! do_logic_assign {
  ($trait:ident, $method:ident) => {
    impl std::ops::$trait<DWordRef<'_>> for DWord {
      fn $method(&mut self, rhs: DWordRef<'_>) {
        let bits = self.bits();
        assert_eq!(bits, rhs.bits());
        for (x,y) in self.as_slice_mut().iter_mut().zip(rhs.as_slice()) {
          x.$method(y)
        }
      }
    }
  };
}

macro_rules! do_logic_eval {
  ($trait:ident, $method:ident) => {
    impl std::ops::$trait for DWordRef<'_> {
      type Output = DWord;
      fn $method(self, rhs: Self) -> Self::Output {
        let bits = self.bits();
        assert_eq!(bits, rhs.bits());
        let mut xs = Vec::<LimbT>::with_capacity(self.limbs());

        for (x,y) in self.as_slice().iter().zip(rhs.as_slice().iter()) {
          xs.push(x.$method(y))
        }

        DWord::from_limbs(bits, xs)
      }
    }
  };
}

do_logic_assign!(BitXorAssign, bitxor_assign);
do_logic_eval!(BitXor, bitxor);
do_logic_assign!(BitAndAssign, bitand_assign);
do_logic_eval!(BitAnd, bitand);
do_logic_assign!(BitOrAssign, bitor_assign);
do_logic_eval!(BitOr, bitor);

impl DWord {
  pub fn not_assign(&mut self) {
    for x in self.as_slice_mut().iter_mut() {
      *x = !*x
    }
    self.fix_underflow()
  }
}

impl std::ops::Not for DWordRef<'_> {
  type Output = DWord;
  fn not(self) -> Self::Output {
    let mut xs = Vec::<LimbT>::with_capacity(self.limbs());
    for x in self.as_slice().iter() {
      xs.push(!x)
    }
    let mut result = DWord::from_limbs(self.bits(),xs);
    result.fix_underflow();
    result
  }
}



