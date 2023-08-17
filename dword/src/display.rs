use crate::DWord;
use std::fmt;

impl fmt::Binary for DWord {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut s = String::new();
    if self.bits() == 0 {
      s.push('0'); // special case so that we see something.
    } else {
      for b in self.iter_msb() {
        s.push(if b { '1' } else { '0' })
      }
    }
    f.pad_integral(true, "0b", &s)
  }
}

impl fmt::Octal for DWord {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut s = String::new();
    let extra = self.bits() % 3;
    let table = ['0','1','2','3','4','5','6','7'];
    let mut emit = |x| s.push(table[ x as usize ]);

    match extra {
      1 => emit(u8::from(&self.slice_be(1,0))),
      2 => emit(u8::from(&self.slice_be(2,0))),
      _ => ()
    }

    for i in 0 .. self.bits() / 3 {
      emit(u8::from(&self.slice_be(3, extra + 3 * i)))
    }

    f.pad_integral(true, "0o", &s)
  }
}


impl DWord {
  fn fmt_hex(&self, f: &mut fmt::Formatter, table: [char; 16]) -> fmt::Result {
    let mut s = String::new();
    let extra = self.bits() % 4;
    let mut emit = |x| s.push(table[ x as usize ]);

    match extra {
      1 => emit(u8::from(&self.slice_be(1,0))),
      2 => emit(u8::from(&self.slice_be(2,0))),
      3 => emit(u8::from(&self.slice_be(3,0))),
      _ => ()
    }

    for i in 0 .. self.bits() / 4 {
      emit(u8::from(&self.slice_be(4, extra + 4 * i)))
    }

    f.pad_integral(true, "0x", &s)

  }
}


impl fmt::UpperHex for DWord {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_hex(f, ['0','1','2','3','4','5','6','7','8','9'
                    ,'A','B','C','D','E','F'])
  }
}

impl fmt::LowerHex for DWord {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.fmt_hex(f, ['0','1','2','3','4','5','6','7','8','9'
                    ,'a','b','c','d','e','f'])
  }
}




