use crate::DWord;
use crate::FromMSB;
use std::fmt;

impl fmt::Binary for DWord {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let selfie = self.as_ref();

    let mut s = String::new();
    if selfie.bits() == 0 {
      s.push('0'); // special case so that we see something.
    } else {
      for b in selfie.iter::<FromMSB>() {
        s.push(if b { '1' } else { '0' })
      }
    }
    f.pad_integral(true, "0b", &s)
  }
}

impl fmt::Octal for DWord {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let selfie = self.as_ref();

    let mut s = String::new();
    let extra = selfie.bits() % 3;
    let table = ['0','1','2','3','4','5','6','7'];
    let mut emit = |x| s.push(table[ x as usize ]);

    match extra {
      1 => emit(u8::from(selfie.sub_word::<FromMSB>(1,0).as_ref())),
      2 => emit(u8::from(selfie.sub_word::<FromMSB>(2,0).as_ref())),
      _ => ()
    }

    for i in 0 .. selfie.bits() / 3 {
      emit(u8::from(selfie.sub_word::<FromMSB>(3, extra + 3 * i).as_ref()))
    }

    f.pad_integral(true, "0o", &s)
  }
}


impl DWord {
  fn fmt_hex(&self, f: &mut fmt::Formatter, table: [char; 16]) -> fmt::Result {
    let selfie = self.as_ref();

    let mut s = String::new();
    let extra = selfie.bits() % 4;
    let mut emit = |x| s.push(table[ x as usize ]);

    match extra {
      1 => emit(u8::from(selfie.sub_word::<FromMSB>(1,0).as_ref())),
      2 => emit(u8::from(selfie.sub_word::<FromMSB>(2,0).as_ref())),
      3 => emit(u8::from(selfie.sub_word::<FromMSB>(3,0).as_ref())),
      _ => ()
    }

    for i in 0 .. selfie.bits() / 4 {
      emit(u8::from(selfie.sub_word::<FromMSB>(4, extra + 4 * i).as_ref()))
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




