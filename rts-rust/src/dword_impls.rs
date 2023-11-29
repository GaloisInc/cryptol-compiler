use dword;

use crate::{Type, CloneArg, Sequence};


impl CloneArg for dword::DWordRef<'_> {
    type Owned = dword::DWord;

    fn clone_arg(self) -> Self::Owned {
        self.clone_word()
    }
}

impl Type for dword::DWord {
    type Arg<'a> = dword::DWordRef<'a>
        where Self : 'a;

    type Length = ();

    fn as_arg(&self) -> Self::Arg<'_> {
        self.as_ref()
    }
}

impl Sequence for dword::DWordRef<'_> {
    type Item = bool;

    fn length(self) -> usize {
      self.bits()
    }

    fn shift_right(self, n: <Self::Item as Type>::Length, amt: usize) -> Self::Owned
        where Self::Item : crate::Zero {
      self >> amt
    }

    fn shift_right_signed(self, amt: usize) -> Self::Owned {
      todo!("not implemented: Sequence::shift_right_signed for DWordRef")
    }

    fn rotate_right(self, amt: usize) -> Self::Owned {
      todo!("not implemented: Sequence::rotate_right for DWordRef")
    }

    fn shift_left(self, n: <Self::Item as Type>::Length, amt: usize) -> Self::Owned {
      self << amt
    }

    fn rotate_left(self, amt: usize) -> Self::Owned {
      todo!("not implemented: Sequence::rotate_left for DWordRef")
    }

    fn index(self, i: usize) -> Self::Item {
      self.index::<dword::FromMSB>(i)
    }
}

