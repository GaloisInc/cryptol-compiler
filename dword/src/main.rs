use dword::*;

pub fn main() {
  let x = DWord::from_u64(65,0xFFFFF);
  print!("{:#b}\n",x);
  print!("{:#o}\n",x);
  print!("{:#x}\n",x);
  print!("{:#X}\n",x);
}
