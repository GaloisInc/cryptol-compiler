use dword::*;

pub fn main() {
  let i : i64 = -1;
  let x = DWord::from_u64(64,i as u64);
  print!("{:#b}\n",x);
  print!("{:#b}\n",x.as_ref() >> 0);
}
