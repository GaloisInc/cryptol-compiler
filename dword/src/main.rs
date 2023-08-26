use dword::*;

pub fn main() {
  let x = DWord::from_u64(1,1);
  let y = DWord::from_u64(65,1);
  let z = x.as_ref().append(y.as_ref());
  println!("{:#b} # {:#b} = {:#b}",x,y,z);
}
