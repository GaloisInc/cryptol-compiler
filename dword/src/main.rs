use dword::*;

pub fn main() {
  let x = DWord::from_u64(3,1);
  print!("{:#b}\n",x);
  print!("{:#x}\n",x.as_ref().get_limb::<FromMSB>(0));
}
