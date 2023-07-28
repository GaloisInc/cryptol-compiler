use cryptol::trait_methods::*;

// This is just for testing stuff out.
fn main() {
  let x: cryptol::Array<1,cryptol::Word!(6)> = [1_u64.into()].into();
  print!("{:#b}\n",x);
}
