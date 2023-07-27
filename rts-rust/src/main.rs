use cryptol::trait_methods::*;

// This is just for testing stuff out.
fn main() {
  let x = <cryptol::Word!(8)>::from(2_u64);
  print!("{} ^ 7 = {}\n",x, x.exp(7));
}
