use cryptol::trait_methods::*;

// This is just for testing stuff out.
fn main() {
  let x: [ cryptol::Word!(8); 1 ] = [ 1_u64.into() ];
  let y = &x;
  print!("{:#b}\n",y.display());
  print!("{:#b}\n",x.display());
  print!("{:#b}\n",x.display());
}
