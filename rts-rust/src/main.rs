use cryptol::trait_methods::*;

// This is just for testing stuff out.
fn main() {
  let x: [ cryptol::Word!(8); 1 ] = [ 1_u64.into() ];
  let y = &x;
  print!("{:#b}\n",y.displayable());
  print!("{:#b}\n",x.displayable());
  print!("{:#b}\n",x.displayable());
}
