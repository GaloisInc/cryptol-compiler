use cryptol::trait_methods::*;



// This is just for testing stuff out.
fn main() {
  let x = cryptol::word!(true,true,true,false);
  print!("{:#b}\n",x.display());
}
