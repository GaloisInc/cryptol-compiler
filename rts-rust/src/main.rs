use cryptol::*;
use num::bigint as n;


// This is is just for testsing stuff out
fn main() {
  let x = "123".parse::<n::BigUint>().unwrap();
  print!("{:?}\n",x);

}
