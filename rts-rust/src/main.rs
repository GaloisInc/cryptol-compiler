use cryptol::*;
use num::bigint as n;
//use crypto_bigint as c;
//

// This is is just for testsing stuff out
fn main() {
  let x = <BitVec!(64)>::from(0x_1000000000_u64);
  print!("{} {:#x}\n", x,x);
}
