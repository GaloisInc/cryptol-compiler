use cryptol::*;
use cryptol::traits::Literal;
use num::bigint as n;
//use crypto_bigint as c;
//

// This is is just for testsing stuff out
fn main() {
  let i = 5_u32 >> 2_i8;
  print!("{}", i);

  //let x = <BitVec!(64)>::from(0x_1000000000_u64);
  print!("{} {:#x}\n", x(),x());
}


pub fn x() -> BitVec![ 8 ] {
  <BitVec![ 8 ]>::number((), 1)
}

