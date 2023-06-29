use cryptol::*;
use num::bigint as n;
use crypto_bigint as c;


// This is is just for testsing stuff out
fn main() {
  let x = <BitVec!(3)>::from(0b111_u64);
  print!("{:?}\n",x);
  let y = c::Uint::<1>::from(0b111_u64);
  for i in 0 .. 3 {
    print!("i = {}, bit = {:?}\n", i, x.index_front(i));
  }
  //for i in x.traverse_bits() {
  //  print!("{:?}\n",i);
  //}

}
