use cryptol::traits::*;
use cryptol::bitvec_fixed::*;
use cryptol::*;
use crypto_bigint as c;
use num::bigint as n;
use std::str::FromStr;


// This is is just for testsing stuff out
fn main() {
  let n = <n::BigUint as num::Num>::from_str_radix("11",16).unwrap();
  let x = <BitVec!(8)>::from_integer(&n);
  let y = <BitVec!(8)>::from_u64(3);

  print!("{:?}\n", (&x % &y).to_uint());

}
