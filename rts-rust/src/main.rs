use cryptol::*;
use num::bigint as n;


// This is is just for testsing stuff out
fn main() {
  let n = <n::BigUint as num::Num>::from_str_radix("4",16).unwrap();
  let x = <BitVec!(7)>::from_integer(&n);
  let y = <BitVec!(7)>::from_u64(3);

  print!("{:?}\n", (&x / &y).to_uint());

}
