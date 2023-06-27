use cryptol::*;
use num::bigint as n;


// This is is just for testsing stuff out
fn main() {
  let a = <BitVec!(63)>::from_u64(1);
  let b = <BitVec!(1)>::from_u64(1);
  let c = append!(1,63,&b,&a);
  print!("{:?}\n",a.to_uint());
  print!("{:?}\n",b.to_uint());
  print!("{:?}\n",c.to_uint());

}
