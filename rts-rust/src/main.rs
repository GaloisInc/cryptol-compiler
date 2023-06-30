use cryptol::*;
//use num::bigint as n;
//use crypto_bigint as c;
//

// This is is just for testsing stuff out
fn main() {
  let x = <BitVec!(17)>::from(0x_00_A1_02_03_04_05_06_07_u64);
  print!("{:#x}\n", x);
  print!("{:#X}\n", x);
}
