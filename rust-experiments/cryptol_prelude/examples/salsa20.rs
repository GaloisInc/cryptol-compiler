// salsa20 implementation in rust based on spec from `cryptol-specs`
// see `spec/salsa20.cry`

use cryptol_prelude::cryptol::{Ring};

// quarterround : [4][32] -> [4][32]
// quarterround [y0, y1, y2, y3] = [z0, z1, z2, z3]
//   where
//     z1 = y1 ^ ((y0 + y3) <<< 0x7)
//     z2 = y2 ^ ((z1 + y0) <<< 0x9)
//     z3 = y3 ^ ((z2 + z1) <<< 0xd)
//     z0 = y0 ^ ((z3 + z2) <<< 0x12)
fn quarterround(y: [u32; 4]) -> [u32; 4] {
  let z1 = y[1] ^ y[0].radd(&y[3]).rotate_left(0x7);
  let z2 = y[2] ^ z1.radd(&y[0]).rotate_left(0x9);
  let z3 = y[3] ^ z2.radd(&z1).rotate_left(0xd);
  let z0 = y[0] ^ z3.radd(&z2).rotate_left(0x12);
  [z0,z1,z2,z3]
}



fn main() {

}