

// This is just for testing stuff out.
fn main() {
  //let x = dword::DWord::from_u64(8,1);
  //let y = dword::DWord::from_u64(8,10);
//  let x = num::BigUint::from(1_u64);
//  let y = num::BigUint::from(1_u64);
//  for i in x .. y {
//    println!("{}",i)
//  }

  for i in cry_rts::from_to_usize::<cry_rts::DWord>(8,1,5).map(|x| x + cry_rts::number::<cry_rts::DWord>(8,1)) {
    println!("{}",i)
  }
}
