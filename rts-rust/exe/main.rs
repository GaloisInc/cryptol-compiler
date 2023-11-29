use cry_rts::trait_methods::*;



// This is just for testing stuff out.
fn main() {
  let x: cry_rts::DWord = 12345678_u32.into();
  let y: cry_rts::DWord = 123_u8.into();

  let r = x.as_ref().pmod(y.as_ref());
  println!("{}", r)

}
