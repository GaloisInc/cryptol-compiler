use dword::*;
use rand::random;

pub fn main() {
  let a = DWord::zero(0);

  let x = DWord::from_u64(123, -1_i64 as u64);
  let y = DWord::from_int(123, &num::BigInt::from(-1_i64));
  let z = x.as_ref()/y.as_ref();
  println!("{:#b}, {:#b}, {}",x,y, x.as_ref()/y.as_ref());
}

fn test_mul () {

  for size in 0 .. 512 {

    let mut buf : Vec<DWord> = vec![];
    for _seed in 0 .. 20 {
      buf.push(DWord::from_u64(size, random()))
    }

    for _case in 0 .. 512 {
      let i: usize = random::<usize>() % buf.len();
      let j: usize = random::<usize>() % buf.len();
      let lhs = DWord::as_ref(&buf[i]);
      let rhs = DWord::as_ref(&buf[j]);
      let product = lhs * rhs;
      println!("({},{:#x},{:#x},{:#x})", size, lhs, rhs, product);
      buf[i] =
        if product == DWord::zero(size) {
          DWord::from_u64(size, random())
        } else { product }
    }
  }
}
