use dword::*;
use rand::random;

pub fn main() {

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
