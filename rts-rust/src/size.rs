
pub fn size_to_int(x: usize) -> num::BigUint { x.into() }

pub fn int_to_size(x: &num::BigUint) -> usize {
  let ds = x.to_u64_digits();
  assert!(ds.len() < 2);
  if ds.len() == 0 { return 0 }
  ds[0] as usize
}

pub fn add_size(x: usize, y: usize) -> usize { x + y }
pub fn sub_size(x: usize, y: usize) -> usize { x - y }
pub fn mul_size(x: usize, y: usize) -> usize { x * y }
pub fn div_size(x: usize, y: usize) -> usize { x / y }
pub fn mod_size(x: usize, y: usize) -> usize { x % y }
pub fn exp_size(x: usize, y: usize) -> usize { x.pow(y as u32) }
pub fn min_size(x: usize, y: usize) -> usize { x.min(y) }
pub fn max_size(x: usize, y: usize) -> usize { x.max(y) }

// XXX: width_size
// XXX: ceil_div_size
// XXX: ceil_mod_size
// XXX: from_then_to_size
//


pub fn add_size_uint(x: &num::BigUint, y: &num::BigUint) -> num::BigUint
  { x + y }

pub fn sub_size_uint(x: &num::BigUint, y: &num::BigUint) -> num::BigUint
  { x - y }

pub fn mul_size_uint(x: &num::BigUint, y: &num::BigUint) -> num::BigUint
  { x * y }

pub fn div_size_uint(x: &num::BigUint, y: &num::BigUint) -> num::BigUint
  { x / y }

pub fn mod_size_uint(x: &num::BigUint, y: &num::BigUint) -> num::BigUint
  { x % y }

pub fn exp_size_uint(x: &num::BigUint, y: usize) -> num::BigUint
  { x.pow(y as u32) }

pub fn min_size_uint(x: &num::BigUint, y: &num::BigUint) -> num::BigUint
  { x.min(y).clone() }

pub fn max_size_uint(x: &num::BigUint, y: &num::BigUint) -> num::BigUint
  { x.max(y).clone() }


