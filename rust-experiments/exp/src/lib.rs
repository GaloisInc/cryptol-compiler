
// -- Integers and Nats -------------------------------------------------------

use std::marker::PhantomData;

// TODO: arbitrary sized
#[derive(Clone,Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Integer { value: i128 }

impl From<usize> for Integer {
    fn from(i: usize) -> Self {
        Integer { value: i as i128 }
    }
}

impl Integer {
    const Z: Integer = Integer { value: 0 };
}

impl std::ops::Add for Integer {
    type Output = Integer;

    fn add(self, rhs: Self) -> Self::Output {
        Integer { value: self.value + rhs.value }
    }
}

#[derive(Clone,Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Nat { value: Integer }

impl Into<Nat> for Integer {
    fn into(self) -> Nat {
        if self.value < 0 {
            panic!("Tried to convert Integer to Nat - but Integer is less than zero");
        } else {
            Nat { value: self }
        }
    }
}

impl Into<Integer> for Nat {
    fn into(self) -> Integer {
        self.value
    }
}


trait Zero {
    const Z: Self;
}

// -- Literal -----------------------------------------------------------------

trait Lit: Clone {
    fn from_to<S: Seq<Self>>(first: Self, last: Self) -> S;
}


// -- Logic -------------------------------------------------------------------

trait Logic: std::ops::BitAnd + std::ops::BitOr + std::ops::BitXor + std::ops::Not + Sized + Clone {

}

// -- Ring --------------------------------------------------------------------

trait Ring: std::ops::Add + std::ops::Sub + std::ops::Mul + std::ops::Neg<Output=Self> + Sized + Clone {
    // TODO: use From/Into builtin traits?
    fn from_integer(i: Integer) -> Self;
}

// -- Integral ----------------------------------------------------------------

trait Integral: std::ops::Div + std::ops::Rem + Sized {
    // TODO: use From/Into builtin traits?
    fn to_integer(&self) -> Integer;
}

trait IntegralExp<E: Ring>: Integral {
    fn exp(&self, e: &E) -> Self;
}

trait IntegralSeq<S: Seq<Self>>: Integral {
    fn inf_from(&self) -> S;
    fn inf_from_then(&self, next: &Self) -> S;
}


// -- Field -------------------------------------------------------------------

trait Field: Ring {
    fn recip(&self) -> Self;
    fn fdiv(&self, other: &Self) -> Self;
}

// -- Round -------------------------------------------------------------------

trait Round {
    fn floor(&self) -> Integer;
    fn ceiling(&self) -> Integer;
    fn trunc(&self) -> Integer;
    fn round_away(&self) -> Integer;
    fn rount_to_even(&self) -> Integer;
}

// -- Eq ----------------------------------------------------------------------

// Eq is mapped to rust's builtin Eq

pub fn fn_eq<F,G,A,B>(f: F, g: G, a: &A) -> bool
    where
    F: Fn(&A) -> B,
    G: Fn(&A) -> B,
    B: Eq

{
    f(a) == g(a)
}

pub fn fn_neq<F,G,A,B>(f: F, g: G, a: &A) -> bool
    where
    F: Fn(&A) -> B,
    G: Fn(&A) -> B,
    B: Eq
{
    f(a) != g(a)
}

// -- Cmp --------------------------------------------------------------------

// Cmp is mapped to rust's Ord
// min is rust's min
// max is rust's max

fn abs<A: Ord + Ring>(a: &A) -> A {
    if a < &A::from_integer(Integer::Z) {
        -a.clone()
    } else {
        a.clone()
    }
}

// -- SignedCmp ---------------------------------------------------------------

trait SignedCmp {
    fn slt(&self, other: &Self) -> bool;
    fn sgt(&self, other: &Self) -> bool {
        other.slt(self)
    }

    fn sleq(&self, other: &Self) -> bool {
        !(other.sgt(self))
    }

    fn sgeq(&self, other: &Self) -> bool {
        !(self.slt(other))
    }
}

// -- BitVector ---------------------------------------------------------------
trait BitVector {
    fn bv_div(&self, other: &Self) -> Self;
    fn bv_mod(&self, other: &Self) -> Self;
    fn carry(&self) -> bool;
    fn scarry(&self) -> bool;
    // TODO: is this right or should we return possibly a new type?
    fn zext(&self, n: Nat) -> Self;

    // TODO: is this right or should we return possibly a new type?
    fn sext(&self, n: Nat) -> Self;
    fn lg2(&self) -> Self;
    fn to_signed_integer(&self) -> Integer;
}

trait BitVectorShift<I: Integral>: BitVector + std::ops::Shl<I> + std::ops::Shr<I> {
    fn arith_shr(&self, i: &I) -> Self;
}

// -- Rational ---------------------------------------------------------------

struct Rational {
    numerator: Integer,
    denominator: Integer,
}

// TODO: fromZ is a no-op?



// -- Word --------------------------------------------------------------------

struct SmallWord<const SZ: usize>(u64);

impl<Rep, const LEN: usize> Seq<bool> for SmallWord<Rep, LEN> {
    fn len(&self) -> SeqLength {
        SeqLength::Finite(LEN)
    }
}






// -- Seq ---------------------------------------------------------------------

enum SeqLength {
    Finite(usize),
    Infinite
}

trait Seq<T> {
    fn len(&self) -> SeqLength;
}

trait SeqConcat<T, B: Seq<T>>: Seq<T> {
    type Output: Seq<T>;
    fn seq_concat(&self, b: &B) -> Self::Output;
}

trait SeqSplitAt<T>: Seq<T> {
    type Output: Seq<T>;
    fn seq_split_at(&self) -> (Self::Output, Self::Output);
}

trait SeqJoin<T, S: Seq<T>>: Seq<S> {
    type Output: Seq<T>;
    fn seq_join(&self) -> Self::Output;
}

trait SeqSplit<T>: Seq<T> {
    type Item: Seq<T>;
    type Output: Seq<Self::Item>;

    fn seq_split(&self, n: Nat) -> Self::Output;
}

trait SeqReverse<T>: Seq<T> {
    type Output: Seq<T>;

    fn seq_reverse(&self) -> Self::Output;
}

trait SeqTranspose<T, S: Seq<T>>: Seq<S> {
    type Item: Seq<T>;
    type Output: Seq<Self::Item>;

    fn seq_transpose(&self) -> Self::Output;
}

trait SeqTake<T>: Seq<T> {
    type Output: Seq<T>;

    fn seq_take(&self, n: Nat) -> Self::Output;
}

trait SeqDrop<T>:  Seq<T> {
    type Output: Seq<T>;

    fn seq_drop(&self, n: Nat) -> Self::Output;
}

trait SeqHead<T>: Seq<T> {
    // internal version
    fn seq_head_unchecked(&self) -> T;

    // external API
    fn seq_head(&self) -> Option<T>;
}

trait SeqLast<T>: Seq<T> {
    fn seq_last_unchecked(&self, n: Nat) -> T;
    fn seq_last(&self, n: Nat) -> Option<T>;
}

trait SeqShiftLeft<T>: Seq<T> {

}




// impl FiniteBitVector {
//     fn add(&self, other: &Self) -> Self {
//         let chunks_len = self.chunks.len().min(other.chunks.len());
//         let out_chunks = Vec::with_capacity(chunks_len);
//         let mut carry: bool = false;
//         for i in 0..chunks_len {
//             (carry, out_chunks[i]) = self.chunks[i].carrying_add(other.chunks[i], carry)
//         }

//         BitVector { size: self.size.min(other.size), chunks: out_chunks }
//     }

//     fn mul(&self, other: &Self) -> Self {
//         let chunks_len = self.chunks.len().min(other.chunks.len());
//         let out_chunks = Vec::with_capacity(chunks_len);
//         let mut carry: u64 = 0;
//         for i in 0..chunks_len {
//             (carry, out_chunks[i]) = self.chunks[i].carrying_mul(other.chunks[i], carry)
//         }

//         BitVector { size: self.size.min(other.size), chunks: out_chunks }
//     }
// }