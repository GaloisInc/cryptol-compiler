The Problem
===========

Rust's type system does not allow one to easily express some concepts that exist in the Cryptol type system - particularly arithmetic on type-level-integers.  This complicates the translation of functions such as concatenation that are parametric in these integers:

As an example consider a Cryptol function with this type:

```
concat : {front, back, a} (fin front) => [front]a -> [back]a -> [front + back] a
```

If we try to express this idea in Rust, we might be tempted to use arrays:

```rust
fn concat<A, const usize: FRONT, const usize: BACK>
  (front: [A; FRONT], back: [A; BACK]) -> [A; ???]
```

Essentially, there is no easy way for us replace `???` with to make this a legal Rust program that will work such that the resulting array will be have the statically-known length `FRONT + BACK`.  This is putting aside the fact that the specification of `concat` above does not require that `back` is finite.

To solve this, there are two major (complimentary) approaches we can consider - over-approximation and specialization.


Over-approximation
------------------

We can _over-approximate_ and write functions that can work for inputs that are essentially a superset of what the Cryptol function accepts.  For example we could consider a rust type that models the values of Cryptol sequences:

```rust
enum Seq<A> {
  // assume Stream<A> represents an infinite stream producing values of type A
  Infinite(Stream<A>), 
  Finite(Vec<A>)
}
```

While this type is a simplification lacking some of the nuance we might like - what about finite sequences that are longer than `usize`?  Putting that aside for the moment, we can write a pretty plausible signature for `concat`:

```rust
fn concat<A>(front: &Seq<A>, back: &Seq<A>) -> Seq<A>
```

The resulting function has fewer static guarantees and might do a bit more work at run-time to figure things out - for example, its implementation might proceed in separate cases after determining if `back` is finite or infinite - an example implementation might look like:

```rust
fn concat_seq<A: Clone>(front: &Seq<A>, back: &Seq<A>) -> Seq<A> {
  if let Seq::Finite(a_vec) = front {
    match back {
      Seq::Infinite(b_str) =>
        Seq::Infinite(stream_concat(&vec_as_stream(a_vec), b_str)),
      Seq::Finite(b_vec) => {
        let mut result = Vec::with_capacity(a_vec.len() + b_vec.len());
        result.extend(a_vec.iter().cloned());
        result.extend(b_vec.iter().cloned());
        Seq::Finite(result)
      }
    }
  } else {
    panic!("concat: expecting `front` to be finite")
  }
}
```

Note the `panic!` indicating that this function could potentially fail on some sets of arguments but a Cryptol program that passes type checking might never encounter this case.  However, is is possible this function that are part of the _interface_ that could be called by arbitrary Rust code.  Such functions should likely be instrumented with `assert!` and `panic!` to (at run-time) ensure the constraints specified in the Cryptol types hold.


Specialization
--------------

Instead of over-approximating, we can instead specialize - essentially breaking the function into cases statically and doing some form of type-directed resolution during the compilation process.  For example,
we might produce several functions corresponding to `concat`:

```rust
fn concat_fin<A: Clone>(a_vec: &Vec<A>, b_vec: &Vec<A>) -> Vec<A> {
  let mut result = Vec::with_capacity(a_vec.len() + b_vec.len());
  result.extend(a_vec.iter().cloned());
  result.extend(b_vec.iter().cloned());
  result
}

fn concat_infin<A: Clone>(a_vec: &Vec<A>, b_str: &Stream<A>) -> Stream<A> {
  stream_concat(&vec_as_stream(a_vec), b_str)
}
```

Note the similarity to the cases in the example implementation of `concat` using `Seq<A>`.  The Cryptol-to-Rust compiler might be able to keep track of these possible implementations of this concept of sequences and also to generate all the necessary specializations (see the `SpecializingOnSizes.md` document for more details).  

This approach may make things a little more complicated to deal with at the interface level as a user may need to figure out which version of a function to call.  This can be ameliorated somewhat by implementing overloaded interface functions using traits:

```rust
trait Concat<Back> {
  type ConcatResult;
  // here, &self plays the role of `front`
  fn concat(&self, back: &Back) -> Self::ConcatResult;
}

impl<A: Clone> Concat<Vec<A>> for Vec<A> {
  type ConcatResult = Vec<A>;
  fn concat(&self, back: &Vec<A>) -> Self::ConcatResult {
    concat_fin(self, back)
  }
}

impl<A: Clone> Concat<Stream<A>> for Vec<A> {
  type ConcatResult = Stream<A>;

  fn concat(&self, back: &Stream<A>) -> Self::ConcatResult {
    concat_infin(self, back)
  }
}
```

With this approach there's no longer a type representing sequences of _arbitrary_ length - everything must be handled case by case at specific types.  However there is nothing stopping us from adding another `impl` for `Seq`:

```rust
impl<A: Clone> Concat<Seq<A>> for Seq<A> {
  type ConcatResult = Seq<A>;

  fn concat(&self, back: &Seq<A>) -> Self::ConcatResult {
    concat_seq(self, back)
  }
}
```


Stream Representation
---------------------

Much of the above discussion deal with `Stream` does not really deal with how the `Stream` type might work or how it is implemented.

The closest analogue to possibly-infinite sequences in Rust is the `Iterator` trait which - represents an in-progress traversal - which also ties into some parts of the language like `for` loops.  For reference this is essentially:

```rust
pub trait Iterator {
  type Item;
  fn next(&mut self) -> Option<Self::Item>;
}
```

We might imagine the essential quality of a sequence as something capable of producing an iterator - for now we will put aside notions of ownership:

```rust
pub trait AsIterator<A>: {
  type Iter: Iterator<Item=A>;
  fn iterate(&self) -> Self::Iter;
}
```

Note that we do not necessarily want the existing `IntoIterator` trait since that trait consumes its input in the process of producing an Iterator implementation.

This gives us one particular possibility for representing `Stream<A>` - as a `dyn AsIterator` which can create a `dyn Iterator<A>`:

```rust
type Stream<A> = Box<dyn AsIterator<Iter=Box<dyn Iterator<A>>>>
```



Traits-Only Approach
--------------------

Rather than try to make `Stream` a type, we could try to just use traits everywhere - similar to how Rust implements many of the operations on `Iterator` in its standard library - for example, the Rust version of `concat` that works on iterators is:

```rust
// note that `self` here is the type implementing `Iterator`
fn chain<U>(self, other: U) -> Chain<Self, U::IntoIter>
where
    Self: Sized,
    U: IntoIterator<Item = Self::Item>,
{
    Chain::new(self, other.into_iter())
}
```

Which is to say it creates a value of type `Chain` that represents the iteration across two values.  We could imagine employing a similar approach in the translation from Cryptol and using traits to model sequences instead of having a particular rust type.


This suggests that we _could_ write `concat` in a similar way - by first specifying a `Concat` type:

```rust
struct Concat<A, F: AsIterator<A>, B: AsIterator<A>> {
  front: F,
  back: B,
  a: PhantomData<A>
}
```

And subsequently implementing `AsIterator<A>` on `Concat` to make it fulfill the requirements of a sequence:

```rust
impl<A, F: AsIterator<A>, B: AsIterator<A>> AsIterator<A> for Concat<A,F, B> {
    type Iter = std::iter::Chain<F::Iter, B::Iter>;

    fn iterate(&self) -> Self::Iter {
        self.front.iterate().chain(self.back.iterate())
    }
}
```
