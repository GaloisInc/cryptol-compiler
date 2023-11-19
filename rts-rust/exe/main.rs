
trait Dup {
  fn dup(&self) -> Box<Self>;
}

trait I : Iterator<Item=u64> + Dup {
}

struct Str {
  bo: Box<dyn I>
}

impl Dup for Str {
  fn dup(&self) -> Self {
    self.dup()
  }
}

impl Iterator for Str {
  type Item=u64;
  fn next(&mut self) -> Option<Self::Item> {
    self.bo.next()
  }
}

fn mk(x: impl Iterator<Item=u64> + Dup) -> Str {
  Str { bo: Box::new(x) }
}


fn f(x: Str) {
  println!("{:?}", x.dup().collect::<Vec<_>>());
  println!("{:?}", x.collect::<Vec<_>>())
}


// This is just for testing stuff out.
fn main() {
  let xs = mk(vec![1,2,3].into_iter());
  f(xs);


  let nats = cry_rts::stream!
        { forall = []
        , element = u64
        , history = 1
        , capture = []
        , init = [0]
        , step = |this| 1 + this.get_history(0)
        };

  let s = cry_rts::stream!
        { forall = [I : [cry_rts::Type, Iterator<Item=u64>, Clone]]
        , element = u64
        , history = 2
        , capture = [ nats: I = nats.clone() ]
        , init = [0,1]
        , step = |this| this.get_history(0) + this.get_history(1)
                                            + this.nats.next()?
        };

  for (i,x) in s.take(10).enumerate() {
    println!("fib({}) = {}",i,x)
  }

}
