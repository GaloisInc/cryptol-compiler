

// This is just for testing stuff out.
fn main() {

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
