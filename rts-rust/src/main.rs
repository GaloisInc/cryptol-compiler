use cry_rts::trait_methods::*;




// This is just for testing stuff out.
fn main() {
  let mut nats = cry_rts::stream!(<>, u64, 1 where {}, [0],
                |this| 1 + this.get_history(1));

  let mut s = cry_rts::stream!(<I : Iterator<Item=u64>>
                              , u64
                              , 2 where { nats: I = nats }
                              , [0,1],
                |this| this.get_history(1) + this.get_history(2)
                            + this.nats.next()?  );

  for (i,x) in s.take(10).enumerate() {
    println!("fib({}) = {}",i,x)
  }
}
