// Some examples of the kind of code we aim to generate for
// recursive stream equations.

// A basic iterator with 2 history
// fibs = [0,1] # [ x + y | x <- fibs, y <- drop`{1} fibs ]
fn fibs() -> impl Iterator<Item=u32> {

  {
    type Element = u32;

    struct State {
      index:   usize,
      history: [Element; 2]
    }

    impl Iterator for State {
      type Item = Element;
      fn next(&mut self) -> Option<Element> {
        let element =
          match self.index {
            0 => 0,
            1 => 1,
            _ => {
              let x = self.history[(self.index - 1) % 2];
              let y = self.history[(self.index - 2) % 2];
              x + y
            }
          };
        self.history[self.index % 2] = element;
        self.index += 1;
        Some(element)
      }
    }

    State { index: 0, history: [0xDEAD,0xBEEF] }
  }
}




// An example of an interator expression that uses an external stream
// sums ps = [0] # [ p + s | p <- ps | s <- sums ]
fn sums<I : Iterator<Item = u32>>(ps : I) -> impl Iterator<Item = u32> {

  {
    type Element = u32;

    // Needs parameter to store the state of the external iterator
    struct State<I : Iterator<Item = Element>> {
      index:   usize,
      history: Element, // Single element, no need for array
      ps: I             // External element
    }

    impl<I : Iterator<Item = Element>> Iterator for State<I> {
      type Item = Element;
      fn next(&mut self) -> Option<Element> {
        let p = self.ps.next()?;     // Get from external
        let s = match self.index {
                  0 => 0,
                  _ => self.history // No array, so no modulus
        };
        let element = p + s;
        self.history = element;
        self.index += 1;
        Some(element)
      }
    }

    State { ps : ps, history: 0xDEAD, index: 0 }
  }
}


fn zig_zag() -> (impl Iterator<Item = u32>, impl Iterator<Item = u32>) {

  {
    struct State {
      index: usize,
      history_zig: u32,
      history_zag: u32
    }

    impl Iterator for State {
      type Item = (u32,u32);
      fn next(&mut self) -> Option<(u32,u32)> {
        let zig =
          match self.index {
            0 => 0,
            _ => self.history_zag
          };
        let zag =
          match self.index {
            0 => 1,
            _ => self.history_zig
          };
        self.history_zig = zig;
        self.history_zag = zag;
        self.index += 1;
        Some ((zig,zag))
      }
    }

    let zig = State { index: 0, history_zig: 0xDEAD, history_zag: 0xBEEF };
    let zag = State { index: 0, history_zig: 0xDEAD, history_zag: 0xBEEF };
    (zig.map(|(a,_)| a),zag.map(|(_,b)| b))
  }


}




fn main() {

  print!("Fibs:\n");
  let fis = fibs();
  for i in fis.take(10) {
    print!("{}\n",i)
  }


  print!("Sums:\n");
  let sus = sums([1,2,3,4,5,6].iter().cloned());
  for i in sus.take(10) {
    print!("{}\n",i)
  }

  let (zig,zag) = zig_zag();
  print!("Zig:\n");
  for i in zig.take(5) {
    print!("{}\n",i)
  }

  print!("Zag:\n");
  for i in zag.take(5) {
    print!("{}\n",i)
  }




}
