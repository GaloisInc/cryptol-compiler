use std::rc::Rc;

pub type Stream<A> = Rc<dyn StreamInitialState<A>>;
pub trait StreamInitialState<A> {
  fn iter(&self) -> Box<dyn Iterator<Item=A>>;
}

// impl<F,I,T> StreamInitialState<T> for F
//   where
//     I: Iterator<Item=T> + 'static,
//     F: Fn() -> I + 'static {
//   fn iter(&self) -> Box<dyn Iterator<Item=T>> {
//       Box::new(self())
//   }
// }

// impl<'a, T, I: Iterator<Item = T> + Clone> CloneableIter<'a, T> for I {
//     fn clone_stream(&self) -> Box<dyn CloneableIter<T> + 'a> {
//         Box::new(self.clone())
//     }
// }


// ----------------------------------------------------------------------------

// fn mkstream<'a, F, I, T>(f: F) -> Stream<T>
//   where
//     I: Iterator<Item = T>,
//     F: Fn() -> I {
//       Rc::new(f)
// }

// fn take<A>(s: Stream<A>, n: usize) -> Stream<A> {
//   mkstream(move || { s.iter().take(n) })
// }

// fn drop<A>(s: Stream<A>, n: usize) -> Stream<A> {
//   mkstream(move || { s.iter().skip(n) })
// }

// fn break_at<A>(s: Stream<A>, n: usize) -> (Stream<A>, Stream<A>) {
//   (take(s.clone(), n), drop(s, n))
// }

// fn zip<A>(s1: Stream<A>, s2: Stream<A>) -> Stream<(A,A)> {
//   mkstream(move || { s1.iter().zip(s2.iter()) })
// }