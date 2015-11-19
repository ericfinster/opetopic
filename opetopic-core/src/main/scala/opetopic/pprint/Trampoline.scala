/**
  * Trampoline.scala - Implement Trampolines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.pprint

import scalaz._

// sealed trait Trampoline[+A] {

//   def resume : Either[() => Trampoline[A], A] =
//     this match {
//       case Done(a) => Right(a)
//       case Call(k) => Left(k)
//       case Suspend(a, f) =>
//         a match {
//           case Done(v) => f(v).resume
//           case Call(k) => Left(() => Suspend(k(), f))
//           case Suspend(b, g) => (Suspend(b, (x: Any) => Suspend(g(x), f)) : Trampoline[A]).resume
//         }
//     }

//   def run : A = ???
//   // this match {
//   //   case Call(thunk) => thunk().run
//   //   case Done(x) => x
//   // }

// }

// case class Done[+A](result: A) extends Trampoline[A]
// case class Call[+A](thunk: () => Trampoline[A]) extends Trampoline[A]
// case class Suspend[A, +B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]
