/**
  * State.scala - State Monad Definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mtl

object State {

  type State[S, A] = S => (S, A)

  def apply[S, A](m: S => (S, A)): State[S, A] = m
  def run[S, A](m: State[S, A])(s: S) : (S, A) = m(s)
  def exec[S, A](f: State[S, A])(s: S): S = f(s)._1
  def eval[S, A](f: State[S, A])(s: S): A = f(s)._2

  def get[S]: State[S, S] = 
    (s: S) => (s, s)

  def put[S](s: S): State[S, Unit] = 
    (s : S) => (s, ())

  def modify[S](f: (S) => S): State[S, Unit] = 
    (s : S) => (f(s), ())

  def stateIsMonad[S] : Monad[Lambda[A => State[S, A]]] = 
    new Monad[Lambda[A => State[S, A]]] {

      def pure[A](a: A) : State[S, A] = 
        (s: S) => (s, a)

      def flatMap[A, B](m: State[S, A])(f: A => State[S, B]): State[S, B] = 
        (s: S) => { val (ss, a) = m(s) ; f(a)(ss) }

    }


}
