/**
  * Monad.scala - Quick Monads and instances
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopictt.nbe

trait Functor[F[_]] {
  def map[A, B](f: A => B)(fa: F[A]): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def ret[A](a: A): F[A]
  def bind[A, B](fa: F[A], f: A => F[B]): F[B]
  def join[A](ffa: F[F[A]]): F[A] = 
    bind[F[A], A](ffa, i => i)
}

trait Comonad[F[_]] extends Functor[F] {
  def obs[A](f: F[A]) : A
  def dup[A](f: F[A]) : F[F[A]]
}

sealed trait Free[F[+_], +A]
case class Return[F[+_], +A](a : A) extends Free[F, A]
case class Join[F[+_], +A](fix: F[Free[F, A]]) extends Free[F, A]

case class Cofree[F[+_], +A](a: A, s: F[Cofree[F, A]])

object Monad {

  def apply[F[_]](implicit m: Monad[F]): Monad[F] = m

  type Id[+A] = A

  implicit object IdMonad extends Monad[Id] {
    def map[A, B](f: A => B)(a: A) : B = f(a)
    def ret[A](a: A): A = a
    def bind[A, B](a: A, f: A => B): B = f(a)
  }

  implicit def cofreeComonad[F[+_]](implicit fn: Functor[F]): Comonad[({ type L[A] = Cofree[F, A] })#L] = 
    new Comonad[({ type L[A] = Cofree[F, A] })#L] {

      def map[A, B](f: A => B)(ca: Cofree[F, A]): Cofree[F, B] = 
        ca match {
          case Cofree(a, c) => Cofree(f(a), c.map(map(f)(_)))
        }

      def obs[A](ca: Cofree[F, A]): A = 
        ca match {
          case Cofree(a, _) => a
        }

      def dup[A](ca: Cofree[F, A]): Cofree[F, Cofree[F, A]] = 
        ca match {
          case Cofree(a, s) => Cofree(ca, s.map(dup(_)))
        }

    }

  implicit def freeMonad[F[+_]](implicit fn: Functor[F]) : Monad[({ type L[A] = Free[F, A] })#L] =
    new Monad[({ type L[A] = Free[F, A] })#L] {
      type FreeF[A] = Free[F, A]

      def map[A, B](f: A => B)(fa: FreeF[A]): FreeF[B] = 
        fa match {
          case Return(a) => Return(f(a))
          case Join(fix) => Join(fn.map((ff: FreeF[A]) => map(f)(ff))(fix))
        }

      def ret[A](a: A): FreeF[A] = Return(a)

      def bind[A, B](fa: FreeF[A], f: A => FreeF[B]): FreeF[B] = 
        fa match {
          case Return(a) => f(a)
          case Join(fx) => Join(fn.map((ff: FreeF[A]) => join(map(f)(ff)))(fx))
        }

    }

  def interp[M[+_], A](fm: Free[M, A])(implicit mnd: Monad[M]): M[A] = 
    fm match {
      case Return(a) => mnd.ret(a)
      case Join(j) => mnd.join(j.map(interp(_)))
    }

  def liftF[F[+_], A](fa: F[A])(implicit f: Functor[F]): Free[F, A] = 
    Join[F, A](f.map((a: A) => Return[F, A](a))(fa))

  class FunctorOps[F[_], A](fa: F[A])(implicit fn: Functor[F]) {
    def map[B](f: A => B): F[B] = fn.map(f)(fa)
  }

  class MonadOps[F[_], A](fa: F[A])(implicit m: Monad[F]) {
    def flatMap[B](f: A => F[B]): F[B] = m.bind(fa, f)
  }

  implicit def toFunctorOps[F[_], A](fa: F[A])(implicit fn: Functor[F]): FunctorOps[F, A] = 
    new FunctorOps[F, A](fa)

  implicit def toMonadOps[F[_], A](fa: F[A])(implicit m: Monad[F]): MonadOps[F, A] = 
    new MonadOps[F, A](fa)

  // For inspiration ...
  type Kleisli[E, A] = E => A

  def kunit[E, A](a: A): Kleisli[E, A] = _ => a
  def kbind[E, A, B](m: Kleisli[E, A], f: A => Kleisli[E, B]): Kleisli[E, B] = (e: E) => f(m(e))(e)
  def kjoin[E, A](m: Kleisli[E, Kleisli[E, A]]): Kleisli[E, A] = (e: E) => m(e)(e)

  // Continuations
  type Cont[R, A] = (A => R) => R

  def contMap[R, A, B](f: A => B)(ca: Cont[R, A]) : Cont[R, B] = 
    (k : (B => R)) => ca((a: A) => k(f(a)))

  def contUnit[R, A](a: A): Cont[R, A] = k => k(a)
  def contBind[R, A, B](ca: Cont[R, A], f: A => Cont[R, B]): Cont[R, B] = 
    (k : (B => R)) => ca((a : A) => f(a)(k))

}
