/**
  * FTree.scala - The Free Monad on Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Monad
import scalaz.Traverse
import scalaz.Applicative
import scalaz.syntax.traverse._
import scalaz.std.option._

sealed trait FTree[+A]
case class FUnit[+A](a: A) extends FTree[A]
case class FJoin[+A](t: STree[FTree[A]]) extends FTree[A]

object FTree {

  implicit object FTreeTraverse extends Traverse[FTree] {
    def traverseImpl[G[_], A, B](ft: FTree[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[FTree[B]] =
      ft match {
        case FUnit(a) => isAp.ap(f(a))(isAp.pure(FUnit(_)))
        case FJoin(t) => isAp.ap(t.traverse(traverseImpl(_)(f)))(isAp.pure(FJoin(_)))
      }
  }

  implicit object FTreeMonad extends Monad[FTree] {

    def point[A](a: => A): FTree[A] = FUnit(a)

    def bind[A, B](fa: FTree[A])(f: A => FTree[B]): FTree[B] = 
      fa match {
        case FUnit(a) => f(a)
        case FJoin(t) => FJoin(t.map(bind(_)(f)))
      }

  }

  implicit class FTreeOps[A](ft: FTree[A]) {

    

  }

}

