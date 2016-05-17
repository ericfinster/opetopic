/**
  * Suite.scala - Custom sequence type for complexes, cardinals, etc.
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Traverse
import scalaz.Applicative

import scalaz.syntax.traverse._

sealed trait Suite[+A] {

  def >>[B >: A](b: B): Suite[B] = 
    new >>(this, b)

}

case class ||[+A](a: A) extends Suite[A]
case class >>[+A](s: Suite[A], a: A) extends Suite[A]

object Suite {

  implicit def toBase[A](a: A): Suite[A] = ||(a)

  implicit object SuiteTraverse extends Traverse[Suite] {

    def traverseImpl[G[_], A, B](s: Suite[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[Suite[B]] = {

      import isAp._

      s match {
        case ||(a) => ap(f(a))(pure(||(_)))
        case as >> a => ap2(traverseImpl(as)(f), f(a))(pure(>>(_, _)))
      }

    }

  }

  implicit class SuiteOps[A](s: Suite[A]) {

    def head: A = 
      s match {
        case ||(a) => a
        case _ >> a => a
      }

    def withHead(a: A): Suite[A] = 
      s match {
        case ||(_) => ||(a)
        case tl >> _ => tl >> a
      }

    def tail: Option[Suite[A]] = 
      s match {
        case ||(_) => None
        case tl >> _ => Some(tl)
      }

    def foreach(op: A => Unit): Unit = 
      s match {
        case ||(a) => op(a)
        case tl >> a => {
          tl.foreach(op)
          op(a)
        }
      }

  }

}
