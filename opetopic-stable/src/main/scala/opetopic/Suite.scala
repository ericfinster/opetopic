/**
  * Suite.scala - Custom sequence type for complexes, cardinals, etc.
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

sealed trait Suite[+A] {

  def >>[B >: A](b: B): Suite[B] = 
    new >>(this, b)

}

case class ||[+A](a: A) extends Suite[A]
case class >>[+A](s: Suite[A], a: A) extends Suite[A] {
  override def toString = s.toString + " >> " + a.toString
}

object Suite {

  implicit def toBase[A](a: A): Suite[A] = ||(a)

  implicit object SuiteTraverse extends Traverse[Suite] {

    def traverse[G[_], A, B](s: Suite[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[Suite[B]] = {

      import isAp._

      s match {
        case ||(a) => ap(f(a))(pure(||(_)))
        case as >> a => ap2(traverse(as)(f), f(a))(pure(>>(_, _)))
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

    def foldRight[B](b: => B)(f: (A, => B) => B): B = 
      s match {
        case ||(a) => f(a, b)
        case tl >> hd => f(hd, foldRight(b)(f))
      }

    def zipWithSuite[B](t: Suite[B]): Suite[(A, B)] = 
      (s, t) match {
        case (||(a), tt) => ||(a, tt.head)
        case (ss, ||(b)) => ||(ss.head, b)
        case (atl >> ahd, btl >> bhd) =>
          atl.zipWithSuite(btl) >> (ahd, bhd)
      }

    def length: Int = 
      s match {
        case ||(_) => 1
        case tl >> _ => tl.length + 1
      }

    def drop(i: Int): Suite[A] = 
      if (i <= 0) s else
        s match {
          case ||(a) => ||(a)
          case tl >> _ => tl.drop(i - 1)
        }

    def take(i: Int): Suite[A] = 
      drop(length - i)

    def grab(i: Int, l: List[A] = List()): (Suite[A], List[A]) = 
      s match {
        case ||(a) => (||(a), l)
        case tl >> hd => 
          if (i <= 0) (s, l) else {
            tl.grab(i - 1, hd :: l)
          }
      }

    def splitAt(i: Int): (Suite[A], List[A]) = 
      grab(length - i)

    def ++(l: List[A]): Suite[A] = 
      l match {
        case Nil => s
        case l :: ls => (s >> l) ++ ls
      }

  }

}
