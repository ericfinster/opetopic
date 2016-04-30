/**
  * PTree.scala - Planar Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mutable

import scalaz.Traverse
import scalaz.Applicative
import scalaz.std.list._

import opetopic._
import syntax.tree._

sealed trait PTree[+A]
case object PLeaf extends PTree[Nothing]
case class PNode[A](a: A, bs: List[PTree[A]]) extends PTree[A]

object PTree {

  def graft[A](p: PTree[A], bs: List[PTree[A]], i: Int = 0): (Int, PTree[A]) = 
    p match {
      case PLeaf => (i + 1, bs(i))
      case PNode(a, as) => {

        val (j, ls) = 
          Traverse[List].mapAccumL(as, i)(
            (j, b) => graft(b, bs, j)
          )

        (j, PNode(a, ls))

      }
    }

  def join[A](pp: PTree[PTree[A]]): PTree[A] = 
    pp match {
      case PLeaf => PLeaf
      case PNode(p, bs) => 
        graft(p, bs.map(join(_)))._2
    }

  implicit class PTreeOps[A](t: PTree[A]) {

    def foreach(op : A => Unit): Unit = 
      t match {
        case PLeaf => ()
        case PNode(a, bs) => {
          for {
            b <- bs
          } { b.foreach(op) }

          op(a)
        }
      }

    def map[B](f: A => B): PTree[B] = 
      t match {
        case PLeaf => PLeaf
        case PNode(a, bs) => 
          PNode(f(a), bs.map(_.map(f)))
      }

    def nodes: List[A] = 
      t match {
        case PLeaf => List()
        case PNode(a, as) => 
          a :: as.map(_.nodes).flatten
      }

  }

  implicit val ptreeTraverse: Traverse[PTree] =
    new Traverse[PTree] {

      def traverseImpl[G[_], A, B](t: PTree[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[PTree[B]] = {

        import isAp._
        
        t match {
          case PLeaf => pure(PLeaf)
          case PNode(a, bs) => {

            val gb: G[B] = f(a)
            val gbs: G[List[PTree[B]]] = 
              Traverse[List].traverse(bs)(traverseImpl(_)(f))

            ap2(f(a), gbs)(pure(PNode(_, _))) 

          }
        }
      }

    }

  @natElim
  def apply[A, N <: Nat](n: N)(t: Tree[A, N]): PTree[A] = {
    case (Z, Pt(a)) => PNode(a, List(PLeaf))
    case (S(p: P), Leaf(_)) => PLeaf
    case (S(p: P), Node(a, sh)) => 
      PNode(a, sh.nodes.map(PTree(S(p))(_)))
  }

  def apply[A, N <: Nat](t: Tree[A, N]): PTree[A] = 
    PTree(t.dim)(t)

}
