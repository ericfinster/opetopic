/**
  * PTree.scala - Planar Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mutable

sealed trait PTree[+A]
case object PLeaf extends PTree[Nothing]
case class PNode[A](a: A, bs: Seq[PTree[A]]) extends PTree[A]

object PTree {

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

  }

  import opetopic._
  import syntax.tree._

  @natElim
  def apply[A, N <: Nat](n: N)(t: Tree[A, N]): PTree[A] = {
    case (Z, Pt(a)) => PNode(a, Seq(PLeaf))
    case (S(p: P), Leaf(_)) => PLeaf
    case (S(p: P), Node(a, sh)) => 
      PNode(a, sh.nodes.toSeq.map(PTree(S(p))(_)))
  }

  def apply[A, N <: Nat](t: Tree[A, N]): PTree[A] = 
    PTree(t.dim)(t)

}
