/**
  * Tree.scala - Higher Dimensional Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import Nats._

sealed abstract class Tree[N <: Nat[N], +A] { def dim : N }
case class Pt[+A](a : A) extends Tree[Z, A] { def dim : Z = Z() }
case class Leaf[P <: Nat[P]](sp : S[P]) extends Tree[S[P], Nothing] { def dim : S[P] = sp }
case class Node[P <: Nat[P], +A](a : A, shell : Tree[P, Tree[S[P], A]]) extends Tree[S[P], A] { def dim : S[P] = S(shell.dim) }

trait TreeFunctions {

  //============================================================================================
  // MAP
  //

  def map[N <: Nat[N], A, B](tr : Tree[N, A])(f : A => B) : Tree[N, B] = 
    (new NatCaseSplit {

      type Out[N <: Nat[N]] = Tree[N, A] => Tree[N, B]

      def caseZero : Tree[Z, A] => Tree[Z, B] = {
        case Pt(a) => Pt(f(a))
      }

      def caseSucc[P <: Nat[P]](p : P) : Tree[S[P], A] => Tree[S[P], B] = {
        case Leaf(sp) => Leaf(sp)
        case Node(a, sh) => Node(f(a), map(sh)(map(_)(f)))
      }

    })(tr.dim)(tr)

  //============================================================================================
  // UNZIP
  //

  def unzip[N <: Nat[N], A, B](tr : Tree[N, (A, B)]) : (Tree[N, A], Tree[N, B]) = 
    (new NatCaseSplit {

      type Out[N <: Nat[N]] = Tree[N, (A, B)] => (Tree[N, A], Tree[N, B])

      def caseZero : Tree[Z, (A, B)] => (Tree[Z, A], Tree[Z, B]) = {
        case Pt((a, b)) => (Pt(a), Pt(b))
      }

      def caseSucc[P <: Nat[P]](p : P) : Tree[S[P], (A, B)] => (Tree[S[P], A], Tree[S[P], B]) = {
        case Leaf(sp) => (Leaf(sp), Leaf(sp))
        case Node((a, b), sh) => {
          val (aSh, bSh) = unzip(map(sh)(unzip(_)))
          (Node(a, aSh), Node(b, bSh))
        }
      }

    })(tr.dim)(tr)

}
