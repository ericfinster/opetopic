/**
  * Suite.scala - Indexed lists of items
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Applicative

import TypeDefs._

sealed trait Suite[F[_ <: Nat], L <: Nat] {

  def >>(fl : F[L]) : Suite[F, S[L]] = 
    new >>(this, fl)

  def length : L

}

case class SNil[F[_ <: Nat]]() extends Suite[F, _0] { 
  def length = Z 
  override def toString = "[]"
}

case class >>[F[_ <: Nat], P <: Nat](tl : Suite[F, P], hd : F[P]) extends Suite[F, S[P]] { 
  def length = S(tl.length) 
  override def toString = tl.toString ++ " >> " ++ hd.toString
}

object Suite {

  def head[F[_ <: Nat], N <: Nat](ts : Suite[F, S[N]]) : F[N] = 
    ts match {
      case (_ >> hd) => hd
    }

  def tail[F[_ <: Nat], N <: Nat](ts : Suite[F, S[N]]) : Suite[F, N] = 
    ts match {
      case (tl >> _) => tl
    }

  def map[F[_ <: Nat], G[_ <: Nat], L <: Nat](seq : Suite[F, L])(f : F ~~> G) : Suite[G, L] =
    seq match {
      case SNil() => SNil()
      case tail >> hd => map(tail)(f) >> f(hd)
    }

  trait SuiteTraverse[T[_], F[_ <: Nat], G[_ <: Nat]] {
    def apply[N <: Nat](fn : F[N]) : T[G[N]]
  }

  def traverse[T[_], F[_ <: Nat], G[_ <: Nat], L <: Nat](seq: Suite[F, L])(trav: SuiteTraverse[T, F, G])(
    implicit apT: Applicative[T]
  ) : T[Suite[G, L]] = 
    seq match {
      case SNil() => apT.pure(SNil())
      case tl >> hd => {
        apT.ap2(traverse(tl)(trav), trav(hd))(apT.pure(
          (newTl: Suite[G, Nat], newHd: G[Nat]) => newTl >> newHd
        ))
      }
    }

}
