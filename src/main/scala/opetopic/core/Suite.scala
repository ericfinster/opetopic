/**
  * Suite.scala - Indexed lists of items
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Applicative

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

//   class TypeSeqOps[F[_ <: Nat], L <: Nat](seq : TypeSeq[F, L]) {

//     def map[G[_ <: Nat]](f : F ~~> G) : TypeSeq[G, L] = 
//       TypeSeq.map(seq)(f)

//   }

//   implicit def toTypeSeqOps[F[_ <: Nat], L <: Nat](seq : TypeSeq[F, L]) : TypeSeqOps[F, L] = 
//     new TypeSeqOps(seq)

}

// //============================================================================================
// // INDEXED CONSTRUCTOR SEQUENCES
// //

// sealed trait ConsSeq[F[_ <: Nat, +_], L <: Nat, +A] {

//   def >>>[B >: A](fn : F[L, B]) : ConsSeq[F, S[L], B] = 
//     new >>>(this, fn)

//   def length : L

// }

// case class CNil[F[_ <: Nat, +_]]() extends ConsSeq[F, _0, Nothing] { 
//   def length = Z 
//   override def toString = "[]"
// }

// case class >>>[F[_ <: Nat, +_], P <: Nat, A](tl : ConsSeq[F, P, A], hd : F[P, A]) extends ConsSeq[F, S[P], A] { 
//   def length = S(tl.length) 
//   override def toString = tl.toString ++ " >>> \n" ++ hd.toString
// }

// trait ConsFold[F[_ <: Nat, +_], A] {

//   type Out[N <: Nat]

//   def caseZero : Out[_0]
//   def caseSucc[P <: Nat](p : P, fp : F[P, A], ih : Out[P]) : Out[S[P]]

// }

// object ConsSeq {

//   def head[F[_ <: Nat, +_], N <: Nat, A](cs : ConsSeq[F, S[N], A]) : F[N, A] = 
//     cs match {
//       case (_ >>> hd) => hd
//     }

//   def tail[F[_ <: Nat, +_], N <: Nat, A](cs : ConsSeq[F, S[N], A]) : ConsSeq[F, N, A] = 
//     cs match {
//       case (tl >>> _) => tl
//     }

//   def drop[F[_ <: Nat, +_], K <: Nat, N <: Nat, D <: Nat, A](cs : ConsSeq[F, N, A])(lte : Lte[K, N, D]) : ConsSeq[F, D, A] = 
//     (new LteCaseSplit {

//       type Out[K <: Nat, N <: Nat, D <: Nat] = ConsSeq[F, N, A] => ConsSeq[F, D, A]

//       def caseZero[N <: Nat](n : N) : Out[_0, N, N] = 
//         cs => cs

//       def caseSucc[M <: Nat, N <: Nat, D <: Nat](plte : Lte[M, N, D]) : Out[S[M], S[N], D] = {
//         case (tl >>> hd) => drop(tl)(plte)
//       }

//     })(lte)(cs)

//   def getAt[F[_ <: Nat, +_], K <: Nat, N <: Nat, D <: Nat, A](cs : ConsSeq[F, S[N], A])(lte : Lte[K, N, D]) : F[K, A] = 
//     head(drop(cs)(Lte.lteSucc(Lte.lteInvert(lte))))

//   def fold[F[_ <: Nat, +_], N <: Nat, A](cs : ConsSeq[F, N, A])(fld : ConsFold[F, A]) : fld.Out[N] = 
//     cs match {
//       case CNil() => fld.caseZero
//       case tl >>> hd => fld.caseSucc(tl.length, hd, fold(tl)(fld))
//     }

//   class ConsSuccOps[F[_ <: Nat, +_], P <: Nat, A](seq : ConsSeq[F, S[P], A]) {

//     def head : F[P, A] = 
//       ConsSeq.head(seq)

//     def tail : ConsSeq[F, P, A] = 
//       ConsSeq.tail(seq)

//     def fold(fld : ConsFold[F, A]) : fld.Out[S[P]] = 
//       ConsSeq.fold(seq)(fld)

//     def getAt[K <: Nat, D <: Nat](lte : Lte[K, P, D]) : F[K, A] = 
//       ConsSeq.getAt(seq)(lte)

//     def get[K <: Nat, D <: Nat](k : K)(implicit lte : Lte[K, P, D]) : F[K, A] = 
//       ConsSeq.getAt(seq)(lte)

//   }

//   implicit def toConsSuccOps[F[_ <: Nat, +_], P <: Nat, A](seq : ConsSeq[F, S[P], A]) : ConsSuccOps[F, P, A] = 
//     new ConsSuccOps(seq)

// }
