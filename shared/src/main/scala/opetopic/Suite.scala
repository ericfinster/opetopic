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

  def drop[F[_ <: Nat], K <: Nat, N <: Nat, D <: Nat](suite : Suite[F, N])(implicit lte : Lte[K, N, D]) : Suite[F, D] = 
    (new LteCaseSplit {

      type Out[K <: Nat, N <: Nat, D <: Nat] = Suite[F, N] => Suite[F, D]

      def caseZero[N <: Nat](n: N) : Out[_0, N, N] = 
        cs => cs

      def caseSucc[K <: Nat, N <: Nat, D <: Nat](plte: Lte[K, N, D]) : Out[S[K], S[N], D] = {
        case (tl >> hd) => drop(tl)(plte)
      }

    })(lte)(suite)


  def getAt[F[_ <: Nat], K <: Nat, N <: Nat, D <: Nat](suite : Suite[F, S[N]])(implicit lte: Lte[K, N, D]) : F[K] =
    head(drop(suite)(Lte.lteSucc(Lte.lteInvert(lte))))

  def foreachWithCount[F[_ <: Nat], N <: Nat](suite: Suite[F, N])(op: IndexedOp[F]) : (Unit, N) = 
    suite match {
      case SNil() => ((), Z)
      case (tl >> hd) => {
        val (u, l) = foreachWithCount(tl)(op)
        (op(l)(hd), S(l))
      }
    }

  def foreach[F[_ <: Nat], N <: Nat](suite: Suite[F, N])(op: IndexedOp[F]) : Unit = 
    foreachWithCount(suite)(op)._1

  def foldWithCount[F[_ <: Nat], A, N <: Nat](suite: Suite[F, N])(fld: IndexedFold[F, A]) : (A, N) = 
    suite match {
      case SNil() => (fld.caseZero, Z)
      case tl >> hd => {
        val (res, p) = foldWithCount(tl)(fld)
        (fld.caseSucc(p)(hd, res), S(p))
      }
    }

  def fold[F[_ <: Nat], A , N <: Nat](suite: Suite[F, N])(fld: IndexedFold[F, A]) : A =
    foldWithCount(suite)(fld)._1

  def traverseWithCount[T[_], F[_ <: Nat], G[_ <: Nat], L <: Nat](seq: Suite[F, L])(trav: IndexedTraverse[T, F, G])(
    implicit apT: Applicative[T]
  ) : (T[Suite[G, L]], L) = 
    seq match {
      case SNil() => (apT.pure(SNil()), Z)
      case tl >> hd => {
        val (t, l) = traverseWithCount(tl)(trav)
        (apT.ap2(t, trav(l)(hd))(apT.pure(
          (newTl: Suite[G, Nat], newHd: G[Nat]) => newTl >> newHd
        )), S(l))
      }
    }

  def traverse[T[_], F[_ <: Nat], G[_ <: Nat], L <: Nat](seq: Suite[F, L])(trav: IndexedTraverse[T, F, G])(
    implicit apT: Applicative[T]
  ) : T[Suite[G, L]] = traverseWithCount[T, F, G, L](seq)(trav)._1

  import upickle._
  import scala.{PartialFunction => PF}

  implicit def suiteWriter[F[_ <: Nat], N <: Nat](implicit iwrtr: IndexedWriter[F]) : Writer[Suite[F, N]] = 
    new Writer[Suite[F, N]] {
      def write0: Suite[F, N] => Js.Value = {
        suite => Js.Arr(writeWithCount(suite)._1: _*)
      }

      def writeWithCount[K <: Nat](s: Suite[F, K]) : (List[Js.Value], Nat) = 
        s match {
          case SNil() => (Nil, Z)
          case (tl >> hd) => {
            writeWithCount(tl) match {
              case (vs, p) => {
                val v : Js.Value = iwrtr.writer.write(hd)
                (v :: vs, S(p))
              }
            }
          }
        }
    }

  implicit def suiteReader[F[_ <: Nat], N <: Nat](implicit irdr: IndexedReader[F]) : Reader[Suite[F, N]] = 
    new Reader[Suite[F, N]] {
      def read0: PF[Js.Value, Suite[F, N]] = {
        case Js.Arr(v) => ???
      }
    }

}

