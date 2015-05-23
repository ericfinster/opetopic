/**
  * ComplexSyntax.scala - Syntax definitions for complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.syntax

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Applicative
import opetopic._

import TypeDefs._
import Complex._

final class ComplexOps[A[_ <: Nat], N <: Nat](cmplx : Complex[A, N]) {

  import Suite._

  type INst[K <: Nat] = Nesting[A[K], K]

  def head : Nesting[A[N], N] =
    Suite.head[INst, N](cmplx)

  def headValue : A[N] = 
    Nesting.baseValue(head)

  def headSpine : ShapeM[Tree[A[N], N]] = 
    focusSpine(cmplx.length.pred)(complexToZipper(cmplx))

  def foreach(op: IndexedOp[A]) : Unit = {
    Suite.foreach[INst, S[N]](cmplx)(new IndexedOp[INst] {
      def apply[P <: Nat](p: P)(nst: Nesting[A[P], P]) : Unit = {
        Nesting.foreach(nst)(op(p)(_))
      }
    })
  }

  def foreach(op: A[_] => Unit) : Unit = 
    foreach(new IndexedOp[A] {
      def apply[N <: Nat](n: N)(an : A[N]) : Unit = op(an)
    })

  def map[B[_ <: Nat]](f: A ~~> B) : Complex[B, N] = 
    Suite.map[INst, ({ type L[K <: Nat] = Nesting[B[K], K] })#L, S[N]](cmplx)(
      new ~~>[INst, ({ type L[K <: Nat] = Nesting[B[K], K] })#L] {
        def apply[N <: Nat](nst: INst[N]) : Nesting[B[N], N] = {
          import scalaz.Id._
          Nesting.traverse[Id, A[N], B[N], N](nst)(f(_))
        }
      }
    )

  def traverse[T[_], B[_ <: Nat]](trav: IndexedTraverse[T, A, B])(implicit apT: Applicative[T]) : T[Complex[B, N]] = {
    type BNst[K <: Nat] = Nesting[B[K], K]
    Suite.traverse[T, INst, BNst, S[N]](cmplx)(new IndexedTraverse[T, INst, BNst] {
      def apply[N <: Nat](n: N)(nst: INst[N]) : T[BNst[N]] = 
        Nesting.traverse(nst)(an => trav(n)(an))
    })
  }

  def sourceAt[K <: Nat](addr: Address[S[K]])(implicit diff: Lte.Diff[K, N]) : ShapeM[Complex[A, K]] = 
    Complex.sourceAt(diff.lte.lower)(getPrefix(diff), addr)

  def sourceAt(addr: Address[S[N]]) : ShapeM[Complex[A, N]] =
    Complex.sourceAt(cmplx.length.pred)(cmplx, addr)

  def getPrefix[K <: Nat](diff: Lte.Diff[K, N]) : Complex[A, K] = {
    Suite.drop[INst, diff.D, S[N], S[K]](Lte.lteSucc(Lte.lteInvert(diff.lte)))(cmplx)
  }

  def getNesting[K <: Nat](diff : Lte.Diff[K, N]) : Nesting[A[K], K] = 
    Suite.getAt[INst, K, N, diff.D](cmplx)(diff.lte)

  def comultiply : ShapeM[DblComplex[A, N]] = 
    Complex.comultiply(cmplx.length.pred)(cmplx)

}

trait ToComplexOps {

  implicit def complexToOps[A[_ <: Nat], N <: Nat](cmplx : Complex[A, N]) : ComplexOps[A, N] = 
    new ComplexOps(cmplx)

  implicit def finiteComplexToOps[A[_ <: Nat]](fc: FiniteComplex[A]) : ComplexOps[A, fc.N] = 
    new ComplexOps[A, fc.N](fc.value)

  implicit def doubleComplexToOps[A[_ <: Nat], N <: Nat](dc: DblComplex[A, N]) 
      : ComplexOps[({ type L[K <: Nat] = Complex[A, K] })#L, N] =
    new ComplexOps[({ type L[K <: Nat] = Complex[A, K] })#L, N](dc)

  implicit def complexToFiniteComplex[A[_ <: Nat], D <: Nat](cmplx: Complex[A, D]) : FiniteComplex[A] = 
    new Sigma[({ type L[K <: Nat] = Complex[A, K] })#L] {

      type N = D
      val n = cmplx.length.pred
      val value = cmplx

    }

}
