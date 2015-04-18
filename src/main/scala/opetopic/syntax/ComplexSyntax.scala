/**
  * ComplexSyntax.scala - Syntax definitions for complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.syntax

import scala.language.higherKinds
import scala.language.implicitConversions

import opetopic._

import TypeDefs._
import Complex._

final class ComplexOps[A[_ <: Nat], N <: Nat](cmplx : Complex[A, N]) {

  import Suite._

  type INst[K <: Nat] = Nesting[A[K], K]

  def head : Nesting[A[N], N] =
    Suite.head[INst, N](cmplx)

  def headSpine : ShapeM[Tree[A[N], N]] = 
    focusSpine(complexToZipper(cmplx))

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

  def sourceAt(addr: Address[S[N]]) : ShapeM[Complex[A, N]] =
    Complex.sourceAt(cmplx, addr)

  def getNesting[K <: Nat](diff : Lte.Diff[K, N]) : Nesting[A[K], K] = 
    Suite.getAt[INst, K, N, diff.D](cmplx)(diff.lte)

  def comultiply : ShapeM[DblComplex[A, N]] = 
    Complex.comultiply(cmplx)

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
