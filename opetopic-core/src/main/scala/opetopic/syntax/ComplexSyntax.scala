/**
  * ComplexSyntax.scala - Syntax definitions for complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.syntax

import scalaz.Applicative
import opetopic._

import TypeLemmas._
import Complex._

final class ComplexOps[A[_ <: Nat], N <: Nat](cmplx : Complex[A, N]) {

  import Suite._

  type INst[K <: Nat] = Nesting[A[K], K]

  def dim : N = 
    cmplx.length.pred

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

  def map[B[_ <: Nat]](f: IndexedMap[A, B]) : Complex[B, N] = 
    Suite.map[INst, ({ type L[K <: Nat] = Nesting[B[K], K] })#L, S[N]](cmplx)(
      new IndexedMap[INst, ({ type L[K <: Nat] = Nesting[B[K], K] })#L] {
        def apply[N <: Nat](n: N)(nst: INst[N]) : Nesting[B[N], N] = {
          import scalaz.Id._
          Nesting.traverse[Id, A[N], B[N], N](nst)(f(n)(_))
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

  def sourceAt[K <: Nat](addr: Address[S[K]])(implicit diff: Diff[K, N]) : ShapeM[Complex[A, K]] = 
    Complex.sourceAt(diff.lte.lower)(getPrefix(diff), addr)

  def sourceAt(addr: Address[S[N]]) : ShapeM[Complex[A, N]] =
    Complex.sourceAt(cmplx.length.pred)(cmplx, addr)

  def sourceAt[K <: Nat](k: K)(addr: Address[S[K]]) : ShapeM[Complex[A, K]] = 
    for {
      diff <- fromOpt(diffOpt(k, cmplx.length.pred))
      srcCmplx <- sourceAt(addr)(diff)
    } yield srcCmplx

  def getPrefix[K <: Nat](diff: Diff[K, N]) : Complex[A, K] = 
    Suite.drop[INst, diff.D, S[N], S[K]](lteSucc(lteInvert(diff.lte)))(cmplx)

  def getNesting[K <: Nat](diff : Diff[K, N]) : Nesting[A[K], K] = 
    Suite.getAt[INst, K, N, diff.D](cmplx)(diff.lte)

  def comultiply : ShapeM[DblComplex[A, N]] = 
    Complex.comultiply(cmplx.length.pred)(cmplx)

  def apply[K <: Nat](k: K)(implicit diff: Diff[K, N]) : Nesting[A[K], K] = 
    Suite.getAt[INst, K, N, diff.D](cmplx)(diff.lte)

  def toCardinal : Cardinal[A, N] = 
    Cardinal.complexToCardinal(cmplx.length.pred)(cmplx)._1

}

final class ComplexSuccOps[A[_ <: Nat], N <: Nat](cmplx : Complex[A, S[N]]) {

  def tail : Complex[A, N] = 
    cmplx match {
      case Complex(tl, _) => tl
    }

  def target : ShapeM[Complex[A, N]] = {

    import TypeLemmas._

    implicit val n : N = cmplx.length.pred.pred
    val d : Diff[N, S[N]] = implicitly[Diff[N, S[N]]]

    val cops = new ComplexOps(cmplx)
    val blorp: Complex[A, N] = cops.getPrefix[N](d)

    new ComplexOps(blorp).sourceAt(Nil)

  }

}

trait ToComplexOps {

  implicit def complexToOps[A[_ <: Nat], N <: Nat](cmplx : Complex[A, N]) : ComplexOps[A, N] = 
    new ComplexOps(cmplx)

  implicit def complexToSuccOps[A[_ <: Nat], N <: Nat](cmplx: Complex[A, S[N]]) : ComplexSuccOps[A, N] = 
    new ComplexSuccOps(cmplx)

  implicit def finiteComplexToOps[A[_ <: Nat]](fc: FiniteComplex[A]) : ComplexOps[A, fc.N] = 
    new ComplexOps[A, fc.N](fc.value)

  implicit def doubleComplexToOps[A[_ <: Nat], N <: Nat](dc: DblComplex[A, N]) 
      : ComplexOps[({ type L[K <: Nat] = Complex[A, K] })#L, N] =
    new ComplexOps[({ type L[K <: Nat] = Complex[A, K] })#L, N](dc)

  implicit def doubleComplexToSuccOps[A[_ <: Nat], N <: Nat](dc: DblComplex[A, S[N]])
      : ComplexSuccOps[Lambda[`K <: Nat` => Complex[A, K]], N] =
    new ComplexSuccOps[Lambda[`K <: Nat` => Complex[A, K]], N](dc)

  implicit def complexToFiniteComplex[A[_ <: Nat], D <: Nat](cmplx: Complex[A, D]) : FiniteComplex[A] = 
    Sigma[Lambda[`N <: Nat` => Complex[A, N]], D](cmplx.length.pred)(cmplx)

}
