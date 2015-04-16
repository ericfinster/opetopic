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
      def apply[P <: Nat](nst: Nesting[A[P], P]) : Unit = {
        Nesting.foreach(nst)(op(_))
      }
    })
  }

  def foreach(op: A[_] => Unit) : Unit = 
    foreach(new IndexedOp[A] {
      def apply[N <: Nat](an : A[N]) : Unit = 
        op(an)
    })

  def sourceAt(addr: Address[S[N]]) : ShapeM[Complex[A, N]] =
    Complex.sourceAt(cmplx, addr)

}

trait ToComplexOps {

  implicit def complexToOps[A[_ <: Nat], N <: Nat](cmplx : Complex[A, N]) : ComplexOps[A, N] = 
    new ComplexOps(cmplx)

  // object ComplexTypes {

  //   def apply[A[_ <: Nat]] : ComplexTypes[A] = 
  //     new ComplexTypes[A]

  // }

}
