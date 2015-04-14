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

final class ComplexOps[A[_ <: Nat], N <: Nat](cmplx : Complex[A, N]) {

  def headSpine[M[+_]](implicit sm: ShapeMonad[M]) : M[Tree[A[N], N]] = ???
    // focusSpine(complexToZipper(cmplx))

}

trait ToComplexOps {

  implicit def complexToOps[A[_ <: Nat], N <: Nat](cmplx : Complex[A, N]) : ComplexOps[A, N] = 
    new ComplexOps(cmplx)

  // object ComplexTypes {

  //   def apply[A[_ <: Nat]] : ComplexTypes[A] = 
  //     new ComplexTypes[A]

  // }

}
