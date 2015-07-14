/**
  * SuiteSyntax.scala - Syntax for Suites
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.syntax

import opetopic._

final class SuiteOps[A[_ <: Nat], N <: Nat](suite: Suite[A, N]) {

  def fold[B](fld: IndexedFold[A, B]) : B = 
    Suite.fold[A, B, N](suite)(fld)

}

final class SuiteSuccOps[A[_ <: Nat], P <: Nat](suite: Suite[A, S[P]]) {

  def head: A[P] = 
    Suite.head(suite)

  def tail: Suite[A, P] = 
    Suite.tail(suite)

}

trait LowPriorityOps0 {

  // The idea is that this will keep us from conflicting with the same name for 
  // complexes ... but maybe we should just remove these for those guys ...

  implicit def toSuiteOps[A[_ <: Nat], N <: Nat](suite: Suite[A, N]) : SuiteOps[A, N] = 
    new SuiteOps[A, N](suite)

  implicit def toSuccSuiteOps[A[_ <: Nat], P <: Nat](suite: Suite[A, S[P]]) : SuiteSuccOps[A, P] = 
    new SuiteSuccOps[A, P](suite)

}

trait ToSuiteOps extends LowPriorityOps0 

