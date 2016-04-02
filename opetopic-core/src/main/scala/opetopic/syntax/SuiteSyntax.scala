/**
  * SuiteSyntax.scala - Syntax for Suites
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.syntax

import opetopic._
import TypeLemmas._

import scalaz.Applicative

class SuiteOps[A[_ <: Nat], N <: Nat](suite: Suite[A, N]) {

  def fold[B](fld: IndexedFold[A, B]) : B = 
    Suite.fold[A, B, N](suite)(fld)

  def map[B[_ <: Nat]](mp: IndexedMap[A, B]) : Suite[B, N] = 
    Suite.map[A, B, N](suite)(mp)

  def zipWith[B[_ <: Nat]](bs: Suite[B, N]) : Suite[Lambda[`K <: Nat` => (A[K], B[K])], N] = 
    Suite.zip[A, B, N](suite, bs)

  def truncate[K <: Nat](k: K)(implicit ev: Diff[K, N]) : Suite[A, K] = 
    Suite.drop[A, ev.D, N, K](lteInvert(ev.lte))(suite)

  def foreach(op: IndexedOp[A]) : Unit = 
    Suite.foreach[A, N](suite)(op)

  def traverse[T[_], B[_ <: Nat]](t: IndexedTraverse[T, A, B])(implicit apT: Applicative[T]) : T[Suite[B, N]] = 
    Suite.traverse[T, A, B, N](suite)(t)

}

final class SuiteSuccOps[A[_ <: Nat], P <: Nat](suite: Suite[A, S[P]]) extends SuiteOps[A, S[P]](suite) {

  def head: A[P] = 
    Suite.head(suite)

  def tail: Suite[A, P] = 
    Suite.tail(suite)

  def get[K <: Nat](k: K)(implicit ev: Diff[K, P]) : A[K] = 
    Suite.getAt[A,K,P,ev.D](suite)(ev.lte)

  def getOpt[K <: Nat](k: K) : Option[A[K]] = 
    for { diff <- diffOpt(k, suite.length.pred) } yield { get(k)(diff) }

}

trait LowPriorityOps0 {

  // The idea is that this will keep us from conflicting with the same name for 
  // complexes ... but maybe we should just remove these for those guys ...

  implicit def toSuiteOps[A[_ <: Nat], N <: Nat](suite: Suite[A, N]) : SuiteOps[A, N] = 
    new SuiteOps[A, N](suite)

  implicit def toSuccSuiteOps[A[_ <: Nat], P <: Nat](suite: Suite[A, S[P]]) : SuiteSuccOps[A, P] = 
    new SuiteSuccOps[A, P](suite)

  implicit def suiteToFiniteSuite[A[_ <: Nat], D <: Nat](suite: Suite[A, D]) : FiniteSuite[A] = 
    Sigma[Lambda[`N <: Nat` => Suite[A, N]], D](suite.length)(suite)

  implicit def finiteSuiteToOps[A[_ <: Nat]](fs: FiniteSuite[A]) : SuiteOps[A, fs.N] = 
    new SuiteOps[A, fs.N](fs.value)

  implicit def suiteToNonemptySuite[A[_ <: Nat], P <: Nat](suite: Suite[A, S[P]]) : NonemptySuite[A] = 
    SuccSigma[Lambda[`N <: Nat` => Suite[A, N]], P](suite.length.pred)(suite)

  implicit def nonemptySuiteToSuccOps[A[_ <: Nat], P <: Nat](nes: NonemptySuite[A]) : SuiteSuccOps[A, nes.P] = 
    new SuiteSuccOps[A, nes.P](nes.value)

}

trait ToSuiteOps extends LowPriorityOps0 


