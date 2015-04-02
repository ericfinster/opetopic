/**
  * IndexedComplex.scala - Complexes Which Take Values in an Indexed Type
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

trait IndexedComplexTypes {

  type IndexedComplex[N <: Nat, A[_ <: Nat]] = 
    TypeSeq[({ type L[N <: Nat] = Nesting[N, A[N]] })#L, S[N]]

  //============================================================================================
  // MAP
  //

  import TypeSeq._

  def map[A[_ <: Nat], B[_ <: Nat], D <: Nat](cmplx : IndexedComplex[D, A])(f : A ~~> B) : IndexedComplex[D, B] = {

    type ANesting[N <: Nat] = Nesting[N, A[N]]
    type BNesting[N <: Nat] = Nesting[N, B[N]]

    cmplx.map(f : ANesting ~~> BNesting)

  }

  implicit def complexToTypeSeqOps[A[_ <: Nat], D <: Nat](cmplx : IndexedComplex[D, A]) :
      TypeSeqOps[({ type L[N <: Nat] = Nesting[N, A[N]] })#L, S[D]] = 
    new TypeSeqOps[({ type L[N <: Nat] = Nesting[N, A[N]] })#L, S[D]](cmplx)

  implicit def listToNesting[A[_ <: Nat], B[_ <: Nat]](f : A ~~> B) : 
      ~~>[({ type L[N <: Nat] = Nesting[N, A[N]] })#L, ({ type L[N <: Nat] = Nesting[N, B[N]] })#L] = ???


  trait IndexedFunctor[F[_ <: Nat, A[_ <: Nat]]]

}
