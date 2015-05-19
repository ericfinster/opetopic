/**
  * NatLemmas.scala - Some Lemmas about Naturals Using Dependent Matcing Macros
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scalaz.Leibniz
import scalaz.Leibniz._

object NatLemmaExamples {

  type =::=[N <: Nat, M <: Nat] = Leibniz[Nothing, Nat, N, M]

  def apS[N <: Nat, M <: Nat](ev : N =::= M) : S[N] =::= S[M] =
    lift[Nothing, Nothing, Nat, Nat, S, N, M](ev)

  @natElim
  def matchNatPair[N <: Nat, M <: Nat](n : N, m : M) : Option[N =::= M] = {
    case (Z, Z) => Some(refl)
    case (S(n), Z) => None
    case (Z, S(m)) => None
    case (S(n), S(m)) => matchNatPair(n, m) map (apS(_)) 
  }

  @natElim
  def plusSuccLemma[M <: Nat, N <: Nat](m : M) : M#Plus[S[N]] =::= S[M#Plus[N]] = {
    case Z => refl
    case S(p) => apS(plusSuccLemma(p))
  }

  @natElim
  def plusUnitRight[N <: Nat](n: N) : N =::= N#Plus[Z.type] = {
    case Z => refl
    case S(p) => apS(plusUnitRight(p))
  }

}
