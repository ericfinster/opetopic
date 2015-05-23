/**
  * TypeLemmas.scala - Using Elimination to produce type-level lemmas
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scalaz.Leibniz
import scalaz.Leibniz._

object TypeLemmas {

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

  def lteSucc[M <: Nat, N <: Nat, D <: Nat](lte : Lte[M, N, D]) : Lte[M, S[N], S[D]] = 
    lte match {
      case ZeroLte(n) => ZeroLte(S(n))
      case SuccLte(plte) => SuccLte(lteSucc(plte))
    }

  @natElim
  def lteRefl[N <: Nat](n : N) : Lte[N, N, Z.type] = {
    case Z => ZeroLte(Z)
    case S(p) => SuccLte(lteRefl(p))
  }

  def ltePred[M <: Nat, N <: Nat, D <: Nat](lte : Lte[S[M], N, D]) : Lte[M, N, S[D]] = 
    lte match {
      case SuccLte(plte) => lteSucc(plte)
    }

  def lteInvert[M <: Nat, N <: Nat, D <: Nat](lte : Lte[M, N, D]) : Lte[D, N, M] = 
    lte match {
      case ZeroLte(n) => lteRefl(n)
      case SuccLte(lte) => lteSucc(lteInvert(lte))
    }

  @lteElim(true)
  def lteInvertTest[M <: Nat, N <: Nat, D <: Nat](lte : Lte[M, N, D]) : Lte[D, N, M] = {
    case ZeroLte(n) => lteRefl(n)
    case SuccLte(lte : (A, B, C)) => lteSucc(lteInvert(lte))
  }

  // @lteElim(true)
  // def lteTest[M <: Nat, N <: Nat, D <: Nat](lte: Lte[S[M], N, D])(snl: List[N], ml: List[M], dl: List[D])  : List[(M, N, D)] = {
  //   case (SuccLte(ZeroLte(n)), snl, Nil, dl) => Nil
  //   case (SuccLte(ZeroLte(n)), snl, ml :: mls, dl) => Nil
  //   case (SuccLte(SuccLte(plte)), snl, ml, dl) => Nil
  // }

}
