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

  def rewriteNatIn[F[_ <: Nat], N <: Nat, M <: Nat](ev : N =::= M) : F[N] === F[M] = 
    lift[Nothing, Nothing, Nat, Any, F, N, M](ev)

  def natSymm[N <: Nat, M <: Nat](ev: N =::= M) : M =::= N = 
    symm[Nothing, Nat, N, M](ev)

  def natTrans[M <: Nat, N <: Nat, O <: Nat](e: M =::= N, f: N =::= O) : M =::= O =
    e.andThen(f)

  def ap[F[_], A, B](ev: A === B) : F[A] === F[B] = 
    lift[Nothing, Nothing, Any, Any, F, A, B](ev)

  def apS[N <: Nat, M <: Nat](ev : N =::= M) : S[N] =::= S[M] =
    lift[Nothing, Nothing, Nat, Nat, S, N, M](ev)

  def inverseOf[A, B](ev: A === B) : B === A = 
    symm[Nothing, Any, A, B](ev)

  @natElim
  def matchNatPair[N <: Nat, M <: Nat](n : N, m : M) : Option[N =::= M] = {
    case (Z, Z) => Some(refl)
    case (S(n), Z) => None
    case (Z, S(m)) => None
    case (S(n), S(m)) => matchNatPair(n, m) map (apS(_)) 
  }

  @natElim
  def asPred[N <: Nat](n: N) : Option[N =::= S[Nat]] = {
    case Z => None
    case S(p) => Some(refl)
  }

  @natElim
  def addNat[N <: Nat, M <: Nat](n: N, m: M) : N#Plus[M] = {
    case (Z, m) => m
    case (S(n), m) => S(addNat(n, m))
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

  @natElim
  def plusComm[N <: Nat, M <: Nat](n: N, m: M) : N#Plus[M] =::= M#Plus[N] = {
    case (Z, m) => plusUnitRight(m)
    case (S(p: P), m) => natTrans(apS(plusComm(p, m)), natSymm(plusSuccLemma(p)))
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

  @lteElim
  def lteSumLemma[M <: Nat, N <: Nat, D <: Nat](lte: Lte[M, N, D]) : D#Plus[M] =::= N = {
    case ZeroLte(n) => natSymm(plusUnitRight(n))
    case SuccLte(plte : (M0, N0, D0)) =>
      plusSuccLemma[D0, M0](plte.diff).andThen(apS(lteSumLemma(plte)))
  }

  @natElim
  def ltePlusLemma[N <: Nat, M <: Nat](n: N)(m: M) : Lte[N, N#Plus[M], M] = {
    case (Z, m) => ZeroLte(m)
    case (S(p), m) => SuccLte(ltePlusLemma(p)(m))
  }

  trait Diff[K <: Nat, N <: Nat] {

    type D <: Nat
    val lte : Lte[K, N, D]

  }

  object Diff {

    def apply[K0 <: Nat, N0 <: Nat, D0 <: Nat](l: Lte[K0, N0, D0]) : Diff[K0, N0] =
      new Diff[K0, N0] {
        type D = D0
        val lte = l
      }

    implicit def zeroDiff[N <: Nat](implicit n: N) : Diff[_0, N] = 
      new Diff[_0, N] {
        type D = N
        val lte = ZeroLte(n)
      }

    implicit def succDiff[K <: Nat, N <: Nat](implicit d: Diff[K, N]) : Diff[K, S[N]] = 
      new Diff[K, S[N]] {
        type D = S[d.D]
        val lte = lteSucc(d.lte)
      }

    implicit def reflDiff[N <: Nat](implicit n: N) : Diff[N, N] = 
      new Diff[N, N] {
        type D = _0
        val lte = lteRefl(n)
      }

  }

  @natElim
  def diffOpt[K <: Nat, N <: Nat](k: K, n: N) : Option[Diff[K, N]] = {
    case (Z, n) => Some(Diff(ZeroLte(n)))
    case (S(p), Z) => None
    case (S(p), S(q)) => 
      for {
        diff <- diffOpt(p, q)
      } yield Diff(SuccLte(diff.lte))
  }

}
