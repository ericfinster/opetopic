/**
  * Lte.scala - Less than or Equal Relation and corresponding utilities
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds

import TypeDefs._

sealed trait Lte[M <: Nat, N <: Nat, D <: Nat] { 
  
  def upper : N
  def lower : M
  def diff : D

}

case class ZeroLte[N <: Nat](n : N) extends Lte[_0, N, N] {

  def upper : N = n
  def lower : _0 = Z
  def diff : N = n

}

case class SuccLte[M <: Nat, N <: Nat, D <: Nat](plte : Lte[M, N, D]) extends Lte[S[M], S[N], D] {

  def upper : S[N] = S(plte.upper)
  def lower : S[M] = S(plte.lower)
  def diff : D = plte.diff

}

trait LteCaseSplit {

  type Out[M <: Nat, N <: Nat, D <: Nat]

  def caseZero[N <: Nat](n : N) : Out[_0, N, N]
  def caseSucc[M <: Nat, N <: Nat, D <: Nat](plte : Lte[M, N, D]) : Out[S[M], S[N], D]

  def apply[M <: Nat, N <: Nat, D <: Nat](lte : Lte[M, N, D]) : Out[M, N, D] = 
    lte match {
      case ZeroLte(n) => caseZero(n)
      case SuccLte(plte) => caseSucc(plte)
    }

}

trait LteImplicits {

  implicit def zeroLte[N <: Nat](implicit n : N) : Lte[_0, N, N] = 
    ZeroLte(n)

  implicit def succLte[M <: Nat, N <: Nat, D <: Nat](implicit plte : Lte[M, N, D]) : Lte[S[M], S[N], D] = 
    SuccLte(plte)

}

object Lte extends LteImplicits {

  def lteSucc[M <: Nat, N <: Nat, D <: Nat](implicit lte : Lte[M, N, D]) : Lte[M, S[N], S[D]] = 
    lte match {
      case ZeroLte(n) => ZeroLte(S(n))
      case SuccLte(plte) => SuccLte(lteSucc(plte))
    }

  def lteRefl[N <: Nat](n : N) : Lte[N, N, _0] = 
    (new NatCaseSplit0 {

      type Out[M <: Nat] = Lte[M, M, _0]

      def caseZero : Lte[_0, _0, _0] = 
        ZeroLte(Z)

      def caseSucc[P <: Nat](p : P) : Lte[S[P], S[P], _0] = 
        SuccLte(lteRefl(p))

    })(n)

  def ltePred[M <: Nat, N <: Nat, D <: Nat](lte : Lte[S[M], N, D]) : Lte[M, N, S[D]] = 
    lte match {
      case SuccLte(plte) => lteSucc(plte)
    }

  def lteInvert[M <: Nat, N <: Nat, D <: Nat](lte : Lte[M, N, D]) : Lte[D, N, M] = 
    (new LteCaseSplit {

      type Out[M <: Nat, N <: Nat, D <: Nat] = Lte[D, N, M]

      def caseZero[N <: Nat](n : N) : Lte[N, N, _0] = 
        lteRefl(n)

      def caseSucc[M <: Nat, N <: Nat, D <: Nat](plte : Lte[M, N, D]) : Lte[D, S[N], S[M]] = 
        lteSucc(lteInvert(plte))

    })(lte)

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

  }

  def getLte[M[+_], K <: Nat, N <: Nat](k: K, n: N)(implicit sm: ShapeMonad[M]) : M[Diff[K, N]] = 
    (new NatCaseSplit0 {

      type Out[K <: Nat] = M[Diff[K, N]]

      def caseZero : Out[_0] = 
        sm.pure(new Diff[_0, N] {

          type D = N
          val lte : Lte[_0, N, N] = 
            ZeroLte(n)

        })

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = 
        (new NatCaseSplit0 {

          type Out[N <: Nat] = M[Diff[S[P], N]]

          def caseZero : Out[_0] = 
            sm.failWith(new ShapeLteError)

          def caseSucc[Q <: Nat](q: Q) : Out[S[Q]] = {

            import scalaz.syntax.monad._

            for {
              diff <- getLte(p, q)
            } yield {
              new Diff[S[P], S[Q]] {

                type D = diff.D
                val lte : Lte[S[P], S[Q], diff.D] = 
                  SuccLte(diff.lte)

              }
            }
          }

        })(n)

    })(k)

}

