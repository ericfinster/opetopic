/**
  * Nat.scala - Type level natural numbers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import scalaz.Leibniz
import scalaz.Leibniz._

sealed trait Nat {

  type TypeRec[Type, R <: NatTypeRec[Type]] <: Type
  type ConsRec[Type, C <: NatConsRec[Type], +A] <: Type

  type Plus[K <: Nat] <: Nat

}

case object Z extends Nat {

  type TypeRec[Type, R <: NatTypeRec[Type]] = R#OnZero
  type ConsRec[Type, C <: NatConsRec[Type], +A] = C#OnZero[A]

  type Plus[K <: Nat] = K

}

case class S[P <: Nat](val pred : P) extends Nat {

  type TypeRec[Type, R <: NatTypeRec[Type]] = 
    R#OnSucc[P, P#TypeRec[Type, R]]

  type ConsRec[Type, C <: NatConsRec[Type], +A] = 
    C#OnSucc[P, ({ type L[+X] = P#ConsRec[Type, C, X] })#L, A]

  type Plus[K <: Nat] = S[P#Plus[K]]

}

trait NatTypeRec[Type] {

  type OnZero <: Type
  type OnSucc[P <: Nat, T <: Type] <: Type

}

trait NatConsRec[Type] {

  type OnZero[+A] <: Type
  type OnSucc[P <: Nat, T[+_] <: Type, +A] <: Type

}

trait NatCaseSplit {

  type Out[N <: Nat]

  def caseZero : Out[Z.type]
  def caseSucc[P <: Nat](p: P) : Out[S[P]]

  def apply[N <: Nat](n : N) : Out[N] = 
    Nat.caseSplit(n)(this)

}

trait NatCaseSplitWithOne extends NatCaseSplit { sp => 

  type Out[N <: Nat]

  def caseZero : Out[Z.type]
  def caseOne : Out[S[Z.type]]
  def caseDblSucc[P <: Nat](p : P) : Out[S[S[P]]]

  final def caseSucc[P <: Nat](p : P) : Out[S[P]] =
    (new NatCaseSplit {

      type Out[N <: Nat] = sp.Out[S[N]]

      def caseZero : Out[Z.type] = 
        sp.caseOne

      def caseSucc[PP <: Nat](pp : PP) : Out[S[PP]] = 
        sp.caseDblSucc(pp)

    })(p)

}

trait NatFunctions {

  def caseSplit[N <: Nat](n: N)(sp: NatCaseSplit) : sp.Out[N] = 
    n match {
      case Z => sp.caseZero.asInstanceOf[sp.Out[N]]
      case S(p) => sp.caseSucc(p).asInstanceOf[sp.Out[N]]
    }

}

trait NatLemmas { this : NatConstants => 

  type =::=[N <: Nat, M <: Nat] = Leibniz[Nothing, Nat, N, M]

  def plusSuccLemma[M <: Nat, N <: Nat](m : M) : M#Plus[S[N]] =::= S[M#Plus[N]] = 
    (new NatCaseSplit {

      type Out[X <: Nat] = X#Plus[S[N]] =::= S[X#Plus[N]]

      def caseZero : _0#Plus[S[N]] =::= S[_0#Plus[N]] = refl

      def caseSucc[P <: Nat](p : P) : S[P]#Plus[S[N]] =::= S[S[P]#Plus[N]] = {
        lift[Nothing, Nothing, Nat, Nat, S, P#Plus[S[N]], S[P]#Plus[N]](
          plusSuccLemma(p)
        )
      }

    })(m)

  def plusUnitRight[N <: Nat](n : N) : N =::= N#Plus[_0] = 
    (new NatCaseSplit {

      type Out[M <: Nat] = M =::= M#Plus[_0]

      def caseZero : _0 =::= _0#Plus[_0] = refl 

      def caseSucc[P <: Nat](p : P) : S[P] =::= S[P]#Plus[_0] = 
        lift[Nothing, Nothing, Nat, Nat, S, P, P#Plus[_0]](
          plusUnitRight(p)
        )

    })(n)

}

trait NatImplicits { self : NatFunctions =>

  implicit class NatOps[N <: Nat](n : N) {

    def caseSplit(sp: NatCaseSplit) : sp.Out[N] = 
      self.caseSplit(n)(sp)

  }

}

trait NatConstants {

  type _0 = Z.type
  type _1 = S[_0]
  type _2 = S[_1]
  type _3 = S[_2]
  type _4 = S[_3]
  type _5 = S[_4]
  type _6 = S[_5]
  type _7 = S[_6]
  type _8 = S[_7]
  type _9 = S[_8]

  val __0 = Z
  val __1 = S(__0)
  val __2 = S(__1)
  val __3 = S(__2)
  val __4 = S(__3)
  val __5 = S(__4)
  val __6 = S(__5)
  val __7 = S(__6)
  val __8 = S(__7)
  val __9 = S(__8)

  def natToInt[N <: Nat](n : N) : Int = 
    n match {
      case Z => 0
      case S(p) => natToInt(p) + 1
    }
}

object Nat extends NatFunctions 
    with NatImplicits 
    with NatConstants
    with NatLemmas
