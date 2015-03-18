/**
  * Nat.scala - Type Level Natural Numbers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

sealed trait Nat[N <: Nat[N]] {

  def caseSplit(sp : NatCaseSplit) : sp.Out[N]

  type TypeRec[Type, R <: NatTypeRec[Type]] <: Type
  type ConsRec[Type, C <: NatConsRec[Type], +A] <: Type

  type Plus[K <: Nat[K]] <: Nat[Plus[K]]

  type Test <: Nat[N#Test]

}

case class Z() extends Nat[Z] {

  def caseSplit(sp: NatCaseSplit) : sp.Out[Z] = 
    sp.caseZero

  type TypeRec[Type, R <: NatTypeRec[Type]] = R#OnZero
  type ConsRec[Type, C <: NatConsRec[Type], +A] = C#OnZero[A]

}

case class S[P <: Nat[P]](p : P) extends Nat[S[P]] {

  def caseSplit(sp: NatCaseSplit) : sp.Out[S[P]] = 
    sp.caseSucc(p)

  type TypeRec[Type, R <: NatTypeRec[Type]] = 
    R#OnSucc[P, P#TypeRec[Type, R]]

  type ConsRec[Type, C <: NatConsRec[Type], +A] = 
    C#OnSucc[P, ({ type L[+X] = P#ConsRec[Type, C, X] })#L, A]

}

//============================================================================================
// VALUE RECURSORS
//

trait NatCaseSplit {

  type Out[N <: Nat[N]]

  def caseZero : Out[Z]
  def caseSucc[P <: Nat[P]](p : P) : Out[S[P]]

  def apply[N <: Nat[N]](n : N) : Out[N] = 
    n.caseSplit(this)

}

trait NatElim {

  type Out[N <: Nat[N]]

  def caseZero : Out[Z]
  def caseSucc[P <: Nat[P]](p : P)(fp : Out[P]) : Out[S[P]]

  def apply[N <: Nat[N]](n : N) : Out[N] = 
    Nats.eliminate(n)(this)

}

//============================================================================================
// TYPE RECURSORS
//

trait NatTypeRec[Type] {

  type OnZero <: Type
  type OnSucc[P <: Nat[P], T <: Type] <: Type

}

trait NatConsRec[Type] {

  type OnZero[+A] <: Type
  type OnSucc[P <: Nat[P], T[+_] <: Type, +A] <: Type

}

trait Nats {

  type _0 = Z
  type _1 = S[_0]
  type _2 = S[_1]
  type _3 = S[_2]
  type _4 = S[_3]
  type _5 = S[_4]
  type _6 = S[_5]
  type _7 = S[_6]
  type _8 = S[_7]
  type _9 = S[_8]

  val __0 : _0 = Z()
  val __1 : _1 = S(__0)
  val __2 : _2 = S(__1)
  val __3 : _3 = S(__2)
  val __4 : _4 = S(__3)
  val __5 : _5 = S(__4)
  val __6 : _6 = S(__5)
  val __7 : _7 = S(__6)
  val __8 : _8 = S(__7)
  val __9 : _9 = S(__8)

}

trait NatFunctions {

  def eliminate[N <: Nat[N]](n : N)(elim : NatElim) : elim.Out[N] = 
    (new NatCaseSplit {

      type Out[N <: Nat[N]] = elim.Out[N]

      def caseZero : Out[Z] = 
        elim.caseZero

      def caseSucc[P <: Nat[P]](p : P) : Out[S[P]] =
        elim.caseSucc(p)(eliminate(p)(elim))

    })(n)

}

trait NatSums { self : Nats => 

  trait Sum[N <: Nat[N], M <: Nat[M]] {
    type Out <: Nat[Out]
  }

  object Sum {

    type Aux[N <: Nat[N], M <: Nat[M], K <: Nat[K]] = Sum[N, M] { type Out = K }

    implicit def zeroSum[N <: Nat[N]] : Aux[Z, N, N] = 
      new Sum[Z, N] { type Out = N }

    implicit def succSum[P <: Nat[P], N <: Nat[N]](implicit psum : Sum[P, N]) : Aux[S[P], N, S[psum.Out]] = 
      new Sum[S[P], N] { type Out = S[psum.Out] }

  }

//   def plusSuccLemma[M <: Nat, N <: Nat](m : M) : M#Plus[S[N]] =::= S[M#Plus[N]] = 
//     (new NatElim {

//       type Out[X <: Nat] = X#Plus[S[N]] =::= S[X#Plus[N]]

//       def caseZero : _0#Plus[S[N]] =::= S[_0#Plus[N]] = refl

//       def caseSucc[P <: Nat](p : P, ih : Out[P]) : S[P]#Plus[S[N]] =::= S[S[P]#Plus[N]] = {
//         lift[Nothing, Nothing, Nat, Nat, S, P#Plus[S[N]], S[P]#Plus[N]](ih)
//       }

//     })(m)

//   def plusUnitRight[N <: Nat](n : N) : N =::= N#Plus[_0] = 
//     (new NatElim {

//       type Out[M <: Nat] = M =::= M#Plus[_0]

//       def caseZero : _0 =::= _0#Plus[_0] = refl 

//       def caseSucc[P <: Nat](p : P, ih : Out[P]) : S[P] =::= S[P]#Plus[_0] = 
//         lift[Nothing, Nothing, Nat, Nat, S, P, P#Plus[_0]](ih)

//     })(n)

}

object Nats extends Nats with NatSums with NatFunctions

