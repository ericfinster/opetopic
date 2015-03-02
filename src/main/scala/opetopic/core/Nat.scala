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

  // type Plus[M <: Nat[M]] <: Nat[Plus[M]]

}

case class Z() extends Nat[Z] {

  def caseSplit(sp: NatCaseSplit) : sp.Out[Z] = 
    sp.caseZero

  type TypeRec[Type, R <: NatTypeRec[Type]] = R#OnZero
  type ConsRec[Type, C <: NatConsRec[Type], +A] = C#OnZero[A]

  type Plus[M <: Nat[M]] = M

}

case class S[P <: Nat[P]](p : P) extends Nat[S[P]] {

  def caseSplit(sp: NatCaseSplit) : sp.Out[S[P]] = 
    sp.caseSucc(p)

  type TypeRec[Type, R <: NatTypeRec[Type]] = 
    R#OnSucc[P, P#TypeRec[Type, R]]

  type ConsRec[Type, C <: NatConsRec[Type], +A] = 
    C#OnSucc[P, ({ type L[+X] = P#ConsRec[Type, C, X] })#L, A]

  // type Plus[M <: Nat[M]] = S[M#Plus[P]]

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

trait NatNatRec {

  type OnZero <: Nat[OnZero]
  type OnSucc[P <: Nat[P]] <: Nat[OnSucc[P]]

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

// trait NatUtils { self : Nats =>

//   def toSelf[N <: Nat](n : N) : N =::= n.Self = 
//     Leibniz.force[Nothing, Nat, N, n.Self]

//   def natFromInt(i : Int) : Nat = 
//     if (i <= 0) Z else S(natFromInt(i - 1))

//   def natToInt[N <: Nat](n : N) : Int = 
//     n match {
//       case Z => 0
//       case S(p) => natToInt(p) + 1
//     }

//   implicit def zeroNat : Z.type = Z
//   implicit def succNat[P <: Nat](implicit p : P) : S[P] = S(p)

//   trait IsZero[N <: Nat] {
//     def leibniz : Leibniz[Nothing, Nat, _0, N]
//   }

//   trait IsSucc[N <: Nat] {
//     type P <: Nat
//     def leibniz : Leibniz[Nothing, Nat, S[P], N]
//   }

//   implicit def zeroIsZero : IsZero[_0] =
//     new IsZero[_0] {
//       def leibniz : Leibniz[Nothing, Nat, _0, _0] = refl[_0]
//     }

//   implicit def succIsSucc[N <: Nat] : IsSucc[S[N]] = 
//     new IsSucc[S[N]] {
//       type P = N
//       def leibniz : Leibniz[Nothing, Nat, S[P], S[P]] = refl[S[P]]
//     }

// }

object Nats extends Nats with NatSums with NatFunctions

object Blorp {

  import Nats._

  trait DirectionRec extends NatTypeRec[AnyRef] {

    type OnZero = Nothing
    type OnSucc[P <: Nat[P], T <: AnyRef] = List[T]

  }

  type Dir[N <: Nat[N]] = N#TypeRec[AnyRef, DirectionRec]

  type Dir0 = Dir[_0] // _0#TypeRec[AnyRef, DirectionRec]
  type Dir1 = Dir[_1] // _1#TypeRec[AnyRef, DirectionRec]
  type Dir2 = Dir[_2] // _2#TypeRec[AnyRef, DirectionRec]
  type Dir3 = Dir[_3] // _3#TypeRec[AnyRef, DirectionRec]

  type DDir0 = Nothing
  type DDir1 = List[Nothing]
  type DDir2 = List[List[Nothing]]
  type DDir3 = List[List[List[Nothing]]]

  implicitly[Dir0 =:= DDir0]
  implicitly[Dir1 =:= DDir1]
  implicitly[Dir2 =:= DDir2]
  implicitly[Dir3 =:= DDir3]

  //============================================================================================
  // CARDINALS
  //

  trait CardinalTreeRec extends NatConsRec[AnyRef] {

    type OnZero[+A] = Tree[Z, A]

    type OnSucc[P <: Nat[P], T[+_] <: AnyRef, +A] =
      T[Tree[S[P], A]]

  }

  type CardinalTree[N <: Nat[N], +A] = N#ConsRec[AnyRef, CardinalTreeRec, A]

  type CTree0[+A] = Tree[Z, A]
  type CTree1[+A] = Tree[Z, Tree[S[Z], A]]
  type CTree2[+A] = Tree[Z, Tree[S[Z], Tree[S[S[Z]], A]]]
  type CTree3[+A] = Tree[Z, Tree[S[Z], Tree[S[S[Z]], Tree[S[S[S[Z]]], A]]]]

  type CCTree0[+A] = CardinalTree[Z, A]
  type CCTree1[+A] = CardinalTree[S[Z], A]
  // type CCTree2[+A] = CardinalTree[S[S[Z]], A]
  // type CCTree3[+A] = CardinalTree[S[S[S[Z]]], A]

  type ECTree0[+A] = _0#ConsRec[AnyRef, CardinalTreeRec, A]
  type ECTree1[+A] = _1#ConsRec[AnyRef, CardinalTreeRec, A]
  // type ECTree2[+A] = _2#ConsRec[AnyRef, CardinalTreeRec, A]
  // type ECTree3[+A] = _3#ConsRec[AnyRef, CardinalTreeRec, A]

  implicitly[CTree0[Int] =:= CardinalTree[_0, Int]]
  implicitly[CTree1[Int] =:= CardinalTree[_1, Int]]
  implicitly[CTree2[Int] =:= CardinalTree[_2, Int]]
  implicitly[CTree3[Int] =:= CardinalTree[_3, Int]]

}
