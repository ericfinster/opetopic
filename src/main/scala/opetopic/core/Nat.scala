/**
  * Nat.scala - Type Level Natural Numbers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import scalaz.Leibniz
import scalaz.Leibniz._

//============================================================================================
// TYPE LEVEL NATURALS
//

sealed trait Nat { self => 

  type Self <: Nat

  val self : Self = this.asInstanceOf[Self]

  type Rec0[Type, R <: NatTypeRec0[Type]] <: Type
  type Rec1[Type, C <: NatTypeRec1[Type], +A] <: Type

  type Plus[N <: Nat] <: Nat

}

case object Z extends Nat {

  type Self = Z.type

  type Rec0[Type, R <: NatTypeRec0[Type]] = R#OnZero
  type Rec1[Type, C <: NatTypeRec1[Type], +A] = C#OnZero[A]

  type Plus[N <: Nat] = N

}


case class S[P <: Nat](val pred : P) extends Nat { self =>

  type Self = S[P]
  type Pred = P

  type Rec0[Type, R <: NatTypeRec0[Type]] = 
    R#OnSucc[P, P#Rec0[Type, R]]

  type Rec1[Type, C <: NatTypeRec1[Type], +A] = 
    C#OnSucc[P, ({ type L[+X] = P#Rec1[Type, C, X] })#L, A]

  type Plus[N <: Nat] = S[P#Plus[N]]

}

trait NatRec[F[_ <: Nat]] {

  def caseZero : F[Z.type]
  def caseSucc[P <: Nat](fp : F[P]) : F[S[P]]

}


trait Nats {

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

  val __0 : _0 = Z
  val __1 : _1 = S(__0)
  val __2 : _2 = S(__1)
  val __3 : _3 = S(__2)
  val __4 : _4 = S(__3)
  val __5 : _5 = S(__4)

  type =::=[K <: Nat, L <: Nat] = Leibniz[Nothing, Nat, K, L]

}

//============================================================================================
// TYPE RECURSORS
//

trait NatTypeRec0[Type] {

  type OnZero <: Type
  type OnSucc[P <: Nat, T <: Type] <: Type

}

trait NatTypeRec1[Type] {

  type OnZero[+A] <: Type
  type OnSucc[P <: Nat, T[+_] <: Type, +A] <: Type

}

//============================================================================================
// MATCHING ON NATURALS
//

trait NatMatches { self : Nats => 

  trait ZeroMatch[N <: Nat] {

    implicit def zeroCoh : Leibniz[Nothing, Nat, N, _0]
    implicit def zeroCoe : Leibniz[Nothing, Nat, _0, N]

  }

  trait OneMatch[N <: Nat] {

    implicit def oneCoh : Leibniz[Nothing, Nat, N, _1]
    implicit def oneCoe : Leibniz[Nothing, Nat, _1, N]

  }

  trait SuccMatch[N <: Nat] {

    type P <: Nat

    implicit val p : P

    implicit def succCoh : Leibniz[Nothing, Nat, N, S[P]]
    implicit def succCoe : Leibniz[Nothing, Nat, S[P], N]

  }

  trait DblSuccMatch[N <: Nat] {

    type PP <: Nat
    type P = S[PP]

    implicit val pp : PP
    implicit val p = S(pp)

    implicit def dblSuccCoh : Leibniz[Nothing, Nat, N, S[S[PP]]]
    implicit def dblSuccCoe : Leibniz[Nothing, Nat, S[S[PP]], N]

  }

  object IsZero {

    def unapply[N <: Nat](n : N) : Option[ZeroMatch[N]] =
      n match {
        case Z => Some(
          new ZeroMatch[N] { 
            implicit def zeroCoh : Leibniz[Nothing, Nat, N, _0] = force[Nothing, Nat, N, _0]
            implicit def zeroCoe : Leibniz[Nothing, Nat, _0, N] = force[Nothing, Nat, _0, N]
          }
        )
        case _ => None
      }

  }

  object IsOne {

    def unapply[N <: Nat](n : N) : Option[OneMatch[N]] = 
      n match {
        case S(Z) => Some(
          new OneMatch[N] {
            implicit def oneCoh : Leibniz[Nothing, Nat, N, _1] = force[Nothing, Nat, N, _1]
            implicit def oneCoe : Leibniz[Nothing, Nat, _1, N] = force[Nothing, Nat, _1, N]
          }
        )
        case _ => None
      }

  }

  object IsSucc {

    def unapply[N <: Nat](n : N) : Option[SuccMatch[N]] = 
      n match {
        case S(pr) => Some(
          new SuccMatch[N] {

            type P = pr.Self
            val p = pr.self

            def succCoh : Leibniz[Nothing, Nat, N, S[P]] = force[Nothing, Nat, N, S[P]]
            def succCoe : Leibniz[Nothing, Nat, S[P], N] = force[Nothing, Nat, S[P], N]
            
          }
        )
        case _ => None
      }

  }

  object IsDblSucc {

    def unapply[N <: Nat](n : N) : Option[DblSuccMatch[N]] = 
      n match {
        case S(S(ppr)) => Some(
          new DblSuccMatch[N] {

            type PP = ppr.Self
            val pp = ppr.self

            implicit def dblSuccCoh : Leibniz[Nothing, Nat, N, S[S[PP]]] = force[Nothing, Nat, N, S[S[PP]]]
            implicit def dblSuccCoe : Leibniz[Nothing, Nat, S[S[PP]], N] = force[Nothing, Nat, S[S[PP]], N]

          }
        )
        case _ => None
      }

  }

}

//============================================================================================
// VALUE RECURSORS
//

trait NatRecursors { self : Nats with NatMatches =>

  //============================================================================================
  // REAL NAT CASE SPLIT
  //

  trait RealNatCaseSplit {

    type Out[N <: Nat]

    def caseZero : Out[_0]
    def caseSucc[P <: Nat](p : P) : Out[S[P]]

    def apply[N <: Nat](n : N) : Out[N] = 
      n match {
        case Z => caseZero.asInstanceOf[Out[N]]
        case S(p) => caseSucc(p).asInstanceOf[Out[N]]
      }

  }

  //============================================================================================
  // ELIMINATORS
  //

  trait NatElim { thisRec =>

    type Out[N <: Nat]

    def caseZero : Out[_0]
    def caseSucc[P <: Nat](p : P, ih : Out[P]) : Out[S[P]]

    def apply[N <: Nat](n : N) : Out[N] =
      n match {
        case Z => caseZero.asInstanceOf[Out[N]]
        case S(p) => caseSucc(p, thisRec(p)).asInstanceOf[Out[N]]
      }

  }

  trait NatElim1 { thisRec =>

    type Out[N <: Nat]

    def caseZero : Out[_0]
    def caseOne : Out[_1]
    def caseDblSucc[P <: Nat](p : P, ih : Out[S[P]]) : Out[S[S[P]]]

    def apply[N <: Nat](n : N) : Out[N] = 
      n match {
        case Z => caseZero.asInstanceOf[Out[N]]
        case S(Z) => caseOne.asInstanceOf[Out[N]]
        case S(S(p)) => caseDblSucc(p, thisRec(S(p))).asInstanceOf[Out[N]]
      }

  }

  //============================================================================================
  // CASE SPLITTING
  //

  trait NatDepMatch[F[_ <: Nat], N <: Nat] extends Dep[F[N]]

  case class ZeroDepMatch[F[_ <: Nat]]() extends NatDepMatch[F, _0]
  case class OneDepMatch[F[_ <: Nat]]() extends NatDepMatch[F, _1]
  case class TwoDepMatch[F[_ <: Nat]]() extends NatDepMatch[F, _2]
  case class SuccDepMatch[F[_ <: Nat], P <: Nat]() extends NatDepMatch[F, S[P]]
  case class DblSuccMatch[F[_ <: Nat], P <: Nat]() extends NatDepMatch[F, S[S[P]]]
  case class TrplSuccMatch[F[_ <: Nat], P <: Nat]() extends NatDepMatch[F, S[S[S[P]]]]

  trait NatCaseSplit {

    type In[N <: Nat]
    type Out[N <: Nat]

    def caseZero(in : In[_0]) : Out[_0]
    def caseSucc[P <: Nat](in : In[S[P]]) : Out[S[P]]

    def matchFromNat[N <: Nat](n : N) : NatDepMatch[In, N] =
      (new NatElim { 

        type Out[M <: Nat] = NatDepMatch[In, M]

        def caseZero : NatDepMatch[In, _0] = ZeroDepMatch[In]
        def caseSucc[P <: Nat](p : P, ih : NatDepMatch[In, P]) : NatDepMatch[In, S[P]] = 
          SuccDepMatch[In, P]()

      })(n)

    def apply[N <: Nat](n : N, in : In[N]) : Out[N] =
      Pack(matchFromNat(n), in) match {
        case Pack(ZeroDepMatch(), inZ) => caseZero(inZ)
        case Pack(SuccDepMatch(), inS) => caseSucc(inS)
      }

  }

  trait NatCaseSplitTwo {

    type In0[N <: Nat]
    type In1[N <: Nat]
    type Out[N <: Nat]

    type ParamPair[N <: Nat] = (In0[N], In1[N])

    def caseZero(in0 : In0[_0], in1 : In1[_0]) : Out[_0]
    def caseSucc[P <: Nat](in0 : In0[S[P]], in1 : In1[S[P]]) : Out[S[P]]

    def matchFromNat[N <: Nat](n : N) : NatDepMatch[ParamPair, N] =
      (new NatElim {

        type Out[M <: Nat] = NatDepMatch[ParamPair, M]

        def caseZero : NatDepMatch[ParamPair, _0] = ZeroDepMatch[ParamPair]
        def caseSucc[P <: Nat](p : P, ih : NatDepMatch[ParamPair, P]) : NatDepMatch[ParamPair, S[P]] = 
          SuccDepMatch[ParamPair, P]()

      })(n)

    def apply[N <: Nat](n : N, in0 : In0[N], in1 : In1[N]) : Out[N] = {
      val pp : ParamPair[N] = (in0, in1)
      Pack(matchFromNat(n), pp) match {
        case Pack(ZeroDepMatch(), (in0Z, in1Z)) => caseZero(in0Z, in1Z)
        case Pack(SuccDepMatch(), (in0S, in1S)) => caseSucc(in0S, in1S)
      }
    }

  }

  trait NatCaseSplit1 {

    type In[N <: Nat]
    type Out[N <: Nat]

    def caseZero(in : In[_0]) : Out[_0]
    def caseOne(in : In[_1]) : Out[_1]
    def caseDblSucc[P <: Nat](in : In[S[S[P]]]) : Out[S[S[P]]]

    def matchFromNat[N <: Nat](n : N) : NatDepMatch[In, N] = 
      (new NatElim1 {

        type Out[M <: Nat] = NatDepMatch[In, M]

        def caseZero : NatDepMatch[In, _0] = ZeroDepMatch[In]
        def caseOne : NatDepMatch[In, _1] = OneDepMatch[In]
        def caseDblSucc[P <: Nat](p : P, ih : Out[S[P]]) : NatDepMatch[In, S[S[P]]] = 
          DblSuccMatch[In, P]

      })(n)

    def apply[N <: Nat](n : N, in : In[N]) : Out[N] = 
      Pack(matchFromNat(n), in) match {
        case Pack(ZeroDepMatch(), inZ) => caseZero(inZ)
        case Pack(OneDepMatch(), inO) => caseOne(inO)
        case Pack(DblSuccMatch(), inDS) => caseDblSucc(inDS)
      }

  }

  trait NatCaseSplit1Two {

    type In0[N <: Nat]
    type In1[N <: Nat]
    type Out[N <: Nat]

    def caseZero(in0 : In0[_0], in1 : In1[_0]) : Out[_0]
    def caseOne(in0 : In0[_1], in1 : In1[_1]) : Out[_1]
    def caseDblSucc[P <: Nat](in0 : In0[S[S[P]]], in1 : In1[S[S[P]]]) : Out[S[S[P]]]

    type ParamPair[N <: Nat] = (In0[N], In1[N])

    def matchFromNat[N <: Nat](n : N) : NatDepMatch[ParamPair, N] = 
      (new NatElim1 {

        type Out[M <: Nat] = NatDepMatch[ParamPair, M]

        def caseZero : NatDepMatch[ParamPair, _0] = ZeroDepMatch[ParamPair]
        def caseOne : NatDepMatch[ParamPair, _1] = OneDepMatch[ParamPair]
        def caseDblSucc[P <: Nat](p : P, ih : Out[S[P]]) : NatDepMatch[ParamPair, S[S[P]]] = 
          DblSuccMatch[ParamPair, P]

      })(n)

    def apply[N <: Nat](n : N, in0 : In0[N], in1 : In1[N]) : Out[N] = {
      val pp : ParamPair[N] = (in0, in1)
      Pack(matchFromNat(n), pp) match {
        case Pack(ZeroDepMatch(), (in0Z, in1Z)) => caseZero(in0Z, in1Z)
        case Pack(OneDepMatch(), (in0O, in1O)) => caseOne(in0O, in1O)
        case Pack(DblSuccMatch(), (in0DS, in1DS)) => caseDblSucc(in0DS, in1DS)
      }
    }

  }

}

trait NatSums { self : Nats with NatRecursors =>

  def plusSuccLemma[M <: Nat, N <: Nat](m : M) : M#Plus[S[N]] =::= S[M#Plus[N]] = 
    (new NatElim {

      type Out[X <: Nat] = X#Plus[S[N]] =::= S[X#Plus[N]]

      def caseZero : _0#Plus[S[N]] =::= S[_0#Plus[N]] = refl

      def caseSucc[P <: Nat](p : P, ih : Out[P]) : S[P]#Plus[S[N]] =::= S[S[P]#Plus[N]] = {
        lift[Nothing, Nothing, Nat, Nat, S, P#Plus[S[N]], S[P]#Plus[N]](ih)
      }

    })(m)

  def plusUnitRight[N <: Nat](n : N) : N =::= N#Plus[_0] = 
    (new NatElim {

      type Out[M <: Nat] = M =::= M#Plus[_0]

      def caseZero : _0 =::= _0#Plus[_0] = refl 

      def caseSucc[P <: Nat](p : P, ih : Out[P]) : S[P] =::= S[P]#Plus[_0] = 
        lift[Nothing, Nothing, Nat, Nat, S, P, P#Plus[_0]](ih)

    })(n)

}

trait NatUtils { self : Nats =>

  def toSelf[N <: Nat](n : N) : N =::= n.Self = 
    Leibniz.force[Nothing, Nat, N, n.Self]

  def natFromInt(i : Int) : Nat = 
    if (i <= 0) Z else S(natFromInt(i - 1))

  def natToInt[N <: Nat](n : N) : Int = 
    n match {
      case Z => 0
      case S(p) => natToInt(p) + 1
    }

  implicit def zeroNat : Z.type = Z
  implicit def succNat[P <: Nat](implicit p : P) : S[P] = S(p)

  trait IsZero[N <: Nat] {
    def leibniz : Leibniz[Nothing, Nat, _0, N]
  }

  trait IsSucc[N <: Nat] {
    type P <: Nat
    def leibniz : Leibniz[Nothing, Nat, S[P], N]
  }

  implicit def zeroIsZero : IsZero[_0] =
    new IsZero[_0] {
      def leibniz : Leibniz[Nothing, Nat, _0, _0] = refl[_0]
    }

  implicit def succIsSucc[N <: Nat] : IsSucc[S[N]] = 
    new IsSucc[S[N]] {
      type P = N
      def leibniz : Leibniz[Nothing, Nat, S[P], S[P]] = refl[S[P]]
    }

}

object Nats extends Nats 
    with NatSums
    with NatMatches 
    with NatRecursors 
    with NatUtils

