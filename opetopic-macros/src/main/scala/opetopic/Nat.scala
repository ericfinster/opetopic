/**
  * Nat.scala - Type level natural numbers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.annotation.StaticAnnotation

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

object Nat {

  // import upickle.Js

  // implicit val natWriter = upickle.Writer[Nat]{
  //   case n => Js.Num(TypeDefs.natToInt(n))
  // }

  // implicit val natReader = upickle.Reader[Nat]{
  //   case Js.Num(d) => TypeDefs.intToNat(d.toInt)
  // }

}

trait NatTypeRec[Type] {

  type OnZero <: Type
  type OnSucc[P <: Nat, T <: Type] <: Type

}

trait NatConsRec[Type] {

  type OnZero[+A] <: Type
  type OnSucc[P <: Nat, T[+_] <: Type, +A] <: Type

}

trait ~~>[F[_ <: Nat], G[_ <: Nat]] {

  def apply[N <: Nat](fn : F[N]) : G[N]

}

trait IndexedTraverse[T[_], F[_ <: Nat], G[_ <: Nat]] {
  def apply[N <: Nat](n: N)(fn: F[N]) : T[G[N]]
}

trait NatCaseSplit0 {

  type Out[N <: Nat]

  def caseZero : Out[Z.type]
  def caseSucc[P <: Nat](p: P) : Out[S[P]]

  def apply[N <: Nat](n : N) : Out[N] = 
    n match {
      case Z => caseZero.asInstanceOf[Out[N]]
      case S(p) => caseSucc(p).asInstanceOf[Out[N]]
    }

}

trait NatCaseSplit1 {

  type Out[N <: Nat, A]

  def caseZero[A] : Out[Z.type, A]
  def caseSucc[P <: Nat, A](p : P) : Out[S[P], A]

  def apply[N <: Nat, A](n : N) : Out[N, A] = 
    n match {
      case Z => caseZero[A].asInstanceOf[Out[N, A]]
      case S(p) => caseSucc[Nat, A](p).asInstanceOf[Out[N, A]]
    }
}

trait NatCaseSplit2 {

  type Out[N <: Nat, A, B]

  def caseZero[A, B] : Out[Z.type, A, B]
  def caseSucc[P <: Nat, A, B](p : P) : Out[S[P], A, B]

  def apply[N <: Nat, A, B](n : N) : Out[N, A, B] = 
    n match {
      case Z => caseZero[A, B].asInstanceOf[Out[N, A, B]]
      case S(p) => caseSucc[Nat, A, B](p).asInstanceOf[Out[N, A, B]]
    }

}

trait NatCaseSplitWithOne extends NatCaseSplit0 { sp => 

  def caseOne : Out[S[Z.type]]
  def caseDblSucc[P <: Nat](p : P) : Out[S[S[P]]]

  override final def caseSucc[P <: Nat](p : P) : Out[S[P]] =
    splitSucc(p)

  object splitSucc extends NatCaseSplit0 {

    type Out[N <: Nat] = sp.Out[S[N]]

    def caseZero : Out[Z.type] =
      sp.caseOne

    def caseSucc[PP <: Nat](pp : PP) : Out[S[PP]] =
      sp.caseDblSucc(pp)

  }

}

trait NatCaseSplitWithTwo extends NatCaseSplitWithOne { sp => 

  def caseTwo : Out[S[S[Z.type]]]
  def caseTrplSucc[P <: Nat](p : P) : Out[S[S[S[P]]]]

  override final def caseDblSucc[P <: Nat](p : P) : Out[S[S[P]]] = 
    splitDblSucc(p)

  object splitDblSucc extends NatCaseSplit0 {

    type Out[N <: Nat] = sp.Out[S[S[N]]]

    def caseZero : Out[Z.type] = 
      sp.caseTwo

    def caseSucc[PPP <: Nat](ppp : PPP) : Out[S[PPP]] = 
      sp.caseTrplSucc(ppp)

  }

}

trait NatLemmas {

  // import TypeDefs._

  type =::=[N <: Nat, M <: Nat] = Leibniz[Nothing, Nat, N, M]

  def rewriteNatIn[F[_ <: Nat], N <: Nat, M <: Nat](ev : N =::= M) : F[N] === F[M] = 
    lift[Nothing, Nothing, Nat, Any, F, N, M](ev)

  def matchNatPair[N <: Nat, M <: Nat](n : N, m : M) : Option[N =::= M] = 
    (new NatCaseSplit0 { sp => 

      type Out[N0 <: Nat] = Option[N0 =::= M]

      def caseZero : Out[Z.type] = 
        (new NatCaseSplit0 {

          type Out[M0 <: Nat] = Option[Z.type =::= M0]

          def caseZero : Out[Z.type] = Some(refl)
          def caseSucc[P <: Nat](p : P) : Out[S[P]] = None

        })(m)

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = 
        (new NatCaseSplit0 {

          type Out[M0 <: Nat] = Option[S[P] =::= M0]

          def caseZero : Out[Z.type] = None
          def caseSucc[Q <: Nat](q : Q) : Out[S[Q]] = 
            for {
              ev <- matchNatPair(p, q)
            } yield {
              lift[Nothing, Nothing, Nat, Nat, S, P, Q](ev)
            }

        })(m)

    })(n)

  def plusSuccLemma[M <: Nat, N <: Nat](m : M) : M#Plus[S[N]] =::= S[M#Plus[N]] = 
    (new NatCaseSplit0 {

      type Out[X <: Nat] = X#Plus[S[N]] =::= S[X#Plus[N]]

      def caseZero : Z.type#Plus[S[N]] =::= S[Z.type#Plus[N]] = refl

      def caseSucc[P <: Nat](p : P) : S[P]#Plus[S[N]] =::= S[S[P]#Plus[N]] = {
        lift[Nothing, Nothing, Nat, Nat, S, P#Plus[S[N]], S[P]#Plus[N]](
          plusSuccLemma(p)
        )
      }

    })(m)

  def plusUnitRight[N <: Nat](n : N) : N =::= N#Plus[Z.type] = 
    (new NatCaseSplit0 {

      type Out[M <: Nat] = M =::= M#Plus[Z.type]

      def caseZero : Z.type =::= Z.type#Plus[Z.type] = refl 

      def caseSucc[P <: Nat](p : P) : S[P] =::= S[P]#Plus[Z.type] = 
        lift[Nothing, Nothing, Nat, Nat, S, P, P#Plus[Z.type]](
          plusUnitRight(p)
        )

    })(n)

}

trait NatImplicits { 

  implicit def zeroNat : Z.type = Z
  implicit def succNat[P <: Nat](implicit p : P) : S[P] = S(p)

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

  def intToNat(i : Int) : Nat = 
    if (i <= 0) Z else S(intToNat(i - 1))

}

object Nats extends NatImplicits
    with NatLemmas 

//============================================================================================
// MACROS
//

class printTree extends StaticAnnotation {
  def macroTransform(annottees: Any*) : Any =
    macro printTreeMacro.impl
}

object printTreeMacro {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val inputs = annottees.map(_.tree).toList

    for {
      t <- inputs
    } {
      println(showRaw(t))
    }

    c.Expr[Any](Block(inputs, Literal(Constant(()))))
  }

}

class natElim extends StaticAnnotation {
  def macroTransform(annottees: Any*) : Any =
    macro natElim.impl
}

object natElim {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val inputs = annottees.map(_.tree).toList

    // Presumably the compiler actually has a way to instantiate
    // a type variable, which is essentially what you're doing here
    class TypeSubst(src: String, tgt: Tree) extends Transformer {

      override def transform(tree: Tree): Tree =
        tree match {
          case tq"$tcons[..$targs]" => {

            val nargs = targs map {
              case Ident(TypeName(id)) => {
                if (id == src) tgt else Ident(TypeName(id))
              }
              case tr @ _ => transform(tr)
            }

            tq"$tcons[..$nargs]"
          }
          case _ =>
            super.transform(tree)
        }

    }

    val result = 
      inputs match {
        case q"def $mname[..$tparams](...$args) : $rtype = { case ..$cases } " :: Nil => {

          // First argument must be the nat we are matching on
          val (matchVar, Ident(TypeName(matchTpe))) = 
            args.head match {
              case q"$mods val $name: $tpe = $expr" :: Nil => (name, tpe)
            }

          // Create shadow names and types for zero and succ cases
          val (zeroTrpls, succTrpls) = 
            (args.tail.flatten map {
              case q"$mods val $name: $tpe = $expr" => {

                val zeroTermName = TermName(c.freshName(name.toString))
                val succTermName = TermName(c.freshName(name.toString))

                val zeroType = new TypeSubst(matchTpe, tq"Z.type").transform(tpe)
                val succType = new TypeSubst(matchTpe, tq"S[Nat]").transform(tpe)

                ((name, zeroTermName, zeroType), (name, succTermName, succType))
              }
            }).unzip

          // Generate the associated declarations
          val zeroDecls = zeroTrpls map { case (n, s, t) => q"val $s = $n.asInstanceOf[$t]" }
          val succDecls = succTrpls map { case (n, s, t) => q"val $s = $n.asInstanceOf[$t]" }

          // Generate the types for the return value
          val resultZeroType = new TypeSubst(matchTpe, tq"Z.type").transform(rtype)
          val resultSuccType = new TypeSubst(matchTpe, tq"S[Nat]").transform(rtype)

          // Now generate the coercions
          val resultParamTerm = TermName(c.freshName("res"))

          // Sort the patterns by looking at the head match 
          val (zeroPats, succPats) = 
            (cases map {
              case cq"(..$pats) => $expr" => {

                val natPattern = pats.head
                val remPattern = pats.tail

                natPattern match {
                  case q"Z" => Left((remPattern, expr))
                  case q"S($p)" => Right((remPattern, expr))
                }

              }
            }).partition(_.isLeft)


          val zeroCases = zeroPats map {
            case Left((pat, expr)) => cq"(..$pat) => $expr"
          }

          val succCases = succPats map {
            case Right((pat, expr)) => cq"(..$pat) => $expr"
          }

          val zeroMatch = 
            q"""
               (..${zeroTrpls map { case (_, s, _) => q"$s" }}) match {
                 case ..$zeroCases
               }
             """

          val succMatch = 
            q"""
               (..${succTrpls map { case (_, s, _) => q"$s" }}) match {
                 case ..$succCases
               }
             """

          q"""
             def $mname[..$tparams](...$args) : $rtype = 
               $matchVar match {
                 case Z => {
                   ..$zeroDecls

                   def coe($resultParamTerm: $resultZeroType) : $rtype = 
                     $resultParamTerm.asInstanceOf[$rtype]

                   coe($zeroMatch)
                 }
                 case S(p) => {
                   ..$succDecls

                   def coe($resultParamTerm: $resultSuccType) : $rtype = 
                     $resultParamTerm.asInstanceOf[$rtype]

                   coe($succMatch)
                 }
               }
           """
        }
      }

    // println("Result of expansion:\n")
    // println(showCode(result))
    // println("\n")

    c.Expr[Any](result)

  }

}
