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
// RECURSIVE TYPE DEFINITION MACRO
//

class recType extends StaticAnnotation {
  def macroTransform(annottees: Any*) : Any =
    macro recType.impl
}

object recType {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val result = 
      annottees.map(_.tree).toList match {
        case q"$mods type $tpname[..$tparams] = $tpe" :: Nil => {
          q"$mods type $tpname[..$tparams] = Unit"
        }
      }

    println("Result of type expansion:\n")
    println(showCode(result))
    println("\n")

    c.Expr[Any](result)

  }

}

//============================================================================================
// COMMON ELIMINATION
//

object elimCommon {

  sealed trait NatPattern
  case object ZeroPattern extends NatPattern
  case class BindPattern(name: String) extends NatPattern
  case class SuccPattern(pat: NatPattern) extends NatPattern

  def patternToTree(c: Context)(pat: NatPattern) : c.Tree = {
    import c.universe._

    pat match {
      case ZeroPattern => pq"Z"
      case BindPattern(name) => Bind(TermName(name), Ident(termNames.WILDCARD))
      case SuccPattern(pt) => pq"S(${patternToTree(c)(pt)})"
    }
  }

  def patternToType(c: Context)(pat: NatPattern) : c.Tree = {
    import c.universe._

    pat match {
      case ZeroPattern => tq"Z.type"
      case BindPattern(name) => tq"Nat"
      case SuccPattern(pt) => tq"S[${patternToType(c)(pt)}]"
    }
  }

  def abortElim(c: Context)(str: String) =
    c.abort(c.enclosingPosition, str)

  def rewriteNat(c: Context)(bindings: Map[String, NatPattern], tree: c.Tree) : c.Tree = {
    import c.universe._

    val result = tree match {
      case Ident(TypeName(id)) => {
        if (bindings.isDefinedAt(id)) {
          patternToType(c)(bindings(id))
        } else Ident(TypeName(id))
      }
      case tq"$tpt.type" => {
        tq"${rewriteNat(c)(bindings, tpt)}.type"
      }
      case tq"$tpt#$tpname" => {
        tq"${rewriteNat(c)(bindings, tpt)}#$tpname"
      }
      case tq"$ref.$tpename" => {
        tq"$ref.$tpename"
      }
      case tq"$tcons[..$targs]" => {

        // if (debug) println("Constructor: " ++ showRaw(tcons))

        val ncons = rewriteNat(c)(bindings, tcons)

        val nargs =
          for { arg <- targs } yield {
            rewriteNat(c)(bindings, arg)
          }

        unfold(c)(bindings, tq"$ncons[..$nargs]")
      }
      case _ => tree
        // super.transform(tree)
    }

    // if (debug) println(tree.toString ++ " -> " ++ result.toString)

    result

  }

  def unfold(c: Context)(bindings: Map[String, NatPattern], tree: c.Tree) : c.Tree = {
    import c.universe._

    tree match {
      case tq"Derivative[$a, S[$p]]" => {
        val newDeriv = unfold(c)(bindings, 
          tq"Derivative[Tree[$a, S[$p]], $p]"
        )
        tq"(Tree[Tree[$a, S[$p]], $p], List[($a, $newDeriv)])"
      }
      case tq"Context[$a, S[$p]]" => {
        val newDeriv = unfold(c)(bindings,
          tq"Derivative[Tree[$a, S[$p]], $p]"
        )
        tq"List[($a, $newDeriv)]"
      }
      case tq"NestingContext[$a, S[$p]]" => {
        val newDeriv = unfold(c)(bindings,
          tq"Derivative[Nesting[$a, S[$p]], S[$p]]"
        )
        tq"List[($a, $newDeriv)]"
      }
      case tq"NestingZipper[$a, S[$p]]" => {
        val newCntxt = unfold(c)(bindings,
          tq"NestingContext[$a, S[$p]]"
        )
        val res = tq"(Nesting[$a, S[$p]], $newCntxt)"
        // println(tr.toString ++ " -> " ++ res.toString)
        res
      }
      case tq"CardinalTree[$a, S[$p]]" => {
        unfold(c)(bindings,
          tq"CardinalTree[Tree[$a, S[$p]], $p]"
        )
      }
      case _ => tree
    }
  }
}

//============================================================================================
// LTE ELIMINATION MACRO
//

// class lteElim extends StaticAnnotation {
//   def macroTransform(annottees: Any*) : Any =
//     macro lteElim.impl
// }

// object lteElim {

//   def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
//     import c.universe._

//   }

// }

//============================================================================================
// NAT ELIMINATION MACRO
//

class natElim extends StaticAnnotation {
  def macroTransform(annottees: Any*) : Any =
    macro natElim.impl
}

object natElim {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {

    import elimCommon._
    import c.universe._

    val debug: Boolean = 
      c.prefix match {
        case Expr(q"new natElim(true)") => true
        case _ => false
      }

    val result = 
      annottees.map(_.tree).toList match {
        case q"$dmods def $mname[..$tparams](...$args)(implicit ..$implargs) : $rtype = { case ..$cases } " :: Nil => {

          // Does not check that the given type is in fact declared to be a Nat
          val varPairs : List[(TermName, String)] = 
            args.headOption match {
              case None => abortElim(c)("No matching agruments provided")
              case Some(hargs) => hargs map {
                case q"$mods val $mname: $tpe = $expr" =>
                  tpe match {
                    case Ident(TypeName(mtype)) => (mname, mtype)
                    case _ =>  abortElim(c)("Matching variable must have simple type")
                  }
              }
            }

          val auxArgs = args.tail.flatten

          val natArgCount = varPairs.length
          val auxArgCount = auxArgs.length

          val caseAnalysis : List[(List[NatPattern], (List[Tree], Tree))] = cases map {
            case cq"(..$pats) => $expr" => {
              val (natPats, auxPats) = pats.splitAt(natArgCount)

              if (natPats.length != natArgCount) abortElim(c)("Incorrect nat pattern count")
              if (auxPats.length != auxArgCount) abortElim(c)("Incorrect parameter pattern count")

              def parsePattern(pat: Tree) : NatPattern = 
                pat match {
                  case pq"Z" => ZeroPattern
                  case pq"S($p)" => SuccPattern(parsePattern(p))
                  case Bind(TermName(name), Ident(termNames.WILDCARD)) =>
                    BindPattern(name)
                }

              (natPats map parsePattern, (auxPats, expr))
            }
          }

          def expandMatch(matchVars: List[(TermName, String)], bindings: Map[String, NatPattern], matchData: List[(List[NatPattern], (List[Tree], Tree))]) : Tree = 
            matchVars match {
              case Nil => {

                val matches : List[Tree] = matchData map {
                  case (_, (pts, expr)) => cq"(..$pts) => $expr"
                }

                val argStrings : List[(TermName, Tree)] = auxArgs map {
                  case q"$mods val $mname: $tpe = $expr" => {
                    //println("Processing argument: " ++ mname.toString)
                    val thisTermType = rewriteNat(c)(bindings, tpe)
                    //println(tpe.toString ++ " -> " ++ resultType.toString)
                    val thisTermName = TermName(c.freshName(mname.toString))
                    val thisTermDecl = 
                      q"val $thisTermName = $mname.asInstanceOf[$thisTermType]"
                    (thisTermName, thisTermDecl)

                  }
                }

                // Now generate the coercions
                val resultParamTerm = TermName(c.freshName("res"))
                val resultParamType = rewriteNat(c)(bindings, rtype)

                q"""
                  ..${argStrings map (_._2)}

                  def coe($resultParamTerm: $resultParamType) : $rtype = 
                    $resultParamTerm.asInstanceOf[$rtype]

                  coe((..${argStrings map { case (nm, _) => q"$nm" }}) match {
                    case ..${matches}
                  })
                 """
              }
              case v :: vs => {

                val patternMap : Map[NatPattern, List[(List[NatPattern], (List[Tree], Tree))]] = 
                  matchData.groupBy(_._1.head)

                val recData : Map[NatPattern, Tree] = 
                  patternMap map { case (thisPat, remPats) => 
                    (thisPat, expandMatch(vs, bindings + (v._2 -> thisPat), remPats map ({ case (l, r) => (l.tail, r) })))
                  }

                val resultCases = recData map {
                  case (np, tr) => cq"${patternToTree(c)(np)} => $tr"
                }

                q"""
                  ${v._1} match {
                    case ..$resultCases
                  }
                 """
              }
            }
              
          q"""
             $dmods def $mname[..$tparams](...$args)(implicit ..$implargs) : $rtype = 
               ${expandMatch(varPairs, Map.empty, caseAnalysis)}
           """
        }
        case _ => abortElim(c)("Failed to parse method definition")
      }

    if (debug) {
      println("Result of expansion:\n")
      println(showCode(result))
      println("\n")
    }

    c.Expr[Any](result)

  }

}
