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

  type N <: Nat

  type TypeRec[Type, R <: NatTypeRec[Type]] <: Type
  type ConsRec[Type, C <: NatConsRec[Type], +A] <: Type

  type Plus[K <: Nat] <: Nat

}

case object Z extends Nat {

  type N = Z.type

  type TypeRec[Type, R <: NatTypeRec[Type]] = R#OnZero
  type ConsRec[Type, C <: NatConsRec[Type], +A] = C#OnZero[A]

  type Plus[K <: Nat] = K

}

case class S[P <: Nat](val pred : P) extends Nat {

  type N = S[P]

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

object Nats extends NatImplicits {

  // import upickle.Js

  // implicit val natWriter = upickle.Writer[Nat]{
  //   case n => Js.Num(TypeDefs.natToInt(n))
  // }

  // implicit val natReader = upickle.Reader[Nat]{
  //   case Js.Num(d) => TypeDefs.intToNat(d.toInt)
  // }

}

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

              def parsePattern(pat: Tree) : NatPattern = {
                pat match {
                  case pq"Z" => ZeroPattern
                  case pq"S($p)" => SuccPattern(parsePattern(p))
                  case Bind(TermName(name), Ident(termNames.WILDCARD)) =>
                    BindPattern(name, "Nat")
                  case Bind(TermName(name), Typed(Ident(termNames.WILDCARD), Ident(TypeName(tpe)))) =>
                    BindPattern(name, tpe)
                }
              }

              (natPats map parsePattern, (auxPats, expr))
            }
          }

          def expandMatch(
            matchVars: List[(TermName, String)], 
            typeBindings: Map[String, Tree], 
            typeDefns: List[(String, String)],
            patnDefns: List[(String, String)],
            matchData: List[(List[NatPattern], (List[Tree], Tree))]
          ) : Tree =
            matchVars match {
              case Nil => {

                val matches : List[Tree] = matchData map {
                  case (_, (pts, expr)) => cq"(..$pts) => $expr"
                }

                val argStrings : List[(TermName, Tree)] = auxArgs map {
                  case q"$mods val $mname: $tpe = $expr" => {
                    val thisTermType = rewriteNat(c)(typeBindings, tpe)
                    val thisTermName = TermName(c.freshName(mname.toString))
                    val thisTermDecl = 
                      q"val $thisTermName = $mname.asInstanceOf[$thisTermType]"
                    (thisTermName, thisTermDecl)

                  }
                }

                val typeDecls = 
                  typeDefns map {
                    case (tpeName, localName) => q"type ${TypeName(tpeName)} = ${TermName(localName)}.N"
                  }

                val termDecls = 
                  patnDefns map {
                    case (name, localName) => {
                      val localTerm = TermName(localName)  
                      q"implicit val ${TermName(name)} = $localTerm.asInstanceOf[$localTerm.N]"
                    }
                  }

                // Now generate the return coercions
                val resultParamTerm = TermName(c.freshName("res"))
                val resultParamType = rewriteNat(c)(typeBindings, rtype)

                q"""
                  ..${typeDecls}
                  ..${termDecls}
                  ..${argStrings map (_._2)}

                  def coe($resultParamTerm: $resultParamType) : $rtype = 
                    $resultParamTerm.asInstanceOf[$rtype]

                  coe((..${argStrings map { case (nm, _) => q"$nm" }}) match {
                    case ..${matches}
                  })
                 """
              }
              case (TermName(arg), tpe) :: vs => {

                val resultCases =
                  matchData.groupBy(_._1.head) map {
                    case (thisPat, remPats) => {

                      val (newTypeDefns, newPatnDefns, matchName) = 
                        (getBinding(thisPat) map { 
                          case (name, "Nat") => (typeDefns, patnDefns, name)
                          case (name, tpe) => {
                            val localName = c.freshName(name)
                            ((tpe, localName) :: typeDefns, (name, localName) :: patnDefns, localName)
                          }
                        }).getOrElse(typeDefns, patnDefns, "")

                      val thisCase =
                        expandMatch(vs, 
                          typeBindings + (tpe -> patternToType(c)(thisPat)), 
                          newTypeDefns, newPatnDefns,
                          remPats map ({ case (l, r) => (l.tail, r) })
                        )

                      cq"${expandPattern(c)(thisPat, matchName)} => $thisCase"
                    }
                  }

                q"""
                  ${TermName(arg)} match {
                    case ..$resultCases
                  }
                 """
              }
            }
              
          q"""
             $dmods def $mname[..$tparams](...$args)(implicit ..$implargs) : $rtype = 
               ${expandMatch(varPairs, Map.empty, Nil, Nil, caseAnalysis)}
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
