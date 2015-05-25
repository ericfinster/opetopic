/**
  * Lte.scala - Less than or Equal Relation and corresponding utilities
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.annotation.StaticAnnotation

sealed trait Lte[M <: Nat, N <: Nat, D <: Nat] { 
  
  val upper : N
  val lower : M
  val diff : D

}

case class ZeroLte[N <: Nat](n : N) extends Lte[Z.type, N, N] {

  val upper : N = n
  val lower : Z.type = Z
  val diff : N = n

}

case class SuccLte[M <: Nat, N <: Nat, D <: Nat](plte : Lte[M, N, D]) extends Lte[S[M], S[N], D] {

  val upper : S[N] = S(plte.upper)
  val lower : S[M] = S(plte.lower)
  val diff : D = plte.diff

}

trait LteImplicits {

  implicit def zeroLte[N <: Nat](implicit n : N) : Lte[Z.type, N, N] = 
    ZeroLte(n)

  implicit def succLte[M <: Nat, N <: Nat, D <: Nat](implicit plte : Lte[M, N, D]) : Lte[S[M], S[N], D] = 
    SuccLte(plte)

}

object Lte extends LteImplicits 

//============================================================================================
// LTE ELIMINATION MACRO
//

class lteElim extends StaticAnnotation {
  def macroTransform(annottees: Any*) : Any =
    macro lteElim.impl
}

object lteElim {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {

    import elimCommon._
    import c.universe._

    val debug: Boolean = 
      c.prefix match {
        case Expr(q"new lteElim(true)") => true
        case _ => false
      }

    sealed trait LtePattern
    case class ZeroLtePat(name: String, tpe: String) extends LtePattern
    case class BindLtePat(name: String, mTpe: String, nTpe: String, dTpe: String) extends LtePattern
    case class SuccLtePat(pat: LtePattern) extends LtePattern

    implicit class LtePatternOps(pat: LtePattern) {

      def depth: Int = 
        pat match {
          case ZeroLtePat(_, _) => 0
          case BindLtePat(_, _, _, _) => -1
          case SuccLtePat(p) => p.depth + 1
        }

      def boundName : String = 
        pat match {
          case ZeroLtePat(name, _) => name
          case BindLtePat(name, _, _, _) => name
          case SuccLtePat(p) => p.boundName
        }

      def toPatternWith(name: String) : Tree = 
        pat match {
          case ZeroLtePat(_, _) => pq"ZeroLte(${Bind(TermName(name), Ident(termNames.WILDCARD))})"
          case BindLtePat(_, _, _, _) => Bind(TermName(name), Ident(termNames.WILDCARD))
          case SuccLtePat(p) => pq"SuccLte(${p.toPatternWith(name)})"
        }

      def coercionDecl(matchName: String) : Tree = 
        pat match {
          case ZeroLtePat(name, _) => 
            q"val ${TermName(name)} = ${TermName(matchName)}.asInstanceOf[${TermName(matchName)}.N]"
          case BindLtePat(name, _, _, _) => {
            val lowerTpe = tq"${TermName(matchName)}.lower.N"
            val upperTpe = tq"${TermName(matchName)}.upper.N"
            val diffTpe = tq"${TermName(matchName)}.diff.N"
            q"val ${TermName(name)} = ${TermName(matchName)}.asInstanceOf[Lte[$lowerTpe, $upperTpe, $diffTpe]]"
          }
          case SuccLtePat(p) => p.coercionDecl(matchName)
        }

      def rawTypeTriple(name: String) : (Tree, Tree, Tree) = 
        pat match {
          case ZeroLtePat(_, _) => 
            (tq"Z.type", tq"${TermName(name)}.N", tq"${TermName(name)}.N")
          case BindLtePat(_, _, _, _) => 
            (tq"S[${TermName(name)}.lower.N]", tq"S[${TermName(name)}.upper.N]", tq"${TermName(name)}.diff.N")
          case SuccLtePat(p) => p.rawTypeTriple(name)
        }

      def typeCoercions(matchName: String) : List[Tree] = 
        pat match {
          case ZeroLtePat(_, tpe) => {
            if (tpe != "") {
              List(q"type ${TypeName(tpe)} = ${TermName(matchName)}.N")
            } else Nil
          }
          case BindLtePat(_, mTpe, nTpe, dTpe) => {
            if (mTpe != "") {
              List(
                q"type ${TypeName(mTpe)} = ${TermName(matchName)}.lower.N",
                q"type ${TypeName(nTpe)} = ${TermName(matchName)}.upper.N",
                q"type ${TypeName(dTpe)} = ${TermName(matchName)}.diff.N"
              )
            } else Nil
          }
          case SuccLtePat(p) => p.typeCoercions(matchName)
        }

    }

    val result = 
      annottees.map(_.tree).toList match {
        case q"$dmods def $mname[..$tparams](...$args)(implicit ..$implargs) : $rtype = { case ..$cases } " :: Nil => {

          val (lname, mPat, nPat, dPat) : (TermName, NatPattern, NatPattern, NatPattern) = 
            args.headOption match {
              case None => abortElim(c)("No matching argument provided")
              case Some(q"$mods val $nm: $tpe = $expr" :: Nil) => {
                tpe match {
                  case tq"Lte[$m, $n, $d]" => {

                    val mPat = patternFromType(c)(m)
                    val nPat = patternFromType(c)(n)
                    val dPat = patternFromType(c)(d)

                    if (debug) println("M: " ++ mPat.toString)
                    if (debug) println("N: " ++ nPat.toString)
                    if (debug) println("D: " ++ dPat.toString)

                    (nm, mPat, nPat, dPat)
                  }
                  case _ => abortElim(c)("Incompatible matching type")
                }
              }
              case _ => abortElim(c)("Unknow argument error")
            }

          val auxArgs = args.tail.flatten
          val auxArgCount = auxArgs.length

          val natUnfoldDepth = mPat.depth - nPat.depth

          val matchVar = c.freshName("u")
          val matchTpe = c.freshName("U")
          val matchPattern = 
            successorTerm(c)(natUnfoldDepth, 
              Bind(TermName(matchVar), Ident(termNames.WILDCARD))
            )

          val localLte = c.freshName("lte")
          val localLteUpperTpe = 
            rewriteNat(c)(getBinding(nPat).map({ 
              case (_, tpe) => Map((tpe -> successorType(c)(natUnfoldDepth, Ident(TypeName(matchTpe)))))
            }).getOrElse(Map.empty), patternToType(c)(nPat))

          val localLteTpe = tq"Lte[${patternToType(c)(mPat)}, ${localLteUpperTpe}, ${patternToType(c)(dPat)}]"

          val umTpe = TypeName(c.freshName("M"))
          val unTpe = TypeName(c.freshName("N"))
          val udTpe = TypeName(c.freshName("D"))

          val unfoldedTpeParams = 
            List(
              TypeDef(Modifiers(), umTpe, List(), TypeBoundsTree(EmptyTree, Ident(TypeName("Nat")))),
              TypeDef(Modifiers(), unTpe, List(), TypeBoundsTree(EmptyTree, Ident(TypeName("Nat")))),
              TypeDef(Modifiers(), udTpe, List(), TypeBoundsTree(EmptyTree, Ident(TypeName("Nat"))))            
            )

          val unfoldedParamType = 
            rewriteNat(c)(Map(
              (getBaseType(mPat) -> Ident(umTpe)), 
              (matchTpe -> Ident(unTpe)), 
              (getBaseType(dPat) -> Ident(udTpe))
            ), localLteTpe)

          if (debug) println("Begining case analysis ...")

          val caseAnalysis : List[(LtePattern, List[Tree], Tree)] = cases map {
            case cq"(..$pats) => $expr" => {

              val ltePat = pats.head
              val argPats = pats.tail

              if (argPats.length != auxArgCount) abortElim(c)("Incorrect match shape")

              def parseLtePattern(pat: Tree) : LtePattern = 
                pat match {
                  case pq"SuccLte($l)" => {
                    SuccLtePat(parseLtePattern(l))
                  }
                  case Apply(Ident(TermName("ZeroLte")), List(Bind(TermName(name), Ident(termNames.WILDCARD)))) => {
                    ZeroLtePat(name, "")
                  }
                  case Apply(Ident(TermName("ZeroLte")), List(Bind(TermName(name), Typed(Ident(termNames.WILDCARD), Ident(TypeName(tpe)))))) =>
                    ZeroLtePat(name, tpe)

                  case Bind(TermName(name), Ident(termNames.WILDCARD)) => {
                    BindLtePat(name, "", "", "")
                  }
                  case pq"$name @ (_: $ttree)" => {
                    ttree match {
                      case tq"scala.Tuple3[$mTpe, $nTpe, $dTpe]" => {
                        (name, mTpe, nTpe, dTpe) match {
                          case (TermName(nm), Ident(TypeName(mnm)), Ident(TypeName(nnm)), Ident(TypeName(dnm))) => {
                            BindLtePat(nm, mnm, nnm, dnm)
                          }
                        }
                      }
                    }
                  }
                }

              if (debug) println("Parsed pattern: " ++ parseLtePattern(ltePat).toString)

              (parseLtePattern(ltePat), argPats, expr)
            }
          }

          if (debug) println("Building cases ...")

          val casePatterns : List[CaseDef] = caseAnalysis.groupBy(_._1).map({
            case (thisPat, rawCases) => {

              val bindingName = c.freshName(thisPat.boundName)
              val (mTpe, nTpe, dTpe) = thisPat.rawTypeTriple(bindingName)

              val typeBindings : Map[String, Tree] = 
                Map(
                  (getBaseType(mPat) -> mTpe),
                  (getBaseType(nPat) -> successorType(c)(natUnfoldDepth, nTpe)),
                  (getBaseType(dPat) -> dTpe)
                )

              val matches : List[Tree] = rawCases map {
                case (_, pts, expr) => cq"(..$pts) => $expr"
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

              if (debug) println("About to rewrite result type ...")

              // Now generate the coercions
              val resultParamTerm = TermName(c.freshName("res"))
              val resultParamType = rewriteNat(c)(typeBindings, rtype)

              if (debug) println("Finished result type")

              cq"""
                ${thisPat.toPatternWith(bindingName)} => {
                  ${thisPat.coercionDecl(bindingName)}
                  ..${thisPat.typeCoercions(bindingName)}
                  ..${argStrings map (_._2)}          

                  def coe($resultParamTerm: $resultParamType) : $rtype = 
                    $resultParamTerm.asInstanceOf[$rtype]

                  coe((..${argStrings map { case (nm, _) => q"$nm" }}) match {
                    case ..${matches}
                  })
                }
              """
            }
          }).toList

          q"""
             $dmods def $mname[..$tparams](...$args)(implicit ..$implargs) : $rtype = 
               $lname.upper match {
                 case $matchPattern => {
                   type ${TypeName(matchTpe)} = ${TermName(matchVar)}.N

                   def unfolded[..$unfoldedTpeParams](uparam: $unfoldedParamType) : $rtype = 
                     uparam match {
                       case ..$casePatterns
                     }

                   unfolded($lname.asInstanceOf[$localLteTpe])
                 }
               }
           """
        }
      }

    if (debug) {
      println("Result of expansion:\n")
      println(showCode(result))
      println("\n")
    }

    c.Expr[Any](result)

  }

}
