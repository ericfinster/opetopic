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

trait LteCaseSplit {

  type Out[M <: Nat, N <: Nat, D <: Nat]

  def caseZero[N <: Nat](n : N) : Out[Z.type, N, N]
  def caseSucc[M <: Nat, N <: Nat, D <: Nat](plte : Lte[M, N, D]) : Out[S[M], S[N], D]

  def apply[M <: Nat, N <: Nat, D <: Nat](lte : Lte[M, N, D]) : Out[M, N, D] = 
    lte match {
      case ZeroLte(n) => caseZero(n)
      case SuccLte(plte) => caseSucc(plte)
    }

}

trait LteImplicits {

  implicit def zeroLte[N <: Nat](implicit n : N) : Lte[Z.type, N, N] = 
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

  def lteRefl[N <: Nat](n : N) : Lte[N, N, Z.type] = 
    (new NatCaseSplit0 {

      type Out[M <: Nat] = Lte[M, M, Z.type]

      def caseZero : Lte[Z.type, Z.type, Z.type] = 
        ZeroLte(Z)

      def caseSucc[P <: Nat](p : P) : Lte[S[P], S[P], Z.type] = 
        SuccLte(lteRefl(p))

    })(n)

  def ltePred[M <: Nat, N <: Nat, D <: Nat](lte : Lte[S[M], N, D]) : Lte[M, N, S[D]] = 
    lte match {
      case SuccLte(plte) => lteSucc(plte)
    }

  def lteInvert[M <: Nat, N <: Nat, D <: Nat](lte : Lte[M, N, D]) : Lte[D, N, M] = 
    (new LteCaseSplit {

      type Out[M <: Nat, N <: Nat, D <: Nat] = Lte[D, N, M]

      def caseZero[N <: Nat](n : N) : Lte[N, N, Z.type] = 
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

  def diffOpt[K <: Nat, N <: Nat](k: K, n: N) : Option[Diff[K, N]] = 
    (new NatCaseSplit0 {

      type Out[K <: Nat] = Option[Diff[K, N]]

      def caseZero : Out[Z.type] = 
        Some(new Diff[Z.type, N] {

          type D = N
          val lte : Lte[Z.type, N, N] = 
            ZeroLte(n)

        })

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = 
        (new NatCaseSplit0 {

          type Out[N <: Nat] = Option[Diff[S[P], N]]

          def caseZero : Out[Z.type] = None

          def caseSucc[Q <: Nat](q: Q) : Out[S[Q]] = {

            import scalaz.syntax.monad._

            for {
              diff <- diffOpt(p, q)
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
    case class ZeroLtePat(name: String) extends LtePattern
    case class BindLtePat(name: String, mTpe: String, nTpe: String, dTpe: String) extends LtePattern
    case class SuccLtePat(pat: LtePattern) extends LtePattern

    implicit class LtePatternOps(pat: LtePattern) {

      def depth: Int = 
        pat match {
          case ZeroLtePat(name) => 0
          case BindLtePat(_, _, _, _) => -1
          case SuccLtePat(p) => p.depth + 1
        }

      def boundName : String = 
        pat match {
          case ZeroLtePat(name) => name
          case BindLtePat(name, _, _, _) => name
          case SuccLtePat(p) => p.boundName
        }

      def toPatternWith(name: String) : Tree = 
        pat match {
          case ZeroLtePat(_) => pq"ZeroLte(${Bind(TermName(name), Ident(termNames.WILDCARD))})"
          case BindLtePat(_, _, _, _) => Bind(TermName(name), Ident(termNames.WILDCARD))
          case SuccLtePat(p) => pq"SuccLte(${p.toPatternWith(name)})"
        }

      def coercionDecl(matchName: String) : Tree = 
        pat match {
          case ZeroLtePat(name) => 
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
          case ZeroLtePat(_) => 
            (tq"Z.type", tq"${TermName(name)}.N", tq"${TermName(name)}.N")
          case BindLtePat(_, _, _, _) => 
            (tq"S[${TermName(name)}.lower.N]", tq"S[${TermName(name)}.upper.N]", tq"${TermName(name)}.diff.N")
          case SuccLtePat(p) => p.rawTypeTriple(name)
        }

      def typeCoercions(matchName: String) : List[Tree] = 
        pat match {
          case ZeroLtePat(_) => Nil
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

                    // if (debug) println("M: " ++ mPat.toString)
                    // if (debug) println("N: " ++ nPat.toString)
                    // if (debug) println("D: " ++ dPat.toString)

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
                    ZeroLtePat(name)
                  }
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

              (parseLtePattern(ltePat), argPats, expr)
            }
          }

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

              // Now generate the coercions
              val resultParamTerm = TermName(c.freshName("res"))
              val resultParamType = rewriteNat(c)(typeBindings, rtype)

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
