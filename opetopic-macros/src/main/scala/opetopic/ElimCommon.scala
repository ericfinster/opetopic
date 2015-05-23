/**
  * ElimCommon.scala - Common Macro Routines for Elimination
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.reflect.macros.whitebox.Context

object elimCommon {

  sealed trait NatPattern { def depth : Int } 
  case object ZeroPattern extends NatPattern { def depth = 0 }
  case class BindPattern(name: String, tpe: String) extends NatPattern { def depth = 0 }
  case class SuccPattern(pat: NatPattern) extends NatPattern { def depth = pat.depth + 1 }

  def getBinding(pat: NatPattern) : Option[(String, String)] = 
    pat match {
      case ZeroPattern => None
      case SuccPattern(p) => getBinding(p)
      case BindPattern(name, tpe) => Some(name, tpe)
    }

  def getBaseType(pat: NatPattern) : String = 
    pat match {
      case ZeroPattern => "Z.type"
      case SuccPattern(p) => getBaseType(p)
      case BindPattern(_, tpe) => tpe
    }

  def successorType(c: Context)(i: Int, tr: c.Tree) : c.Tree = {
    import c.universe._
    if (i <= 0) tr else tq"S[${successorType(c)(i-1, tr)}]"
  }

  def successorTerm(c: Context)(i: Int, tr: c.Tree): c.Tree = {
    import c.universe._
    if (i <= 0) tr else q"S(${successorTerm(c)(i-1, tr)})"
  }

  def expandPattern(c: Context)(pat: NatPattern, name: String) : c.Tree = {
    import c.universe._

    pat match {
      case ZeroPattern => pq"Z"
      case BindPattern(_, _) => Bind(TermName(name), Ident(termNames.WILDCARD))
      case SuccPattern(pt) => pq"S(${expandPattern(c)(pt, name)})"
    }
  }

  def patternToTree(c: Context)(pat: NatPattern) : c.Tree = {
    import c.universe._

    pat match {
      case ZeroPattern => pq"Z"
      case BindPattern(name, _) => Bind(TermName(name), Ident(termNames.WILDCARD))
      case SuccPattern(pt) => pq"S(${patternToTree(c)(pt)})"
    }
  }

  def patternToType(c: Context)(pat: NatPattern) : c.Tree = {
    import c.universe._

    pat match {
      case ZeroPattern => tq"Z.type"
      case BindPattern(_, tpe) => Ident(TypeName(tpe))
      case SuccPattern(pt) => tq"S[${patternToType(c)(pt)}]"
    }
  }

  def patternFromType(c: Context)(tree: c.Tree) : NatPattern = {
    import c.universe._

    tree match {
      case tq"Z.type" => ZeroPattern
      case tq"_0" => ZeroPattern
      case Ident(TypeName(name)) => BindPattern("", name)
      case tq"S[$tpe]" => SuccPattern(patternFromType(c)(tpe))
    }
  }

  def abortElim(c: Context)(str: String) =
    c.abort(c.enclosingPosition, str)

  def rewriteNat(c: Context)(bindings: Map[String, c.Tree], tree: c.Tree) : c.Tree = {
    import c.universe._

    // This seems to work okay, but I'm not sure if you should be using the "Transformer"
    // class instead for some reason.  Note that, if you want to, you could instantiate such
    // a class here, move the unfold method inside, and call that method to generate your
    // result.

    val result = tree match {
      case Ident(TypeName(id)) => {
        if (bindings.isDefinedAt(id)) {
          bindings(id)
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

  def unfold(c: Context)(bindings: Map[String, c.Tree], tree: c.Tree) : c.Tree = {
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
