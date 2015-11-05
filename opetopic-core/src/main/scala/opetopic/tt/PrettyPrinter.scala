/**
  * PrettyPrinter.scala - PrettyPrinter for OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

object PrettyPrinter {

  def prettyPrint(p: Patt) : String = 
    p match {
      case Punit => "_"
      case PVar(id) => id
      case PPair(p, q) => prettyPrint(p) ++ ", " ++ prettyPrint(q)
    }

  def prettyPrint(d: Decl) : String = 
    d match {
      case Def(p, e, f) => "let " ++ prettyPrint(p) ++ " : " ++ prettyPrint(e) ++ " = " ++ prettyPrint(f)
      case Drec(p, e, f) => "letrec " ++ prettyPrint(p) ++ " : " ++ prettyPrint(e) ++ " = " ++ prettyPrint(f)
    }

  def prettyPrint(e: Expr) : String =
    e match {
      case EType => "Type"
      case EUnit => "Unit"
      case ETt => "tt"
      case EVar(id) => id
      case ELam(p, e) => "\\ " ++ prettyPrint(p) ++ " . " ++ prettyPrint(e)
      case EPi(p, e, t) => "(" ++ prettyPrint(p) ++ " : " ++ prettyPrint(e) ++ ") -> " ++ prettyPrint(t)
      case ESig(p, e, t) => "Sig " ++ prettyPrint(p) ++ " : " ++ prettyPrint(e) ++ " . " ++ prettyPrint(t)
      case EPair(e, f) => "(" ++ prettyPrint(e) ++ " , " ++ prettyPrint(f) ++ ")"
      case EFst(e) => prettyPrint(e) ++ ".1"
      case ESnd(e) => prettyPrint(e) ++ ".2"
      case EApp(e, f) => prettyPrint(e) ++ " " ++ prettyPrint(f)
      case EDec(d, e) => prettyPrint(d) ++ " ; " ++ prettyPrint(e)
      case ECmplx(c) => "complex"
    }

}
