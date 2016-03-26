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
      case EEmpty => "empty"
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
      case ERec(_) => ""
      case EProj(_, _) => ""
      case ECat => "Cat"
      case EOb(e) => "Obj " ++ prettyPrint(e)
      case EHom(e, c) => "Hom " ++ prettyPrint(e) ++ " {frame}"
      case ECell(e, c) => "Cell " ++ prettyPrint(e) ++ " {frame}"
      case EComp(e, fp, nch) => "comp"
      case EFill(e, fp, nch) => "fill"
      // case ELeftExt(e) => "isLeftExt " ++ prettyPrint(e)
      // case ERightExt(e, a) => "isRightExt " ++ prettyPrint(e)
      // case EBal(e, fp, nch) => "isBalanced"
      // case ELeftBal(_, _, e, f) => "leftBalanced " ++ prettyPrint(e) ++ " " ++ prettyPrint(f)
      // case ERightBal(_, _, e, a, f) => "rightBalanced " ++ prettyPrint(e) ++ " " ++ prettyPrint(f)
      // case EFillerLeftExt(_, _, _) => "fillerLeftExt"
      // case EFillerCompLeftExt(_, _, _) => "fillerCompLeftExt"
      // case ELift(_, _, _, _) => "lift"
      // case ELiftFiller(_, _, _, _) => "liftFiller"
      // case ELiftFillerLeftExt(_, _, _, _) => "liftFillerLeftExt"
      // case EFillerLeftIsRight(_, _, _, _) => "fillerLeftIsRight"
    }

}
