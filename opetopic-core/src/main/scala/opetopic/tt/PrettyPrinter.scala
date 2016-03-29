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
      case EHom(e, c) => "Hom " ++ prettyPrint(e) ++ " {...}"
      case ECell(e, c) => "Cell " ++ prettyPrint(e) ++ " {...}"

      case EIsLeftExt(e) => "isLeftExt " ++ prettyPrint(e)
      case EIsRightExt(e, a) => "isRightExt " ++ prettyPrint(e)

      case EComp(e, fp, nch) => "comp " ++ prettyPrint(e) ++ " {...}"
      case EFill(e, fp, nch) => "fill " ++ prettyPrint(e) ++ " {...}"
      case ELiftLeft(e, ev, c, t) => "liftLeft " ++ prettyPrint(e) ++ " " ++ prettyPrint(ev) ++ " " ++ prettyPrint(c) ++ " " ++ prettyPrint(t)
      case EFillLeft(e, ev, c, t) => "fillLeft " ++ prettyPrint(e) ++ " " ++ prettyPrint(ev) ++ " " ++ prettyPrint(c) ++ " " ++ prettyPrint(t)
      case ELiftRight(e, ev, c, t) => "liftRight " ++ prettyPrint(e) ++ " " ++ prettyPrint(ev) ++ " " ++ prettyPrint(c) ++ " " ++ prettyPrint(t)
      case EFillRight(e, ev, c, t) => "fillRight " ++ prettyPrint(e) ++ " " ++ prettyPrint(ev) ++ " " ++ prettyPrint(c) ++ " " ++ prettyPrint(t)

      case EFillIsLeft(e, fp, ch) => "fillIsLeft " ++ prettyPrint(e) ++ " {...}"
      case EShellIsLeft(e, ev, src, tgt) => "shellIsLeft"
      case EFillLeftIsLeft(e, ev, c, t) => "fillLeftIsLeft"
      case EFillRightIsLeft(e, ev, c, t) => "fillRightIsLeft"
      case EFillLeftIsRight(e, ev, c, t, l, f, fev) => "fillLeftIsRight"
      case EFillRightIsRight(e, ev, c, t, l, f, fev) => "fillRightIsRight"

      case tr : TreeExpr => "tree"

    }

}
