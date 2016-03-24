/**
  * Parser.scala - Parser for OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import opetopic._
import TypeLemmas._

import scala.util.parsing.combinator._
import scala.util.parsing.input._

object OpetopicParser extends RegexParsers with PackratParsers {

  lazy val ident: Parser[String] = 
    """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r

  lazy val expr: PackratParser[Expr] = (
      expr1 ~ "," ~ expr ^^
        { case e ~ "," ~ f => EPair(e, f) }
    | expr1
  )

  lazy val expr1: PackratParser[Expr] = (
      "\\" ~ pattern1 ~ "." ~ expr1 ^^
        { case "\\" ~ p ~ "." ~ e => ELam(p, e) }
    | "(" ~ pattern1 ~ ":" ~ expr1 ~ ") -> " ~ expr1 ^^
        { case "(" ~ p ~ ":" ~ e ~ ") -> " ~ f => EPi(p, e, f) }
    | "(" ~ pattern1 ~ ":" ~ expr1 ~ ") * " ~ expr1 ^^
        { case "(" ~ p ~ ":" ~ e ~ ") * " ~ f => ESig(p, e, f) }
    | decl ~ ";" ~ expr1 ^^ 
        { case d ~ ";" ~ e => EDec(d, e) }
    | expr2 ~ "->" ~ expr1 ^^ 
        { case e ~ "->" ~ f => EPi(Punit, e, f) }
    | expr2 ~ "*" ~ expr1 ^^
        { case e ~ "*" ~ f => ESig(Punit, e, f) }
    | record 
    | "comp" ~ expr3 ~ nicheExpr ^^
        { case "comp" ~ e ~ ne => EComp(e, ne.value._1, ne.value._2) }
    | "fill" ~ expr3 ~ nicheExpr ^^
        { case "fill" ~ e ~ ne => EFill(e, ne.value._1, ne.value._2) }
    | "isLeftExt" ~ expr3 ^^
        { case "isLeftExt" ~ e => EIsLeftExt(e) }
    | "isRightExt" ~ expr3 ~ addrExpr ^^
        { case "isRightExt" ~ e ~ ae => EIsRightExt(e, ae) }
    | "fillIsLeft" ~ expr3 ~ nicheExpr ^^
        { case "fillIsLeft" ~ e ~ ne => EFillIsLeft(e, ne.value._1, ne.value._2) }
    | "liftLeft" ~ expr3 ~ expr3 ~ expr3 ~ expr3 ^^
        { case "liftLeft" ~ e ~ ev ~ c ~ t => ELiftLeft(e, ev, c, t) }
    | "fillLeft" ~ expr3 ~ expr3 ~ expr3 ~ expr3 ^^
        { case "fillLeft" ~ e ~ ev ~ c ~ t => EFillLeft(e, ev, c, t) }
    | "liftRight" ~ expr3 ~ expr3 ~ expr3 ~ expr3 ~ addrExpr ^^
        { case "liftRight" ~ e ~ ev ~ c ~ t ~ a => ELiftRight(e, ev, c, t, a) }
    | "fillRight" ~ expr3 ~ expr3 ~ expr3 ~ expr3 ~ addrExpr ^^
        { case "fillRight" ~ e ~ ev ~ c ~ t ~ a => EFillRight(e, ev, c, t, a) }
    | expr2
  )

  lazy val record: PackratParser[ERec] = 
    "record" ~ "{" ~ repsep(field, "|") ~ "}" ^^
      { case "record" ~ "{" ~ fs ~ "}" => ERec(fs) }

  lazy val field: PackratParser[Field] = 
    ident ~ expr ^^ { case id ~ e => Field(id, e) }

  lazy val expr2: PackratParser[Expr] = (
      "$" ~ ident ~ expr3 ^^
        { case "$" ~ id ~ e => EProj(id, e) }
    | expr2 ~ expr3 ^^
        { case e ~ f => EApp(e, f) }
    | expr3
  )

  lazy val expr3: PackratParser[Expr] = (
      "Unit" ^^^ EUnit
    | "Type" ^^^ EType
    | "empty" ^^^ EEmpty
    | "Cat" ^^^ ECat
    | "tt" ^^^ ETt
    | "Obj" ~ expr3 ^^ 
        { case "Obj" ~ e => EOb(e) }
    | "Cell" ~ expr3 ~ frameExpr ^^ 
        { case "Cell" ~ e ~ frm => ECell(e, frm.value) }
    | "Hom" ~ expr3 ~ frameExpr ^^
        { case "Hom" ~ e ~ frm => EHom(e, frm.value) }
    | ident ^^ (EVar(_))
    | expr3 ~ ".1" ^^ 
        { case e ~ ".1" => EFst(e) }
    | expr3 ~ ".2" ^^ 
        { case e ~ ".2" => ESnd(e) }
    | "(" ~ expr ~ ")" ^^
        { case "(" ~ e ~ ")" => e }
  )

  @natElim
  def nstListToFrmPref[N <: Nat](n: N)(nl: NstList) : Suite[NstExpr, N] = {
    case (Z, Nil) => SNil[NstExpr]()
    case (Z, _) => ???
    case (S(p: P), Nil) => ???
    case (S(p: P), nst :: nsts) => {
      nstListToFrmPref(p)(nsts) >> nst.value.asInstanceOf[NstExpr[P]]
    }
  }

  lazy val addrExpr : PackratParser[Addr] = (
      addrExpr1 ~ "::" ~ addrExpr ^^
        { case a ~ "::" ~ b => ACons(a, b) }
    | addrExpr1
  )

  lazy val addrExpr1 : PackratParser[Addr] = (
      "#" ^^^ AUnit
    | "nil" ^^^ ANil
    | "(" ~ addrExpr ~ ")" ^^ 
        { case "(" ~ a ~ ")" => a }
  )

  @natElim
  def addrExpr[N <: Nat](n: N) : PackratParser[Address[N]] = {
    case Z => "#" ^^^ (())
    case S(p) => (
        "nil" ^^^ Nil
      | addrExpr(p) ~ "::" ~ addrExpr(S(p)) ^^
          { case hd ~ "::" ~ tl => hd :: tl }
    )
  }

  def frameExpr : PackratParser[Sigma[ExprComplex]] = 
    cmplxPrefix[NstExpr, _0](Z)(NestingExpr) ^^
      { case frm => {
        val frmPrefLength = intToNat(frm._1.length)
        val frmPref = nstListToFrmPref(frmPrefLength)(frm._1.reverse)
        Sigma[ExprComplex, Nat](frmPrefLength)(frmPref >> frm._2.value.asInstanceOf[NstExpr[Nat]])
      }}

  def nicheExpr : PackratParser[Sigma[NchExpr]] = 
    cmplxPrefix[TrExpr, _0](Z)(TreeExpr) ^^
      { case nch => {
        val frmPrefLength = intToNat(nch._1.length)
        val frmPref = nstListToFrmPref(frmPrefLength)(nch._1.reverse)
        Sigma[NchExpr, Nat](frmPrefLength)((frmPref, nch._2.value.asInstanceOf[TrExpr[Nat]]))
      }}

  def treeExpr[A, N <: Nat](n: N)(pp: PackratParser[A]) : PackratParser[Tree[A, N]] = (
      treeExpr1(n)(pp)
    | "(" ~ treeExpr(n)(pp) ~ ")" ^^
        { case "(" ~ t ~ ")" => t }
  )

  @natElim
  def treeExpr1[A, N <: Nat](n: N)(pp: PackratParser[A]) : PackratParser[Tree[A, N]] = {
    case (Z, pp) => "pt" ~ pp ^^ { case "pt" ~ a => Pt(a) }
    case (S(p), pp) => (
        "leaf" ^^^ Leaf(S(p))
      | "node" ~ pp ~ treeExpr(p)(treeExpr(S(p))(pp)) ^^
        { case "node" ~ a ~ sh => Node(a, sh) }
    )
  }

  def nstExpr[A, N <: Nat](n: N)(pp: PackratParser[A]) : PackratParser[Nesting[A, N]] = (
      nstExpr1(n)(pp)
    | "(" ~ nstExpr(n)(pp) ~ ")" ^^
        { case "(" ~ n ~ ")" => n }
  )

  @natElim
  def nstExpr1[A, N <:  Nat](n: N)(pp: PackratParser[A]) : PackratParser[Nesting[A, N]] = {
    case (Z, pp) => (
        "obj" ~ pp ^^ { case "obj" ~ a => Obj(a) }
      | "box" ~ pp ~ treeExpr(Z)(nstExpr(Z)(pp)) ^^ { case "box" ~ a ~ sh => Box(a, sh) }
    )
    case (S(p), pp) => (
        "dot" ~ pp ^^ { case "dot" ~ a => Dot(a, S(p)) }
      | "box" ~ pp ~ treeExpr(S(p))(nstExpr(S(p))(pp)) ^^ { case "box" ~ a ~ sh => Box(a, sh) }
    )
  }

  trait IndexedParser[T[_ <: Nat]] {
    def apply[N <: Nat](n: N) : PackratParser[T[N]]
  }

  object NestingExpr extends IndexedParser[NstExpr] {
    def apply[N <: Nat](n: N) : PackratParser[Nesting[Expr, N]] = 
      "[" ~ nstExpr(n)(expr3) ~ "]" ^^
        { case "[" ~ nst ~ "]" => nst }
  }

  object TreeExpr extends IndexedParser[TrExpr] {
    def apply[N <: Nat](n: N) : PackratParser[Tree[Expr, N]] = 
      "{" ~ treeExpr(n)(expr3) ~ "}" ^^
        { case "{" ~ tr ~ "}" => tr }
  }

  def cmplxPrefix[T[_ <: Nat], N <: Nat](n: N)(pp : IndexedParser[T]) : PackratParser[(NstList, Sigma[T])] = (
      "[" ~ nstExpr(n)(expr3) ~ "]>>" ~ cmplxPrefix(S(n))(pp) ^^
        { case "[" ~ nst ~ "]>>" ~ tl => (Sigma[NstExpr, N](n)(nst) :: tl._1, tl._2) }
    | pp(n) ^^ { case t => (Nil, Sigma(n)(t)) }
  )

  lazy val decl: PackratParser[Decl] = (
      "let" ~ pattern ~ ":" ~ expr ~ "=" ~ expr ^^ 
        { case "let" ~ p ~ ":" ~ e ~ "=" ~ f => Def(p, e, f) }
    | "letrec" ~ pattern ~ ":" ~ expr ~ "=" ~ expr ^^ 
        { case "letrec" ~ p ~ ":" ~ e ~ "=" ~ f => Drec(p, e, f) }
  )

  lazy val pattern: PackratParser[Patt] = (
      pattern1 ~ "," ~ pattern ^^
        { case p ~ "," ~ q => PPair(p, q) }
    | pattern1
  )

  lazy val pattern1: PackratParser[Patt] = (
      "_" ^^^ Punit
    | ident ^^ { PVar(_) }
    | "(" ~ pattern ~ ")" ^^ 
        { case "(" ~ p ~ ")" => p }
  )

  override protected val whiteSpace = """(\s|--.*)+""".r

}

object LineParser extends RegexParsers {

  override def skipWhitespace = false

  def nonEmptyLine : Parser[(Int, String)] = 
    "[ \\t]*".r ~ "^(?!--).*".r ^^ { case ws ~ ln => (ws.size, ln) }

  def emptyLine : Parser[(Int, String)] = 
    "[ \\t]*(--.*)?".r ^^^ (0, "")

  def unit : Parser[List[String]] = 
    repsep(nonEmptyLine | emptyLine, "([\\n\\r]|(\\n\\r))".r) ^^ { joinLines(_) }

  def joinLines(lns: List[(Int, String)]) : List[String] = {

    var res : List[String] = Nil
    var lastIndent : Int = 0

    for {
      (indent, ln) <- lns
      if (ln.length > 0)
    } {
      if (indent > lastIndent) {
        res = 
          res match {
            case Nil => { lastIndent = indent ; ln :: Nil }
            case l :: ls => (l ++ " " ++ ln) :: ls
          }
      } else {
        lastIndent = indent
        res = ln :: res
      }
    }

    res.reverse

  }
}
