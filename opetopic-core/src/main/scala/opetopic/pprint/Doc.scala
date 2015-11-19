/**
  * Doc.scala - Wadler's Pretty Printer
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.pprint

import opetopic._
import syntax.tree._
import syntax.nesting._

sealed trait Doc 
case object Empty extends Doc 
case class Text(s: String, x: Doc) extends Doc 
case class Line(i: Int, x: Doc) extends Doc 

sealed trait DocF
case object EmptyF extends DocF
case class <>:(x: DocF, y: DocF) extends DocF
case class <|>:(x: DocF, y: DocF) extends DocF
case class NestF(i: Int, x: DocF) extends DocF
case class TextF(s: String) extends DocF
case object LineF extends DocF

object Doc {

  implicit class DocFOps(x: DocF) {
    // def <>(y: DocF) = new <:>(x, y)
    def <>:(y: DocF) = new <>:(y, x)
    def <|>:(y: DocF) = new <|>:(y, x)
    def <+>(y: DocF) = x <>: text(" ") <>: y
    def </>(y: DocF) = x <>: line <>: y
    def <+/>(y: DocF) = x <>: (text(" ") <|>: line) <>: y
  }

  def folddoc(f : (DocF, DocF) => DocF)(l: List[DocF]) : DocF = 
    l match {
      case Nil => EmptyF
      case x :: Nil => x
      case x :: xs => f(x, folddoc(f)(xs))
    }

  def bracket(l: String, x: DocF, r: String) = 
    group(text(l) <>: nest(2, line <>: x) <>: text(r))

  def separate(l: List[DocF]) : DocF =
    folddoc(_<+/>_)(l)

  def spread(l: List[DocF]) : DocF = 
    folddoc(_<+>_)(l)

  def stack(l: List[DocF]) : DocF = 
    folddoc(_</>_)(l)

  def nil: DocF = EmptyF
  def text(s: String) : DocF = TextF(s)
  def nest(i: Int, x:DocF) : DocF = NestF(i, x)
  def line: DocF = LineF
 
  def group(x: DocF) : DocF =
    flatten(x) <|>: x

  def flatten(x: DocF) : DocF = 
    x match {
      case EmptyF => EmptyF
      case <>:(x, y) => flatten(x) <>: flatten(y)
      case NestF(i, x) => NestF(i, flatten(x))
      case TextF(s) => TextF(s)
      case LineF => TextF(" ")
      case <|>:(x, y) => flatten(x)
    }

  // def layout(x : Doc) : Trampoline[String] =
  //   x match {
  //     case Empty => done("")
  //     case Text(s, y) => for { l <- suspend(layout(y)) } yield s ++ l
  //     case Line(j, y) => for { l <- suspend(layout(y)) } yield "\n" ++ (" " * j) ++ l
  //   }

  def layout(x : Doc) : String =
    x match {
      case Empty => ""
      case Text(s, y) => s ++ layout(y)
      case Line(j, y) => "\n" ++ (" " * j) ++ layout(y)
    }

  def best(w: Int, k: Int, x: DocF) : Doc = 
    be(w, k, List((0, x)))

  // def be(w: Int, k: Int, l: List[(Int, DocF)]) : Trampoline[Doc] = 
  //   l match {
  //     case Nil => done(Empty)
  //     case (i, EmptyF) :: ls => suspend(be(w, k, ls))
  //     case (i, <>:(x, y)) :: ls => suspend(be(w, k, ((i, x) :: (i, y) :: ls)))
  //     case (i, NestF(j, x)) :: ls => suspend(be(w, k, (i + j, x) :: ls))
  //     case (i, TextF(s)) :: ls => for { d <- suspend(be(w, k + s.length, ls)) } yield Text(s, d)
  //     case (i, LineF) :: ls => for { d <- suspend(be(w, k, ls)) } yield Line(i, d)
  //     case (i, <|>:(x, y)) :: ls => 
  //       for {
  //         d <- suspend(be(w, k, (i, x) :: ls))
  //         e <- suspend(be(w, k, (i, y) :: ls))
  //       } yield better(w, k, d, e)
  //   }

  def be(w: Int, k: Int, l: List[(Int, DocF)]) : Doc = 
    l match {
      case Nil => Empty
      case (i, EmptyF) :: ls => be(w, k, ls)
      case (i, <>:(x, y)) :: ls => be(w, k, ((i, x) :: (i, y) :: ls))
      case (i, NestF(j, x)) :: ls => be(w, k, (i + j, x) :: ls)
      case (i, TextF(s)) :: ls => Text(s, be(w, k + s.length, ls))
      case (i, LineF) :: ls => Line(i, be(w, k, ls))
      case (i, <|>:(x, y)) :: ls => better(w, k, be(w, k, (i, x) :: ls), be(w, k, (i, y) :: ls))
    }

  def better(w: Int, k: Int, x: Doc, y: Doc) : Doc = 
    if (fits(w - k, x)) x else y

  def fits(w: Int, x: Doc) : Boolean = 
    if (w < 0) false else
      x match {
        case Empty => true
        case Text(s, y) => fits(w - s.length, y)
        case Line(j, y) => true
        case _ => throw new IllegalArgumentException("whoops ...")
      }

  trait PPrint[A] {
    def toDoc(a: A) : DocF
  }

  implicit def treePP[A, N <: Nat](implicit pp: PPrint[A]) : PPrint[Tree[A, N]] = 
    new PPrint[Tree[A, N]] {
      def toDoc(t: Tree[A, N]) = treeToDoc(t.dim)(t)
    }

  implicit def nestingPP[A, N <: Nat](implicit pp: PPrint[A]) : PPrint[Nesting[A, N]] = 
    new PPrint[Nesting[A, N]] {
      def toDoc(n: Nesting[A, N]) = nestingToDoc(n)
    }

  implicit object IntPP extends PPrint[Int] {
    def toDoc(i: Int) = text(i.toString)
  }

  @natElim
  def treeToDoc[A, N <: Nat](n: N)(tr: Tree[A, N])(implicit pp: PPrint[A]) : DocF = {
    case (Z, Pt(a)) =>             text("pt") </> nest(2, group(pp.toDoc(a)))
    case (S(p: P), Leaf(d)) =>     text("leaf")
    case (S(p: P), Node(a, sh)) => text("node") </> nest(2, group(pp.toDoc(a))) </> nest(2, group(treeToDoc(p)(sh)))
  }

  def nestingToDoc[A, N <: Nat](n: Nesting[A, N])(implicit pp: PPrint[A]) : DocF = 
    n match {
      case Obj(a) =>     text("obj") </> pp.toDoc(a)
      case Dot(a, _) =>  text("dot") </> pp.toDoc(a)
      case Box(a, cn) => text("box") </> nest(2, pp.toDoc(a)) </> nest(2, group(treeToDoc(cn.dim)(cn)))
    }

  def pprint[A](a: A, w: Int = 100)(implicit pp: PPrint[A]) : String = 
    layout(best(w, 0, pp.toDoc(a)))

}
