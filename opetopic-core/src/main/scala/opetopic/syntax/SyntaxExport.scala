/**
  * SyntaxExport.scala - Exporting opetopes to formal syntax
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.syntax

import opetopic._
import mtl._


object SyntaxExport {

  sealed trait Term

  // Frame constructors
  case object EmptyFrm extends Term
  case class ExtendFrm(frm: Term, op: Term) extends Term

  // Opetope Constructors
  case object Ob extends Term
  case class Lf(frm: Term) extends Term
  case class Nd(frm: Term, op: Term, tyDec: Term, tmDec: Term) extends Term

  // Node decoration eliminators
  case object ObSrcElim extends Term
  case object LfSrcElim extends Term
  case class NdSrcElim(here: Term, there: Term) extends Term

  //
  //  Type class for objects which have a term representation ...
  //

  sealed trait HasTerm[A] {
    def toTerm(a: A): Term
  }

  object HasTerm {

    def apply[A](implicit ht: HasTerm[A]): HasTerm[A] = ht

    def treeAsDec[A](ht: HasTerm[A]) : HasTerm[STree[A]] =
      new HasTerm[STree[A]] {
        def toTerm(st: STree[A]) = toDecoration(st)(ht)
      }

    implicit val termHasTerm: HasTerm[Term] =
      new HasTerm[Term] {
        def toTerm(t: Term): Term = t
      }

  }

  // Yeah, this is actually pretty trivial: the elimination
  // structure is just the tree itself....
  def toDecoration[A : HasTerm](tr: STree[A]): Term =
    tr match {
      case SLeaf => LfSrcElim
      case SNode(a, sh) => {
        val ht = HasTerm[A]
        NdSrcElim(
          ht.toTerm(a),
          toDecoration(sh)(HasTerm.treeAsDec(ht))
        )
      }
    }

  def frameOf[A](c: SComplex[A]): Except[Term] =
    if (c.dim == 0)
      succeed(EmptyFrm)
    else if (c.dim == 1)
      succeed(ExtendFrm(EmptyFrm, Ob))
    else
      for {
        tgt <- attempt(c.target, "Error extracting target")
        tgtFrm <- frameOf(tgt)
        op <- complexToTerm(tgt)
      } yield ExtendFrm(tgtFrm, op)

  def complexToTerm[A](c: SComplex[A]): Except[Term] =
    c match {
      case ||(SDot(_)) => succeed(Ob)
      case ||(_) >> SDot(_) => succeed(Nd(EmptyFrm, Ob, ObSrcElim, ObSrcElim)) 
      case tl >> SBox(_, SLeaf) >> SDot(_) => frameOf(c).map(Lf(_))
      case tl >> (f @ SBox(tgtVal, SNode(SDot(_), sh))) >> SDot(hdVal) => {

        for {
          z <- attempt(SCmplxZipper(tl >> f).seek(SDir(Nil) :: Nil), "Error in seek")
          op <- attempt(z.focusFace, "Error extracting face.")
          shTms <- sh.traverseWithAddr((branch, addr) => {

            for {
              ztl <- attempt(z.tail, "Getting zipper tail")
              zlcl <- attempt(ztl.visit(SDir(addr)), "Visiting local address")
              subc <- attempt(zlcl.extract(branch), "Error during extraction")
              // This is a hack, reusing the values from before.
              // You have to be slightly more careful about the values here...
              branchCmplx = subc >> SBox(tgtVal, branch) >> SDot(hdVal)
              branchTgt <- attempt(branchCmplx.target, "Error getting branch target")
              branchTm <- complexToTerm(branchCmplx)
              branchTgtTm <- complexToTerm(branchTgt)
            } yield (branchTm, branchTgtTm)

          })
          (delta, epsilon) = STree.unzip(shTms)
          opTerm <- complexToTerm(op)
          frm <- frameOf(c)
        } yield {
          Nd(frm, opTerm, toDecoration(delta), toDecoration(epsilon))
        }

      }
      case _ => throwError("Malformed complex...")
    }

  //
  //  Now, I am going to write a routine to output the
  //  tree like structure of the pretty printer...
  //

  import PrettyPrinter._

  def toPrintTree(t: Term): Tree =
    t match {
      case EmptyFrm => Literal("*")
      case ExtendFrm(frm, op) => Nested("(", Iterator(toPrintTree(frm), toPrintTree(op)), " >> ", ")")
      case Ob => Literal("ob")
      case Lf(frm) => Nested("(lf ", Iterator(toPrintTree(frm)), " ", ")")
      case Nd(frm, op, delta, epsilon) =>
        Nested("(nd ", Iterator(toPrintTree(frm), toPrintTree(op), toPrintTree(delta), toPrintTree(epsilon)), " ", ")")
      case ObSrcElim => Literal("ob-elim")
      case LfSrcElim => Literal("lf-elim")
      case NdSrcElim(here, there) =>
        Nested("(nd-elim ", Iterator(toPrintTree(here), toPrintTree(there)), " ", ")")
    }

}

// (nd (* >> ob >> (nd * ob ob-elim ob-elim))
//   (nd * ob ob-elim ob-elim)
//   (nd-elim (lf (* >> ob >> (nd * ob ob-elim ob-elim))) lf-elim)
//   (nd-elim (nd * ob ob-elim ob-elim) lf-elim))
