/**
  * SCardinal.scala - Experiments with Cardinals
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Id._
import scalaz.Leibniz
import scalaz.Leibniz._
import scalaz.Traverse
import scalaz.Applicative
import scalaz.syntax.traverse._

import opetopic._

sealed trait MTree[+A] 
case class MObj[+A](t: A) extends MTree[A] 
case class MFix[+A](t: MTree[STree[A]]) extends MTree[A] 

trait CardinalTypes {

  type SCardNst[+A] = MTree[SNesting[A]]
  type SCardAddr = Suite[SAddr]
  type SCardinal[+A] = Suite[SCardNst[A]]

  // Extended addresses
  sealed trait SExtAddr
  case class SExtBase(ca: SCardAddr) extends SExtAddr
  case class SExtSucc(ea: SExtAddr) extends SExtAddr

  sealed trait SCardDeriv[+A] 
  case object SCardUnit extends SCardDeriv[Nothing]
  case class SCardSucc[+A](cd: SCardDeriv[STree[A]], d: SDeriv[A]) extends SCardDeriv[A]

  implicit object MTreeTraverse extends Traverse[MTree] {
    def traverseImpl[G[_], A, B](mt: MTree[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[MTree[B]] = 
      mt match {
        case MObj(a) => isAp.ap(f(a))(isAp.pure(MObj(_)))
        case MFix(t) => isAp.ap(traverseImpl(t)(_.traverse(f)))(isAp.pure(MFix(_)))
      }
  }

  implicit object CardinalTraverse extends Traverse[SCardinal] {
    def traverseImpl[G[_], A, B](c: SCardinal[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[SCardinal[B]] = {
      Traverse[Suite].traverse(c)(
        Traverse[MTree].traverse(_)(
          Traverse[SNesting].traverse(_)(f)
        )
      )
    }
  }

  // Okay, this seems to do the trick.  It let's you tag the returned
  // type from the seek operation below with an extra external STree
  // instance so that you can the continue to seek on it.  Will will
  // probably need a second evident class for the case when we need
  // to use double successors ...

  // The other option would to make this somehow a case class as well
  // which could then be inductively built up as we pass down out of
  // the outer fixpoint calls and build the guy back up.

  abstract class SSuccDeriv[A] {

    type T[+_]

    val value: STree[T[A]]
    val deriv: SCardDeriv[STree[T[A]]]

  }


  implicit class MTreeOps[A](mt: MTree[A]) {

    def seekPartial(ea: SExtAddr): Option[SSuccDeriv[A]] = 
      (mt, ea) match {
        case (MFix(t), SExtBase(ca)) => 
          for {
            pr <- t.seek(ca)
          } yield {

            new SSuccDeriv[A] {

              type T[+_] = Id[_]

              val value = pr._1
              val deriv = pr._2

            }

          }
        case (MFix(t), SExtSucc(eb)) => 
          for {
            ed <- t.seekPartial(eb)
          } yield {

            val edr: SSuccDeriv[STree[A]] = ed
            val v0: STree[ed.T[STree[A]]] = ed.value

            new SSuccDeriv[A] {
              type T[+_] = ed.T[STree[_]]
              val value = ed.value
              val deriv = ed.deriv
            }

          }
        case _ => None
      }

    def completeWith(a: A): STree[A] = 
      mt match {
        case MObj(x) => STree.obj(x)
        case MFix(t) => SNode(a, t.completeWith(SLeaf))
      }

    def seekDblSucc(ca: SCardAddr): Option[(Shell[A], SCardDeriv[Shell[A]])] = 
      mt match {
        case MFix(MFix(t)) => t.seek(ca)
        case _ => None
      }

    def seekSucc(ca: SCardAddr): Option[(STree[A], SCardDeriv[STree[A]])] = 
      mt match {
        case MFix(t) => t.seek(ca)
        case _ => None
      }

    def seek(ca: SCardAddr): Option[(A, SCardDeriv[A])] = 
      (mt, ca) match {
        case (MObj(a), ||(Nil)) => Some(a, SCardUnit)
        case (MFix(t), tl >> hd) => 
          for {
            res <- t.seek(tl)
            (tr, d) = res
            plc <- tr.seekTo(hd)
            r <- plc.focus match {
              case SLeaf => None
              case SNode(a, sh) => 
                Some(a, SCardSucc(d, SDeriv(sh, plc.ctxt)))
            }
          } yield r
        case _ => None
      }

  }

  implicit class SCardDerivOps[A](d: SCardDeriv[A]) {

    def plug(a: A): MTree[A] = 
      d match {
        case SCardUnit => MObj(a)
        case SCardSucc(cd, d) => MFix(cd.plug(d.plug(a)))
      }

  }


  implicit class SCardNstOps[A](c: SCardNst[A]) {

    def toNesting[B >: A](pos: B, neg: B): SNesting[B] = 
      c match {
        case MObj(n) => SBox(pos, STree.obj(n))
        case MFix(t) => SBox(pos, SNode(SDot(neg), t.completeWith(SLeaf)))
      }

    def toPolarityNesting: SNesting[Polarity[A]] = 
      c match {
        case MObj(n) => SBox(Positive(), STree.obj(n.map(Neutral(_))))
        case MFix(t) => SBox(Positive(), SNode(SDot(Negative()), 
          t.map(_.map(_.map(Neutral(_)))).completeWith(SLeaf)))
      }

    //============================================================================================
    // EXTRUSION
    //

    def extudeAt(ca: SCardAddr, a: A)(pred: A => Boolean): Option[(SCardNst[A], STree[SNesting[A]])] = 
      (c, ca) match {
        case (MObj(n), ||(Nil)) => 
          if (pred(n.baseValue)) {
            Some(MObj(SBox(a, STree.obj(n))), STree.obj(n))
          } else None
        case (MFix(t), tl >> hd) => 
          for {
            pr <- t.seek(tl)
            (canopy, cd) = pr
            zp <- canopy.seekTo(hd)
            cut <- zp.focus.takeWhile((n:  SNesting[A]) => pred(n.baseValue))
          } yield {
            val (excisedTree, excisedShell) = cut
            (MFix(cd.plug(SNode(SBox(a, excisedTree), excisedShell))), excisedTree)
          }
        case _ => None
      }


    // Okay, this looks decent.  There's still a whole lot of address things to do 
    // to get the dimensions to match correctly, etc.
    def extrudeFillerAt[B](hd: SAddr, tl: SCardAddr, a: A, msk: STree[B]): Option[SCardNst[A]] = 
      for {
        sd <- c.seekDblSucc(tl)
        (nt, dd) = sd
        zp <- nt.seekTo(hd)
        cut <- zp.focus.takeWithMask(msk)
      } yield {

        val test = SNode(SNode(SDot(a), cut._1), cut._2)
        val blorp = zp.ctxt.close(test)
        val bleep = dd.plug(blorp)

        MFix(MFix(bleep))

      }

    def extrudeLeafAt[B](ca: SCardAddr, msk: STree[B]): Option[SCardNst[A]] = 
      for {
        ed <- c.seekPartial(SExtBase(ca))
        zp <- ed.value.seekTo(ca.head)
        pr <- zp.focus.takeWithMask(msk)
      } yield {

        val test: STree[ed.T[SNesting[A]]] = zp.focus
        val newShit = SNode(???, pr._2)

        ???

      }

    // @lteElim
    // def extrudeLeafAt[A, B, K <: Nat, N <: Nat, D <: Nat](lte: Lte[S[S[K]], N, D])(
    //   cn: CardinalNesting[A, N], ca: CardinalAddress[K], msk: Tree[B, K]
    // ) : ShapeM[CardinalNesting[A, N]] = {
    //   case (SuccLte(SuccLte(ZeroLte(n0: N0))), cn, ca, msk) =>
    //     for {
    //       pr <- tailWithDerivative[Nesting[A, S[S[N0]]], _0, S[N0], S[N0]](cn, ca)(ZeroLte(S(n0)))
    //     } yield {
    //       symm[Nothing, Any, CardinalNestingDblSucc[A, N0], TreeSeqDblSucc[Nesting[A, S[S[N0]]], _0, N0]](
    //         cardinalTreeAssoc(SuccLte(ZeroLte(n0)))
    //       )(
    //         plugCardinal(__0)(pr._2, Node(seqLeaf(__1, n0), Pt(pr._1)))
    //       )
    //     }
    //   case (SuccLte(SuccLte(SuccLte(plte: (K0, N0, D0)))), cn, tl >> hd, msk) => {

    //     val k0 = plte.lower
    //     val ev : Lte[K0, S[S[N0]], S[S[D0]]] = lteSucc(lteSucc(plte))

    //     for {
    //       pr0 <- tailWithDerivative[Nesting[A, S[S[S[N0]]]], K0, S[S[N0]], S[S[D0]]](cn, tl)(ev)
    //       pr1 <- Tree.seekTo(pr0._1, hd)
    //       pr2 <- Tree.exciseWithMask(pr1._1, msk)
    //     } yield {
    //       symm[Nothing, Any, CardinalNestingTrplSucc[A, N0], CardinalTree[TreeSeq[Nesting[A, S[S[S[N0]]]], S[K0], S[S[D0]]], K0]](
    //         cardinalTreeAssoc[Nesting[A, S[S[S[N0]]]], K0, S[S[N0]], S[S[D0]]](ev)
    //       )(
    //         plugCardinal(k0)(pr0._2,
    //           Zipper.close(S(k0))(pr1._2, Node(Node(seqLeaf(S(S(k0)), plte.diff), pr2._1), pr2._2))
    //         )
    //       )
    //     }
    //   }
    // }



  }

  implicit class CardinalOps[A](c: SCardinal[A]) {

    def toComplex(ps: Suite[(A, A)]): Option[SComplex[A]] = 
      (c, ps) match {
        case (||(MObj(cn)), ||((p, _))) => Some(||(SBox(p, STree.obj(cn))))
        case (ct >> chd, pt >> ((p, n))) =>
          for {
            tl <- ct.toComplex(pt)
            hd = chd.toNesting(p, n)
          } yield tl >> hd
        case _ => None
      }

  }

  object SCardinal {

    def apply[A](a: A): SCardinal[A] = 
      ||(MObj(SDot(a))) >> MFix(MObj(SLeaf))

  }

}

