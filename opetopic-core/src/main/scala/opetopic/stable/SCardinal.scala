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
case class MObj[+A](t: STree[A]) extends MTree[A] 
case class MFix[+A](t: MTree[STree[A]]) extends MTree[A] 

trait CardinalTypes {

  type SCardNst[+A] = MTree[SNesting[A]]
  type SCardAddr = Suite[SAddr]
  type SCardinal[+A] = Suite[SCardNst[A]]

  sealed trait SCardDeriv[+A] 
  case object SCardUnit extends SCardDeriv[Nothing]
  case class SCardSucc[+A](cd: SCardDeriv[STree[A]], d: SDeriv[STree[A]]) extends SCardDeriv[A]

  implicit object MTreeTraverse extends Traverse[MTree] {
    def traverseImpl[G[_], A, B](mt: MTree[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[MTree[B]] = 
      mt match {
        case MObj(t) => isAp.ap(t.traverse(f))(isAp.pure(MObj(_)))
        case MFix(t) => isAp.ap(traverseImpl(t)(_.traverse(f)))(isAp.pure(MFix(_)))
      }
  }

  implicit object CardNstTraverse extends Traverse[SCardNst] {
    def traverseImpl[G[_], A, B](c: SCardNst[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[SCardNst[B]] = 
      Traverse[MTree].traverse(c)(
        Traverse[SNesting].traverse(_)(f)
      )
  }

  implicit object CardinalTraverse extends Traverse[SCardinal] {
    def traverseImpl[G[_], A, B](c: SCardinal[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[SCardinal[B]] = 
      Traverse[Suite].traverse(c)(
        Traverse[SCardNst].traverse(_)(f)
      )
  }


  // Leaf addresses
  sealed trait SLeafAddr
  case class SLeafBase(ca: SCardAddr) extends SLeafAddr
  case class SLeafSucc(ea: SLeafAddr) extends SLeafAddr

  // Leaf Markers
  abstract class LeafMarker[A] {

    type T[+U]

    val value: TShell[T[A]]
    val deriv: SCardDeriv[TShell[T[A]]]

    // I think we need a special method for
    // closing up the derivative!
    def plug(ts: TShell[T[A]]): MTree[A]

  }

  implicit class MTreeOps[A](mt: MTree[A]) {

    // This is supposed to be a seeking mechanism for
    // enclosing an arbitrary number of extra types
    // inside a codimension 2 fixpoint.  We'll see if
    // it works or not ...
    def leafSeek(la: SLeafAddr): Option[LeafMarker[A]] = ???
      // (mt, la) match {
      //   case (MFix(MFix(t)), SLeafBase(ca)) =>
      //     for {
      //       pr <- t.newSeek(ca)
      //     } yield {

      //       new LeafMarker[A] {
      //         type T[+U] = Id[U]
      //         val value = pr._1
      //         val deriv = pr._2
      //         def plug(ts: TShell[T[A]]): MTree[A] =
      //           MFix(MFix(MFix(deriv.plug(ts))))
      //       }

      //     }
      //   case (MFix(t), SLeafSucc(ls)) => 
      //     for {
      //       lm <- t.leafSeek(ls)
      //     } yield {

      //       new LeafMarker[A] {
      //         type T[+U] = lm.T[STree[U]]
      //         val value = lm.value
      //         val deriv = lm.deriv
      //         def plug(ts: TShell[T[A]]): MTree[A] = 
      //           MFix(lm.plug(ts))
      //       }

      //     }
      //   case _ => None
      // }

    def completeWith(a: A): STree[A] =
      mt match {
        case MObj(t) => t
        case MFix(t) => SNode(a, t.completeWith(SLeaf))
      }

    // def seek(ca: SCardAddr): Option[(A, SCardDeriv[A])] = 
    //   (mt, ca) match {
    //     case (MObj(t), ||(Nil)) => 
    //       for {
    //         a <- t.rootValue
    //       } yield (a, SCardUnit)
    //     case (MFix(t), tl >> hd) => 
    //       for {
    //         res <- t.seek(tl)
    //         (tr, d) = res
    //         plc <- tr.seekTo(hd)
    //         r <- plc.focus match {
    //           case SLeaf => None
    //           case SNode(a, sh) => 
    //             Some(a, SCardSucc(d, SDeriv(sh, plc.ctxt)))
    //         }
    //       } yield r
    //     case _ => None
    //   }

    def canopySeek(ca: SCardAddr): Option[(STree[A], SCardDeriv[A])] = 
      (mt, ca) match {
        case (MObj(t), ||(Nil)) => Some(t, SCardUnit)
        case (MFix(t), tl >> hd) => 
          for {
            res <- t.canopySeek(tl)
            (tr, d) = res
            plc <- tr.seekTo(hd)
            r <- plc.focus match {
              case SLeaf => None
              case SNode(tt, tsh) => 
                Some(tt, SCardSucc(d, SDeriv(tsh, plc.ctxt)))
            }
          } yield r
        case _ => None
      }

    def foreachWithAddr(op: (A, SCardAddr) => Unit) : Unit = 
      mt match {
        case MObj(t) => t.foreachWithAddr((a, d) => op(a, ||(d)))
        case MFix(t) => t.foreachWithAddr((b, p) => 
          b.foreachWithAddr((a, d) => op(a, p >> d))
        )
      }

  }

  implicit class SCardDerivOps[A](d: SCardDeriv[A]) {

    def plug(t: STree[A]): MTree[A] = 
      d match {
        case SCardUnit => MObj(t)
        case SCardSucc(cd, d) => MFix(cd.plug(d.plug(t)))
      }

  }


  implicit class SCardNstOps[A](c: SCardNst[A]) {

    def toNesting[B >: A](pos: B, neg: B): SNesting[B] = 
      c match {
        case MObj(t) => SBox(pos, t)
        case MFix(t) => SBox(pos, SNode(SDot(neg), t.completeWith(SLeaf)))
      }

    def toPolarityNesting: SNesting[Polarity[A]] = 
      c match {
        case MObj(t) => SBox(Positive(), t.map(_.map(Neutral(_))))
        case MFix(t) => SBox(Positive(), SNode(SDot(Negative()), 
          t.map(_.map(_.map(Neutral(_)))).completeWith(SLeaf)))
      }

    //============================================================================================
    // EXTRUSION
    //

    def extrudeAt(hd: SAddr, tl: SCardAddr, a: A)(pred: A => Boolean): Option[(SCardNst[A], STree[SNesting[A]])] = 
      for {
        pr <- c.canopySeek(tl)
        (canopy, cd) = pr
        zp <- canopy.seekTo(hd)
        cut <- zp.focus.takeWhile((n: SNesting[A]) => pred(n.baseValue))
        (et, es) = cut
      } yield (cd.plug(SNode(SBox(a, et), es)), et)

    def extrudeFillerAt[B](hd: SAddr, tl: SCardAddr, a: A, msk: STree[B]): Option[SCardNst[A]] = 
      c match {
        case MFix(t) =>
          for {
            sd <- t.canopySeek(tl)
            (nt, dd) = sd
            zp <- nt.seekTo(hd)
            cut <- zp.focus.takeWithMask(msk)
            (et, es) = cut
          } yield {

            val extrusion = SNode(SNode(SDot(a), et), es)
            val ncn : MTree[STree[SNesting[A]]] =
              dd.plug(zp.ctxt.close(extrusion))

            // And this is consistent, we should have to 
            // apply exactly one MFix to get back where we
            // were
            MFix(ncn)

          }
        case _ => None // Must have at least dimension 1
      }

    def extrudeLeafAt[B](hd: SAddr, la: SLeafAddr, msk: STree[B]): Option[SCardNst[A]] = 
      c match {
        case MFix(MFix(t)) => None
        case _ => None
      }

    //   for {
    //     lm <- c.leafSeek(la)
    //     zp <- lm.value.seekTo(hd)
    //     cut <- zp.focus.takeWithMask(msk)
    //     (et, es) = cut
    //   } yield {

    //     val extrusion = SNode(SNode(SLeaf, et), es)
    //     lm.plug(zp.ctxt.close(extrusion))

    //   }

  }

  implicit class CardinalOps[A](c: SCardinal[A]) {

    def toComplex(ps: Suite[(A, A)]): Option[SComplex[A]] = 
      (c, ps) match {
        case (||(MObj(cn)), ||((p, _))) => Some(||(SBox(p, cn)))
        case (ct >> chd, pt >> ((p, n))) =>
          for {
            tl <- ct.toComplex(pt)
            hd = chd.toNesting(p, n)
          } yield tl >> hd
        case _ => None
      }

    def seek(addr: SCardAddr): Option[(SNesting[A], SCardDeriv[SNesting[A]])] = ???
      // c.take(addr.length).head.seek(addr)

    // Okay, so I'd like to actually start working on the extrusion.  Let's 
    // go at it from this end.  The first thing I'm going to need is to
    // split the cardinal based on the dimension I'm passed.  To do this,
    // I'll need a routine to split a suite.

    // def extrude(addr: SCardAddr, a: A)(pred: A => Boolean): Option[SCardinal[A]] = {

    //   val (p, l) = c.splitAt(addr.length)

    //   p match {
    //     case ||(_) => None
    //     case tl >> hd => {

    //       val extNst = tl.head
    //       val fillNst = hd

    //       // So, the idea is that we've got the head and filler guys.
    //       // All the things left in the list there should have leaves
    //       // added to them.

    //       for {
    //         pr <- extNst.extrudeAt(addr, a)(pred)
    //         (cn, msk) = pr
    //       } yield ???

    //     }
    //   }

    //   None

    // }

  }

  object SCardinal {

    def apply[A](a: A): SCardinal[A] = 
      ||(MObj(STree.obj(SDot(a)))) >> MFix(MObj(SLeaf))

    def apply[A](c: SComplex[A]): SCardinal[A] = 
      fromComplex(c)._1

    def fromComplex[A](c: SComplex[A]): (SCardinal[A], SDeriv[SNesting[A]]) = 
      c match {
        case ||(nst) => (||(MObj(STree.obj(nst))), SDeriv(STree.obj(SLeaf)))
        case tl >> hd => {

          val (nTl, SDeriv(sh, _)) = fromComplex(tl)

          val nHd: MTree[STree[SNesting[A]]] = 
            Traverse[MTree].map(nTl.head)(
              (n: SNesting[A]) => SNode(hd, sh)
            )

          (nTl >> MFix(nHd), SDeriv(tl.head.toTree.asShell))

        }
      }

  }

}

