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

sealed trait MDeriv[+A]
case object MUnit extends MDeriv[Nothing]
case class MSucc[+A](md: MDeriv[STree[A]], sd: SDeriv[STree[A]]) extends MDeriv[A]

trait CardinalTypes {

  type MAddr = List[SAddr]
  type SCardNst[+A] = MTree[SNesting[A]]
  type SCardinal[+A] = Suite[SCardNst[A]]

  case class SCardAddr(
    val branchAddr: MAddr = Nil,  // Find the correct branch
    val canopyAddr: SAddr = Nil,  // The address in the canopy there
    val boxAddr: SAddr    = Nil   // The address of the box
  ) { def dim: Int = branchAddr.length }

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
  case class SLeafBase(ma: MAddr) extends SLeafAddr
  case class SLeafSucc(ea: SLeafAddr) extends SLeafAddr

  // Leaf Markers
  abstract class LeafMarker[A] {

    type T[+U]

    val value: TShell[T[A]]
    def plug(ts: TShell[T[A]]): MTree[A]

  }

  implicit class MTreeOps[A](mt: MTree[A]) {

    def leafSeek(la: SLeafAddr): Option[LeafMarker[A]] = 
      (mt, la) match {
        case (MFix(MFix(t)), SLeafBase(ma)) =>
          for {
            pr <- t.mSeek(ma)
          } yield {

            new LeafMarker[A] {
              type T[+U] = Id[U]
              val value = pr._1
              def plug(ts: TShell[T[A]]): MTree[A] =
                MFix(MFix(pr._2.plug(ts)))
            }

          }
        case (MFix(t), SLeafSucc(ls)) => 
          for {
            lm <- t.leafSeek(ls)
          } yield {

            new LeafMarker[A] {
              type T[+U] = lm.T[STree[U]]
              val value = lm.value
              def plug(ts: TShell[T[A]]): MTree[A] = 
                MFix(lm.plug(ts))
            }

          }
        case _ => None
      }

    def completeWith(a: A): STree[A] =
      mt match {
        case MObj(t) => t
        case MFix(t) => SNode(a, t.completeWith(SLeaf))
      }

    def mSeek(ma: MAddr): Option[(STree[A], MDeriv[A])] = 
      (mt, ma) match {
        case (MObj(t), Nil) => Some(t, MUnit)
        case (MFix(t), addr :: addrs) => 
          for {
            res <- t.mSeek(addrs)
            (tr, d) = res
            plc <- tr.seekTo(addr)
            r <- plc.focus match {
              case SLeaf => None
              case SNode(tt, tsh) => 
                Some(tt, MSucc(d, SDeriv(tsh, plc.ctxt)))
            }
          } yield r
        case _ => None
      }

    def foreachWithAddr(op: (STree[A], MAddr) => Unit, ma: MAddr = Nil) : Unit = 
      mt match {
        case MObj(t) => op(t, ma)
        case MFix(t) => t.foreachWithAddr(
          (sh, pa) => sh.foreachWithAddr(
            (b, d) => op(b, d :: pa)
          )
        )
      }

  }

  implicit class MDerivOps[A](d: MDeriv[A]) {

    def plug(t: STree[A]): MTree[A] = 
      d match {
        case MUnit => MObj(t)
        case MSucc(md, sd) => MFix(md.plug(sd.plug(t)))
      }

  }


  implicit class SCardNstOps[A](cnst: SCardNst[A]) {

    def toNesting[B >: A](pos: B, neg: B): SNesting[B] = 
      cnst match {
        case MObj(t) => SBox(pos, t)
        case MFix(t) => SBox(pos, SNode(SDot(neg), t.completeWith(SLeaf)))
      }

    def toPolarityNesting: SNesting[Polarity[A]] = 
      cnst match {
        case MObj(t) => SBox(Positive(), t.map(_.map(Neutral(_))))
        case MFix(t) => SBox(Positive(), SNode(SDot(Negative()), 
          t.map(_.map(_.map(Neutral(_)))).completeWith(SLeaf)))
      }

    // You can actuall extract much more information, but you
    // haven't really defined the corresponding derivative type ...
    def seek(addr: SCardAddr): Option[SNstZipper[A]] = 
      for {
        mz <- cnst.mSeek(addr.branchAddr)
        cz <- mz._1.seekTo(addr.canopyAddr)
        n <- cz.focus.rootValue
        nz <- n.seek(addr.boxAddr)
      } yield nz

    def foreachWithAddr(op: (A, SCardAddr) => Unit): Unit = 
      MTreeOps(cnst).foreachWithAddr((canopy, ma) =>
        canopy.foreachWithAddr((nst, ca) => 
          nst.foreachWithAddr((a, ba) => op(a, SCardAddr(ma, ca, ba)))
        )
      )

    //============================================================================================
    // EXTRUSION
    //

    def extrudeAt(ca: SAddr, ma: MAddr, a: A)(pred: A => Boolean): Option[(SCardNst[A], STree[SNesting[A]])] = 
      for {
        pr <- cnst.mSeek(ma)
        (canopy, cd) = pr
        zp <- canopy.seekTo(ca)
        cut <- zp.focus.takeWhile((n: SNesting[A]) => pred(n.baseValue))
        (et, es) = cut
      } yield (cd.plug(SNode(SBox(a, et), es)), et)

    def extrudeFillerAt[B](ca: SAddr, ma: MAddr, a: A, msk: STree[B]): Option[SCardNst[A]] = 
      cnst match {
        case MFix(t) =>
          for {
            sd <- t.mSeek(ma)
            (nt, dd) = sd
            zp <- nt.seekTo(ca)
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

    def extrudeLeafAt[B](ca: SAddr, la: SLeafAddr, msk: STree[B]): Option[SCardNst[A]] = 
      for {
        lm <- cnst.leafSeek(la)
        zp <- lm.value.seekTo(ca)
        cut <- zp.focus.takeWithMask(msk)
        (et, es) = cut
      } yield {

        val extrusion = SNode(SNode(SLeaf, et), es)
        lm.plug(zp.ctxt.close(extrusion))

      }

  }

  implicit class CardinalOps[A](card: SCardinal[A]) {

    def toComplex(ps: Suite[(A, A)]): Option[SComplex[A]] = 
      (card, ps) match {
        case (||(MObj(cn)), ||((p, _))) => Some(||(SBox(p, cn)))
        case (ct >> chd, pt >> ((p, n))) =>
          for {
            tl <- ct.toComplex(pt)
            hd = chd.toNesting(p, n)
          } yield tl >> hd
        case _ => None
      }

    def seekNesting(addr: SCardAddr): Option[SNstZipper[A]] = 
      card.take(addr.dim + 1).head.seek(addr)


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

