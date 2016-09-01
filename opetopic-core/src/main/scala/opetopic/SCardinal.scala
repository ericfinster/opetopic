/**
  * SCardinal.scala - Experiments with Cardinals
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

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
  ) { 
    
    def dim: Int = branchAddr.length 

    def complexAddress: SAddr = {

      def processBranch(sa: SAddr, ma: MAddr): SAddr = 
        ma match {
          case Nil => SDir(sa) :: Nil
          case m :: ms => SDir(sa ++ processBranch(m, ms)) :: Nil
        }

      boxAddr ++ processBranch(canopyAddr, branchAddr)

    }

    override def toString = 
      "\nbr: " + branchAddr.toString + "\n" +
        "cn: " + canopyAddr.toString + "\n" +
        "bx: " + boxAddr.toString 

  }

  implicit object MTreeTraverse extends Traverse[MTree] {
    def traverse[G[_], A, B](mt: MTree[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[MTree[B]] = 
      mt match {
        case MObj(t) => isAp.ap(t.traverse(f))(isAp.pure(MObj(_)))
        case MFix(t) => isAp.ap(traverse(t)(_.traverse(f)))(isAp.pure(MFix(_)))
      }
  }

  implicit object CardNstTraverse extends Traverse[SCardNst] {
    def traverse[G[_], A, B](c: SCardNst[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[SCardNst[B]] = 
      Traverse[MTree].traverse(c)(
        Traverse[SNesting].traverse(_)(f)
      )
  }

  implicit object CardinalTraverse extends Traverse[SCardinal] {
    def traverse[G[_], A, B](c: SCardinal[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[SCardinal[B]] = 
      Traverse[Suite].traverse(c)(
        Traverse[SCardNst].traverse(_)(f)
      )
  }


  // Leaf addresses
  sealed trait SLeafAddr
  case class SLeafBase(ma: MAddr) extends SLeafAddr
  case class SLeafSucc(ea: SLeafAddr) extends SLeafAddr

  object SLeafAddr {

    // Pad an MAddr with leaf depth markers
    def apply(i: Int, ma: MAddr): SLeafAddr =
      if (i <= 0) SLeafBase(ma) else
        SLeafSucc(SLeafAddr(i-1, ma))

  }

  // Leaf Markers
  abstract class LeafDeriv[A] {

    type T[+U]

    val value: TShell[T[A]]
    def plug(ts: TShell[T[A]]): MTree[A]
    def leaf[B]: T[STree[B]]

  }

  implicit class MTreeOps[A](mt: MTree[A]) {

    def rootValue: Option[A] =
      mt match {
        case MObj(tr) => tr.rootValue
        case MFix(t) =>
          for {
            m <- t.rootValue
            a <- m.rootValue
          } yield a
      }

    def unfold: Option[MTree[STree[A]]] =
      mt match {
        case MFix(t) => Some(t)
        case _ => None
      }

    // This probably more properly belongs to the
    // cardinal nesting ops, no?
    def leafSeek(la: SLeafAddr): Option[LeafDeriv[A]] = 
      (mt, la) match {
        case (MFix(MFix(t)), SLeafBase(ma)) =>
          for {
            pr <- t.mSeek(ma)
          } yield {

            new LeafDeriv[A] {
              type T[+U] = Id[U]
              val value = pr._1
              def plug(ts: TShell[T[A]]): MTree[A] =
                MFix(MFix(pr._2.plug(ts)))
              def leaf[B] = SLeaf
            }

          }
        case (MFix(t), SLeafSucc(ls)) => 
          for {
            lm <- t.leafSeek(ls)
          } yield {

            new LeafDeriv[A] {
              type T[+U] = lm.T[STree[U]]
              val value = lm.value
              def plug(ts: TShell[T[A]]): MTree[A] = 
                MFix(lm.plug(ts))
              def leaf[B] = lm.leaf
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

    def foreachWithAddr(op: (A, SCardAddr) => Unit): Unit = 
      MTreeOps(cnst).foreachWithAddr((canopy, ma) =>
        canopy.foreachWithAddr((nst, ca) => 
          nst.foreachWithAddr((a, ba) => op(a, SCardAddr(ma, ca, ba)))
        )
      )

    //============================================================================================
    // NESTING EXTRACTION
    //

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

    //============================================================================================
    // SEEK ROUTINES
    //

    // You can actualy extract much more information, but you
    // haven't really defined the corresponding derivative type ...
    def seek(addr: SCardAddr): Option[SNstZipper[A]] = 
      for {
        mz <- cnst.mSeek(addr.branchAddr)
        cz <- mz._1.seekTo(addr.canopyAddr)
        n <- cz.focus.rootValue
        nz <- n.seek(addr.boxAddr)
      } yield nz

    // The address points to a box.  We return a zipper in the
    // canopy of the box containing this one, or in the case where
    // it is a root of some polarized canopy, a zipper therein.
    def canopySeek(addr: SCardAddr): Option[SZipper[SNesting[A]]] = 
      for {
        mz <- cnst.mSeek(addr.branchAddr)
        cz <- mz._1.seekTo(addr.canopyAddr)
        czr <- addr.boxAddr match {
          case Nil => Some(cz)
          case SDir(d) :: ds => 
            for {
              bn <- cz.focus.rootValue
              bz <- bn.seek(ds)
              bcn <- bz.focus.boxOption
              (_, cn) = bcn
              r <- cn.seekTo(d)
            } yield r
        }
      } yield czr

    //============================================================================================
    // EXTRUSION
    //

    def extrudeAt(addr: SCardAddr, a: A)(pred: A => Boolean): Option[(SCardNst[A], STree[SNesting[A]])] = 
      for {
        pr <- cnst.mSeek(addr.branchAddr)
        (canopy, cd) = pr
        zp <- canopy.seekTo(addr.canopyAddr)
        cut <- zp.focus.takeWhile((n: SNesting[A]) => pred(n.baseValue))
        (et, es) = cut
      } yield (cd.plug(zp.ctxt.close(SNode(SBox(a, et), es))), et)

    def extrudeAtWithMask[B](addr: SCardAddr, a: A)(msk: STree[B]): Option[SCardNst[A]] =
      for {
        pr <- cnst.mSeek(addr.branchAddr)
        (canopy, cd) = pr
        zp <- canopy.seekTo(addr.canopyAddr)
        cut <- zp.focus.takeWithMask(msk)
        (et, es) = cut
      } yield cd.plug(zp.ctxt.close(SNode(SBox(a, et), es)))

    def extrudeFillerAt[B](addr: SCardAddr, a: A)(msk: STree[B]): Option[SCardNst[A]] =
      for {
        t <- cnst.unfold
        sd <- t.mSeek(addr.branchAddr)
        (nt, dd) = sd
        zp <- nt.seekTo(addr.canopyAddr)
        cut <- zp.focus.takeWithMask(msk)
        (et, es) = cut
      } yield {

        val extrusion = SNode(SNode(SDot(a), et), es)
        val ncn : MTree[STree[SNesting[A]]] =
          dd.plug(zp.ctxt.close(extrusion))

        MFix(ncn)

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

    //============================================================================================
    // LOOPS AND DROPS
    //

    def loopAt(addr: SCardAddr, a: A): Option[SCardNst[A]] = 
      for {
        t <- cnst.unfold
        pr <- t.mSeek(addr.branchAddr)
        (canopy, cd) = pr
        cz <- canopy.seekTo(addr.canopyAddr)
        bbrs <- cz.focus.nodeOption
        (br, brs) = bbrs
      } yield {

        // Okay, not sure, but I think this is right.
        val loopExt: Shell[SNesting[A]] =
          SNode(SNode(SBox(a, SLeaf), SNode(br, brs.asShell)), brs)

        MFix(cd.plug(cz.closeWith(loopExt)))

      }

    def loopFillerAt(addr: SCardAddr, a: A): Option[SCardNst[A]] = 
      for {
        u <- cnst.unfold
        t <- u.unfold
        pr <- t.mSeek(addr.branchAddr)
        (canopy, cd) = pr
        cz <- canopy.seekTo(addr.canopyAddr)
        bbrs <- cz.focus.nodeOption
        (br, brs) = bbrs
      } yield {

        val dropExt: TShell[SNesting[A]] =
          SNode(SNode(SNode(SDot(a), SLeaf), SNode(br, brs.asShell)), brs)

        MFix(MFix(cd.plug(cz.closeWith(dropExt))))

      }

    def loopLeafAt(ca: SAddr, la: SLeafAddr): Option[SCardNst[A]] = 
      for {
        t <- cnst.unfold
        lm <- t.leafSeek(la)
        zp <- lm.value.seekTo(ca)
        bbrs <- zp.focus.nodeOption
        (br, brs) = bbrs
      } yield {

        val extrusion : TShell[lm.T[STree[SNesting[A]]]] =
          SNode(SNode(SNode(lm.leaf, SLeaf), SNode(br, brs.asShell)), brs)

        MFix(lm.plug(zp.closeWith(extrusion)))

      }

    //============================================================================================
    // SPROUT
    //

    def sproutAt(addr: SCardAddr, a: A): Option[SCardNst[A]] = 
      for {
        pr <- cnst.mSeek(addr.branchAddr)
        (canopy, cd) = pr
        zp <- canopy.seekTo(addr.canopyAddr)
        nnsh <- zp.focus.nodeOption
        (n, nsh) = nnsh
        nz <- n.seek(addr.boxAddr)
        v <- nz.focus.dotOption
      } yield {

        val sproutShell : Shell[SNesting[A]] = 
          nz.ctxt.g match {
            case Nil => nsh.asShell
            case (_, SDeriv(sh, _)) :: _ => sh.asShell
          }

        val sproutExtrusion = SBox(v, SNode(SDot(a), sproutShell))
        cd.plug(zp.closeWith(SNode(nz.closeWith(sproutExtrusion), nsh)))

      }

    def sproutFillerAt(addr: SCardAddr, a: A): Option[(SCardNst[A], SAddr)] =
      for {
        t <- cnst.unfold
        pr <- t.mSeek(addr.branchAddr)
        (canopy, cd) = pr
        zp <- canopy.seekTo(addr.canopyAddr)
        ttsh <- zp.focus.nodeOption
        (tr, trsh) = ttsh
        na <- tr.spineToCanopyAddr(addr.boxAddr)
        cz <- tr.seekTo(na)
        _ <- cz.focus.leafOption
      } yield {

        val sproutShell : TShell[SNesting[A]] = 
          cz.ctxt.g match {
            case Nil => trsh.asShell
            case (_, SDeriv(sh, _)) :: _ => sh.asShell
          }

        val sproutExtrusion = SNode(SDot(a), SNode(SLeaf, sproutShell)) 
        (MFix(cd.plug(zp.closeWith(SNode(cz.closeWith(sproutExtrusion), trsh)))), na)

      }

    def sproutLeafAt(ca: SAddr, ba: SAddr, la: SLeafAddr): Option[SCardNst[A]] = 
      for {
        lm <- cnst.leafSeek(la)
        zp <- lm.value.seekTo(ca)
        ttsh <- zp.focus.nodeOption
        (t, tsh) = ttsh
        cz <- t.seekTo(ba)
        _ <- cz.focus.leafOption
      } yield {

        val sproutShell : QShell[lm.T[SNesting[A]]] = 
          cz.ctxt.g match {
            case Nil => tsh.asShell
            case (_, SDeriv(sh, _)) :: _ => sh.asShell
          }

        val extrusion : Shell[lm.T[SNesting[A]]] = 
          SNode(SLeaf, SNode(SLeaf, sproutShell))

        lm.plug(zp.closeWith(SNode(cz.closeWith(extrusion), tsh)))

      }

  }

  implicit class CardinalOps[A](card: SCardinal[A]) {

    def dim: Int = card.length - 1

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

    def seekCanopy(addr: SCardAddr): Option[SZipper[SNesting[A]]] = 
      card.take(addr.dim + 1).head.canopySeek(addr)

    def extend(a: A): SCardinal[A] = {
      val newHead = Traverse[MTree].map(card.head)(nst => nst.toTreeWith(_ => SDot(a)))
      card >> MFix(newHead)
    }

    def extrude(addr: SCardAddr, tgt: A, fill: A)(pred: A => Boolean): Option[(SCardinal[A], STree[Int])] = {

      val extDim = addr.dim
      val (p, l) = card.splitAt(extDim + 2)

      for {
        tl <- p.tail
        extNst = tl.head
        fillNst = p.head
        pr <- extNst.extrudeAt(addr, tgt)(pred)
        (en, msk) = pr
        fn <- fillNst.extrudeFillerAt(addr, fill)(msk)
        lns <- l.zipWithIndex.traverse({
          case (lfNst, i) => {

            val lfAddr = SLeafAddr(i, addr.branchAddr)
            lfNst.extrudeLeafAt(addr.canopyAddr, lfAddr, msk)

          }
        })
      } yield ((tl.withHead(en) >> fn) ++ lns, msk.map(_ => 0))

    }

    def extrudeWithMask[B](addr: SCardAddr, tgt: A, fill: A)(msk: STree[B]): Option[SCardinal[A]] = {

      val extDim = addr.dim
      val (p, l) = card.splitAt(extDim + 2)

      for {
        tl <- p.tail
        extNst = tl.head
        fillNst = p.head
        nn <- extNst.extrudeAtWithMask(addr, tgt)(msk)
        fn <- fillNst.extrudeFillerAt(addr, fill)(msk)
        lns <- l.zipWithIndex.traverse({
          case (lfNst, i) => {

            val lfAddr = SLeafAddr(i, addr.branchAddr)
            lfNst.extrudeLeafAt(addr.canopyAddr, lfAddr, msk)

          }
        })
      } yield (tl.withHead(nn) >> fn) ++ lns

    }

    def extrudeLoop(addr: SCardAddr, tgt: A, fill: A): Option[SCardinal[A]] = {

      val extDim = addr.dim
      val (p, l) = card.splitAt(extDim + 3)

      for {
        tl <- p.tail
        extNst = tl.head
        fillNst = p.head
        ln <- extNst.loopAt(addr, tgt)
        fn <- fillNst.loopFillerAt(addr, fill)
        lns <- l.zipWithIndex.traverse({
          case (lnst, i) => {

            // Not sure.  Maybe the leaf address is the same dimension,
            // but you append an extra step to the canopy address?
            val lfAddr = SLeafAddr(i, addr.branchAddr)
            lnst.loopLeafAt(addr.canopyAddr, lfAddr)

          }
        })
      } yield (tl.withHead(ln) >> fn) ++ lns

    }

    def sprout(addr: SCardAddr, src: A, fill: A): Option[SCardinal[A]] = {

      val extDim = addr.dim
      val (p, l) = card.splitAt(extDim + 2)

      for {
        tl <- p.tail
        extNst = tl.head
        fillNst = p.head
        sn <- extNst.sproutAt(addr, src)
        pr <- fillNst.sproutFillerAt(addr, fill)
        (fn, na) = pr
        lns <- l.zipWithIndex.traverse({
          case (lfNst, i) => {

            val lfAddr = SLeafAddr(i, addr.branchAddr)
            lfNst.sproutLeafAt(addr.canopyAddr, na, lfAddr)

          }
        })
      } yield (tl.withHead(sn) >> fn) ++ lns

    }

  }

  object SCardinal {

    def apply[A](a: A): SCardinal[A] = 
      ||(MObj(STree.obj(SDot(a)))) >> MFix(MObj(SLeaf))

    def apply[A](c: SComplex[A]): SCardinal[A] = 
      fromComplex(c)._1

    def apply[A](): SCardinal[Option[A]] = 
      ||(MObj(STree.obj(SDot(None))))

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

