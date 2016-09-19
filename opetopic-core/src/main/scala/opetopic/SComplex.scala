/**
  * SComplex.scala - Stable Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

trait ComplexTypes {

  sealed trait FaceAddr
  case class ThisDim(addr: SAddr) extends FaceAddr
  case class PrevDim(faddr: FaceAddr) extends FaceAddr

  object FaceAddr {

    def apply(i: Int, addr: SAddr): FaceAddr =
      if (i <= 0) ThisDim(addr) else PrevDim(FaceAddr(i-1, addr))

    implicit class FaceAddrOps(fa: FaceAddr) {

      def codim: Int =
        fa match {
          case ThisDim(_) => 0
          case PrevDim(p) => p.codim + 1
        }

      def address: SAddr =
        fa match {
          case ThisDim(a) => a
          case PrevDim(p) => p.address
        }

    }

  }

  type SComplex[+A] = Suite[SNesting[A]]
  type SCmplxZipper[+A] = Suite[SNstZipper[A]]

  object SCmplxZipper {
    def apply[A](c: SComplex[A]): SCmplxZipper[A] = 
      Traverse[Suite].map(c)(SNstZipper(_))
  }

  implicit object ComplexTraverse extends Traverse[SComplex] {

    def traverse[G[_], A, B](c: SComplex[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[SComplex[B]] =
      Traverse[Suite].traverse(c)(_.traverse(f))
    
  }

  implicit class SComplexOps[A](c: SComplex[A]) {

    def dim: Int = c.length - 1
    def topValue: A = c.head.baseValue

    def withTopValue(a: A): SComplex[A] =
      c match {
        case ||(hd) => ||(hd.withBase(a))
        case tl >> hd => tl >> hd.withBase(a)
      }

    //
    //  Traversals and maps
    //

    // This is only here because the complex traverse instance seems to have lower priority
    // that the suite instance.  You should see if you can fix this ...
    def traverseComplex[G[_], B](f: A => G[B])(implicit isAp: Applicative[G]): G[SComplex[B]] =
      ComplexTraverse.traverse(c)(f)

    def traverseWithAddr[G[_], B](f: (A, FaceAddr) => G[B], codim: Int = 0)(implicit isAp: Applicative[G]): G[SComplex[B]] = {

      import isAp._

      c match {
        case ||(nst) => ap(nst.traverseWithAddr((a, addr) => f(a, FaceAddr(codim, addr))))(pure(||(_)))
        case tl >> hd => {

          val newTl : G[SComplex[B]] = tl.traverseWithAddr(f, codim + 1)
          val newHd : G[SNesting[B]] = hd.traverseWithAddr((a, addr) => f(a, FaceAddr(codim, addr)))

          ap2(newTl, newHd)(pure(_ >> _))

        }
      }
    }

    def mapWithAddr[B](f: (A, FaceAddr) => B, codim: Int = 0): SComplex[B] =
      c match {
        case ||(hd) => hd.mapWithAddr((a, addr) => f(a, FaceAddr(codim, addr)))
        case tl >> hd => tl.mapWithAddr(f, codim + 1) >>
          hd.mapWithAddr((a, addr) => f(a, FaceAddr(codim, addr)))
      }

    //
    //  Source Calculation
    //

    def elementAt(fa: FaceAddr): Option[A] =
      fa match {
        case ThisDim(addr) => c.head.elementAt(addr)
        case PrevDim(pa) => c.tail.flatMap(_.elementAt(pa))
      }

    def sourceAt(addr: SAddr): Option[SComplex[A]] =
      for {
        z <- SCmplxZipper(c).seek(addr)
        f <- z.focusFace
      } yield f

    def face(fa: FaceAddr): Option[SComplex[A]] =
      fa match {
        case ThisDim(addr) => c.sourceAt(addr)
        case PrevDim(pa) => c.tail.flatMap(_.face(pa))
      }

    def isDrop: Boolean =
      c match {
        case _ >> SBox(_, SLeaf) >> SDot(_) => true
        case _ => false
      }

    def headSpine: Option[STree[A]] =
      c match {
        case ||(hd) =>
          for {
            a <- hd.firstDotValue
          } yield SNode(a, SLeaf)
        case tl >> SDot(a) =>
          for {
            sp <- tl.headSpine
          } yield SNode(a, sp.map(_ => SLeaf))
        case tl >> SBox(_, cn) => cn.spine
      }


    // This seems horribly inefficient
    def drops: List[FaceAddr] =
      comultiply.map(_.withFaceAddresses.toList.filter(_._1.isDrop).map(_._2)).getOrElse(List())

    def loops: List[FaceAddr] = {

      def getLoops(nst: SNesting[A], codim: Int, addr: SAddr = Nil): (List[FaceAddr], Boolean) =
        nst match {
          case SDot(_) => (List(), false)
          case SBox(_, SLeaf) => (List(FaceAddr(codim, addr)), true)
          case SBox(_, cn) => {

            val (childLoops, childIsLoop) = cn.mapWithAddr((cnst, dir) =>
              getLoops(cnst, codim, SDir(dir) :: addr)).toList.unzip

            if (childIsLoop.forall(b => b)) {
              (FaceAddr(codim, addr) :: childLoops.flatten, true)
            } else (childLoops.flatten, false)

          }
        }

      def loopsInner(cc: SComplex[A], codim: Int): List[FaceAddr] =
        cc match {
          case ||(hd) => getLoops(hd, codim)._1
          case tl >> hd => getLoops(hd, codim)._1 ++ loopsInner(tl, codim + 1)
        }

      loopsInner(c, 0)

    }

    // Get a face in a given dimension
    def face(d: Int)(addr: SAddr): Option[SComplex[A]] =
      c.face(FaceAddr(dim - d, addr))

    def comultiply: Option[SComplex[SComplex[A]]] =
      c.traverseWithAddr((_, fa) => c.face(fa))

    def addrComplex: SComplex[FaceAddr] =
      c.mapWithAddr((_, fa) => fa)

    def withFaceAddresses: SComplex[(A, FaceAddr)] =
      c.mapWithAddr((a, fa) => (a, fa))

    def target: Option[SComplex[A]] =
      c match {
        case ||(_) => None
        case tl >> _ => tl.sourceAt(Nil)
      }

    def glob(tgt: A, fill: A): Option[SComplex[A]] = 
      c match {
        case ||(n) => Some(||(SBox(tgt, STree.obj(n))) >> SDot(fill))
        case tl >> hd => 
          tl.head match {
            case SBox(_, cn) => Some(
              tl >> SBox(tgt, SNode(hd, cn.asShell)) >> SDot(fill)
            )
            case _ => None
          }
      }

    def foreach(op: A => Unit): Unit = 
      new Suite.SuiteOps(c).foreach((n: SNesting[A]) => n.foreach(op))

    def foreachWithAddr(op: (A, SAddr) => Unit): Unit = 
      new Suite.SuiteOps(c).foreach((n: SNesting[A]) => 
        n.foreachWithAddr(op)
      )

    //
    //  Extracting frames from complexes
    //

    def asFrame: Option[(STree[A], A)] = {
      for {
        (a, cn) <- c.head.boxOption
        srcTr <- cn.traverse(n => n.dotOption)
      } yield (srcTr, a)
    }

    def cellFrame: Option[(STree[A], A)] = 
      for {
        tl <- c.tail
        frm <- tl.asFrame
      } yield frm

    // Colorings

    def isColoredBy(cc: SComplex[FaceAddr]): Except[Unit] =
      validColoring(c, cc)

    // Target and source extensions

    def targetExtension(ext: A, glob: A, back: A): Option[SComplex[A]] =
      c match {
        case ||(SBox(t, SNode(SDot(s), SLeaf))) >> SDot(a) => {
          // Target extension of an arrow is a simplex
          val objs = SBox(ext, SNode(SBox(t, SNode(SDot(s), SLeaf)), SLeaf))
          val arrs = SBox(back, SNode(SDot(glob), SNode(SNode(SDot(a), SNode(SLeaf, SLeaf)), SLeaf)))
          Some(||(objs) >> arrs)
        }
        case tl >> frm >> SDot(a) =>
          for {
            // Higher case
            tlSp <- tl.headSpine
            newFrm = SBox(ext, SNode(frm, tlSp.map(_ => SLeaf)))
            newCanopy = newFrm.toTree.mapWithAddr((_, addr) =>
              if (addr == Nil) SDot(glob) else SDot(a)
            )
            newHd = SBox(back, newCanopy)
          } yield tl >> newFrm >> newHd 
        case _ => None
      }

    def sourceExtension(addr: SAddr)(ext: A, glob: A, back: A): Option[SComplex[A]] =
      c match {
        case ||(SBox(t, SNode(SDot(s), SLeaf))) >> SDot(a) => {
          if (addr == Nil) {
            // Target extension of an arrow is a simplex
            val objs = SBox(ext, SNode(SBox(t, SNode(SDot(s), SLeaf)), SLeaf))
            val arrs = SBox(back, SNode(SDot(glob), SNode(SNode(SDot(a), SNode(SLeaf, SLeaf)), SLeaf)))
            Some(||(objs) >> arrs)
          } else None
        }
        case tl >> frm >> SDot(a) => {
          for {
            zp <- frm.seek(SDir(addr) :: Nil)
            s <- zp.focus.dotOption
            pr <- zp.ctxt.g.headOption
            newFcs = SBox(s, SNode(SDot(ext), pr._2.sh))
            newFrm = zp.withFocus(newFcs).close
            newCanopy = newFrm.toTree.mapWithAddr((_, addr) =>
              if (addr == Nil) SDot(a) else SDot(glob)
            )
            newHd = SBox(back, newCanopy)
          } yield tl >> newFrm >> newHd 
        }
        case _ => None
      }

  }


  implicit class SCmplxZipperOps[A](z: SCmplxZipper[A]) {

    //
    //  Focus information
    //

    def focus: SNesting[A] = 
      z.head.focus

    def withFocus(n: SNesting[A]): SCmplxZipper[A] = 
      z match {
        case ||(hd) => ||(hd.withFocus(n))
        case tl >> hd => tl >> hd.withFocus(n)
      }

    // This does a bunch of extra work calculating
    // spines for derivatives which don't get used.
    // You should use laziness here to avoid all the
    // extra work.
    def focusDeriv[B]: Option[SDeriv[B]] =
      z match {
        case ||(_) => Some(SDeriv(SLeaf))
        case tl >> hd =>
          hd.focus match {
            case SDot(_) => 
              for {
                tc <- tl.focusCanopy
              } yield SDeriv(tc.asShell)
            case SBox(_, cn) => 
              for {
                sp <- cn.spine
                d <- tl.focusDeriv[SNesting[A]]
                dsh <- tl.focus.canopyWithGuide(sp, d)
              } yield SDeriv(dsh.asShell)
          }
      }

    def focusCanopy: Option[STree[SNesting[A]]] = 
      z.focus match {
        case SDot(_) => None
        case SBox(_, cn) => Some(cn)
      }

    // The spine starting from the current focus
    def focusSpine: Option[STree[A]] =
      focus match {
        case SDot(a) => focusDeriv[A].map(_.plug(a))
        case SBox(a, cn) => cn.spine
      }

    def focusFace: Option[SComplex[A]] = 
      z match {
        case ||(hd) => Some(||(SDot(hd.focus.baseValue)))
        case tl >> hd => 
          for {
            sp <- focusSpine
            d <- focusDeriv[STree[A]]
            c <- tl.extract(sp)
            dd <- SCmplxZipper(c).focusDeriv[SNesting[A]]
            chd <- c.head.compressWith(d.plug(sp), dd)
          } yield c.withHead(chd) >> SDot(hd.focus.baseValue)
      }

    //
    //  Basic Zipper Ops
    // 

    def close: SComplex[A] = 
      Traverse[Suite].map(z)(_.close)

    def visit(dir: SDir): Option[SCmplxZipper[A]] = 
      (z, dir) match {
        case (||(hd), d) => 
          for {
            zp <- hd.visit(dir)
          } yield ||(zp)
        case (tl >> hd, SDir(Nil)) => 
          for {
            zp <- hd.visit(dir)
          } yield tl >> zp
        case (tl >> hd, SDir(d :: ds)) => 
          for {
            zp <- z.visit(SDir(ds))
            hz <- zp.head.sibling(d)
            sp <- zp.focusSpine
            res <- sp match {
              case SLeaf => zp.tail.map(_ >> hz)
              case SNode(a, sh) => 
                for {
                  extents <- sh.extents
                  addr <- extents.elementAt(d.dir)
                  ntl <- zp.tail
                  ztl <- ntl.seek(addr)
                } yield ztl >> hz
            }
          } yield res
      }

    def seek(addr: SAddr): Option[SCmplxZipper[A]] = 
      addr match {
        case Nil => Some(z)
        case d :: ds =>
          for {
            zp <- z.seek(ds)
            zzp <- zp.visit(d)
          } yield zzp
      }

    //
    //  Source Extraction
    //

    def extract[B](guide: STree[B]): Option[SComplex[A]] = 
      z match {
        case ||(hd) => 
          for {
            pr <- hd.focus.exciseWith(guide, SDeriv(SLeaf))
          } yield ||(pr._1)
        case tl >> hd => 
          for {
            d <- focusDeriv[SNesting[A]]
            pr <- hd.focus.exciseWith(guide, d)
            (excised, boxTr) = pr
            (localSpine, compressor) = boxTr.treeSplit({
              case SDot(a) => ??? // An error ...
              case SBox(a, cn) => (a, cn)
            })
            sp <- compressor.join
            c <- tl.extract(sp)
            dd <- SCmplxZipper(c).focusDeriv[SNesting[A]]
            chd <- c.head.compressWith(compressor, dd)
          } yield c.withHead(chd) >> excised
      }

  }

  //============================================================================================
  // COMPLEX GRAFTING
  //

  // Uh, yeah, I haven't really tested these routines out yet.  I just translated from the
  // unstable version so they would be here for use in the type checker.  But you should
  // do some kind of test to make sure they're working as you would expect ...
  def graft[A](pd: STree[SComplex[A]])(disc: (A, A) => Option[A]): Option[(SComplex[A], STree[SNesting[A]])] = 
    for {
      pr <- pd.traverse({ 
        case ||(_) => None
        case tl >> hd => Some(tl, hd)
      })
      (cmplxTr, pdTr) = STree.unzip(pr)
      cmplxNst <- cmplxTr.toNesting({
        case Nil => None // Should have been a leaf? (I don't know what this means ...)
        case d :: ds => 
          for {
            zp <- cmplxTr.seekTo(ds)
            c <- zp.focus.rootValue
            cnst <- c.sourceAt(d :: Nil)
          } yield cnst
      })
      gres <- graftNesting(cmplxNst)(disc)
    } yield (gres, pdTr)


  // Uggh.  I hate this name, but I can't think of anything which is much better ...
  def graftNesting[A](nst: SNesting[SComplex[A]])(disc: (A, A) => Option[A]): Option[SComplex[A]] = 
    nst match {
      case SDot(c) => Some(c)
      case SBox(||(SBox(tgt, SNode(SDot(src), _))), SNode(pd, _)) => {

        // This should mean we are grafting a bunch of arrows, I think.

        for {
          grft <- graftNesting(pd)(disc)
          objNesting = grft.head
          v <- disc(src, objNesting.baseValue)
        } yield ||(SBox(tgt, STree.obj(objNesting.withBase(v))))

      }
      case SBox(c >> SBox(tgt, tcn), cn) => {

        // Blech.  A bit rough, no?

        for {
          cmplxSh <- cn.traverse(graftNesting(_)(disc))
          prTr <- tcn.matchTraverse(cmplxSh)({
            case (SDot(src), c0 >> nst) => {
              for {
                v <- disc(src, nst.baseValue)
              } yield (nst.withBase(v), c0.head)
            }
            case _ => None // Malformed result
          })
          (newCnpy, fillerTr) = STree.unzip(prTr)
          nstNst <- fillerTr.toNesting((addr: SAddr) => {
            for { zp <- c.head.seek(addr) } yield zp.focus
          })
          cdim2 <- SNesting.join(nstNst)
        } yield c.withHead(cdim2) >> SBox(tgt, newCnpy)

      }
      case _ => None // Malformed grafting problem ...
    }


  //============================================================================================
  // PICKLING
  //

  import upickle.Js
  import upickle.default._

  def complexToJson[A](c: SComplex[A])(implicit w: Writer[A]): String =
    upickle.json.write(Suite.suiteWriter(SNesting.nestingWriter(w)).write(c))

  def complexFromJson[A](c: Js.Value)(implicit r: Reader[A]): SComplex[A] = 
    Suite.suiteReader(SNesting.nestingReader(r)).read(c)

  //============================================================================================
  // COLOR CHECKING
  //

  sealed trait ColorMarker
  case class AddrMarker(val fa: FaceAddr) extends ColorMarker
  case class LoopMarker(val src: FaceAddr, val tgt: FaceAddr) extends ColorMarker

  def validColoring[A](c: SComplex[A], cc: SComplex[FaceAddr]): Except[Unit] =
    checkColoring(c, cc.map(AddrMarker(_)))
  
  def checkColoring[A](c: SComplex[A], cc: SComplex[ColorMarker]): Except[Unit] = {

    val coloringDim = c.dim
    val coloredDim = cc.dim

    // println("Coloring dim: " + coloringDim)
    // println("Colored dim: " + coloredDim)

    def ok: Except[Unit] = Xor.Right(())

    cc.head.dotOption match {
      case Some(AddrMarker(headColor)) => {

        if (headColor.codim > 0) {

          println("Recoloring!")

          import scala.collection.mutable.HashMap

          val addrMap : HashMap[FaceAddr, ColorMarker] = HashMap()

          for {
            coloringFace <- attempt(c.addrComplex.face(headColor), "Failed to calculate face in recoloring!")
            _ <- coloringFace.traverseWithAddr((g, l) => {
              if (addrMap.isDefinedAt(g)) {
                addrMap(g) match {
                  case AddrMarker(fa) => { addrMap(g) = LoopMarker(fa, l) ; ok } // Check the order here!
                  case _ => throwError("Duplicate loop marker!")
                }
              } else { addrMap(g) = AddrMarker(l) ; ok }
            })
            recoloring <- cc.traverse({
              case AddrMarker(fa) => attempt(addrMap.get(fa), "Unrecognized face in coloring!")
              case LoopMarker(src, tgt) => {
                (addrMap.get(src), addrMap.get(tgt)) match {
                  case (Some(AddrMarker(sf)), Some(AddrMarker(tf))) => Xor.Right(LoopMarker(sf, tf))
                  case (Some(AddrMarker(sf)), None) => Xor.Right(AddrMarker(sf))
                  case (None, Some(AddrMarker(tf))) => Xor.Right(AddrMarker(tf))
                  case (_, _) => Xor.Left("Error in conflict resolution.")
                }
              }
            })
            valid <- checkColoring(coloringFace, recoloring)
          } yield valid

        } else {

          if (coloredDim == 0) {

            println("Object case.")

            cc match {
              case ||(SDot(AddrMarker(ThisDim(Nil)))) => ok
              case _ => throwError("Invalid object coloring.")
            }

          } else if (coloringDim == coloredDim) {

            println("Equality case.")

            for {
              frmData <- attempt(cc.cellFrame, "Failed to extract equidimensional frame.")
              (srcs, tgtColor) = frmData
              _ <- if (headColor == ThisDim(Nil)) ok else throwError("Top cell not colored by an identity")
              valid <- tgtColor match {
                case AddrMarker(PrevDim(ThisDim(Nil))) => {

                  // The target is not decorated by a conflict, so check as usual.
                  for {
                    _ <- srcs.traverseWithAddr((srcColor, addr) =>
                      for {
                        _ <- if (srcColor != AddrMarker(PrevDim(ThisDim(Nil)))) ok else throwError("Source was decorated by a codomain")
                        coloredFace <- attempt(cc.face(coloredDim - 1)(SDir(addr) :: Nil), "Failed to get equidimensional colored source face")
                        valid <- checkColoring(c, coloredFace)
                      } yield valid
                    )
                    coloredTgt <- attempt(cc.target, "Failed to get equidimensional colored target")
                    valid <- checkColoring(c, coloredTgt)
                  } yield valid

                }
                case AddrMarker(_) => throwError("Target cell not colored by codomain: " + tgtColor.toString)
                case LoopMarker(src, tgt) => {

                  // What to do here?  Extract the *unique* source and check the source and target when
                  // colored by the source and target addresses we see here.

                  for {
                    _ <- srcs.toList match {
                      case LoopMarker(s, t) :: Nil =>
                        if (s == src && t == tgt) {
                          for {
                            srcFace <- attempt(cc.face(PrevDim(ThisDim(SDir(Nil) :: Nil))), "Failed to calculate source face in loop case")
                            valid <- checkColoring(c, srcFace.withTopValue(AddrMarker(src)))
                          } yield valid
                        } else throwError("Loop marker mismatch!")
                      case _ => throwError("Malformed source in loop case")
                    }
                    tgtFace <- attempt(cc.target, "Failed to calculate target in loop case")
                    valid <- checkColoring(c, tgtFace.withTopValue(AddrMarker(tgt)))
                  } yield valid

                }
              }
            } yield valid

          } else if (coloringDim < coloredDim) {

            println("Degenerate case.")

            // println("Coloring complex: " + c.toString)
            // println("Colored complex: " + cc.toString)

            for {
              _ <- if (headColor != ThisDim(Nil)) throwError("Top cell not colored by an identity") else ok
              frmData <- attempt(cc.cellFrame, "Failed to extract degenerate frame.")
              (srcs, tgtColor) = frmData
              _ <- srcs.traverseWithAddr((srcColor, addr) => {
                for {
                  coloredFace <- attempt(cc.face(coloredDim - 1)(SDir(addr) :: Nil), "Failed to get degenerate colored source face")
                  valid <- checkColoring(c, coloredFace)
                } yield valid
              })
              coloredTgt <- attempt(cc.target, "Failed to get degenerate colored target")
              _ <- if (tgtColor == AddrMarker(ThisDim(Nil))) ok else throwError("Degenerate target is not colored by id")
              valid <- checkColoring(c, coloredTgt)
            } yield valid

          } else throwError("Coloring dimension is too high!")


        }
      }
      case Some(_) => throwError("Top coloring is a loop!")
      case None => throwError("Complex is not a cell!")
    }

  }

}
