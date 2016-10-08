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

    def matchWith[B](d: SComplex[B]): Option[SComplex[(A, B)]] =
      (c, d) match {
        case (||(cn), ||(dn)) => cn.matchWith(dn).map(pn => ||(pn))
        case (ctl >> chd, dtl >> dhd) =>
          for {
            tl <- ctl.matchWith(dtl)
            hd <- chd.matchWith(dhd)
          } yield tl >> hd
        case _ => None
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

    def isCFreeFace(fa: FaceAddr): Option[Boolean] = {

      val codim = fa.codim
      val addr = fa.address

      if (codim > 0) {

        c.grab(codim) match {
          case (fcmplx, nst :: _) => {

            def hasCofaceLoops(bnst: SNesting[Boolean]): Boolean =
              bnst match {
                case SDot(_) => false
                case SBox(b, SLeaf) => b
                case SBox(_, cn) => cn.map(hasCofaceLoops(_)).toList.exists(b => b)
              }

            for {
              zp <- fcmplx.head.seek(addr)
              // _ = println("Focus: " + zp.focus.toString)
              _ <- zp.focus.dotOption  // Make sure it's external
              bd <- SCmplxZipper(fcmplx).focusDeriv[Boolean]
              bn = fcmplx.head.mapWithAddr({
                case (a, baddr) => baddr == addr
              })
              // _ = println("Boolean coface nesting: " + bn.toString)
              cofaces <- nst.bondTraverse(bn, bd)({
                case (a, srcBs, tgtB) => {
                  Some(tgtB || srcBs.toList.exists(b => b))
                }
              })
              // _ = println("Cofaces: " + cofaces.toString)
            } yield (! hasCofaceLoops(cofaces))

          }
          case _ => None
        }

      } else None

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

    // Applies f to left for non-cofaces and right
    // for cofaces of those faces selected by the selection
    // function.  There is a lot of room for improvement
    // here. Many inefficiencies ....
    def traverseCofaces[B](codim: Int, sel: A => Boolean)(
      f: Either[A, A] => Option[B]
    ): Option[SComplex[B]] = {

      val (web, coweb) = c.grab(codim)

      val dispatch : A => Option[B] =
        (a: A) => if (sel(a)) f(Right(a)) else f(Left(a))

      // Map in the lower dimensions and the
      // head dimension, returning the seed boolean complex
      val (resWebOpt, bcmplx) =
        web match {
          case ||(hd) => (hd.traverse(dispatch).map(||(_)), ||(hd.map(sel)))
          case tl >> hd => {
            val rwo = 
              for {
                dt <- (tl : SComplex[A]).traverse(dispatch)
                dh <- hd.traverse(dispatch)
              } yield dt >> dh

            (rwo, (tl : SComplex[A]).map(_ => false) >> hd.map(sel))
          }
        }

      // Actually, we don't need the whole boolean complex because
      // we can annotate the focusDeriv call below with a type

      def traverseCoweb(cw: List[SNesting[A]], bc: SComplex[Boolean]): Option[List[SNesting[B]]] =
        cw match {
          case Nil => Some(Nil)
          case n :: ns => {
            for {
              bd <- SCmplxZipper(bc).focusDeriv[Boolean]
              newBnst <- n.bondTraverse(bc.head, bd)({
                case (a, srcBs, tgtB) => {
                  Some(tgtB || srcBs.toList.exists(b => b))
                }
              })  // Uh, yuck.  Two traversals instead of
                  // one because the match function is to specific
              prNst <- n.matchWith(newBnst)
              r <- prNst.traverse({
                case (a, true) => f(Right(a))
                case (a, false) => f(Left(a))
              })
              rs <- traverseCoweb(ns, bc >> newBnst)
            } yield r :: rs
          }
        }

      for {
        resWeb <- resWebOpt
        resCw <- traverseCoweb(coweb, bcmplx)
      } yield resWeb ++ resCw

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
            val objs = SBox(t, SNode(SBox(s, SNode(SDot(ext), SLeaf)), SLeaf))
            val arrs = SBox(back, SNode(SDot(a), SNode(SNode(SDot(glob), SNode(SLeaf, SLeaf)), SLeaf)))
            Some(||(objs) >> arrs)
          } else None
        }
        case tl >> frm >> SDot(a) => {
          for {
            zp <- frm.seek(SDir(addr) :: Nil)
            s <- zp.focus.dotOption
            pr <- zp.ctxt.g.headOption
            newFcs = SBox(s, SNode(SDot(ext), pr._2.sh.asShell))
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

    def focusDeriv[B]: Option[SDeriv[B]] =
      z match {
        case ||(_) => Some(SDeriv(SLeaf))
        case tl >> hd =>
          hd.focus match {
            case SDot(_) => 
              for {
                tc <- tl.focusCanopy
              } yield SDeriv(tc.asShell)
            case SBox(_, cn) => {

              // This is to avoid dropping all the way to the
              // object level for computing a derivative that
              // will often not be used.  But I haven't tested
              // if it actually works or not ...
              lazy val d = tl.focusDeriv[SNesting[A]]

              for {
                sp <- cn.spine
                dsh <- tl.focus.canopyWithGuide(sp, d)
              } yield SDeriv(dsh.asShell)
            }
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
    //  Source Extraction - extract the subcomplex of boxes
    //  *above* the focus using the guide tree
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

    //
    // Expansion
    //

    def expandWith(pd: STree[A], webOpt: Option[SComplex[A]]): Option[SComplex[A]] =
      z match {
        case ||(hd) =>
          pd match {
            case SNode(a, SLeaf) => hd.focus.dotOption.map(_ => ||(hd.withFocus(SDot(a)).close))
            case _ => None // In zero case, pasting diagram must be an object
          }
        case tl >> hd => {
          for {
            _ <- hd.focus.dotOption
            pr <- tl.focus.boxOption
            (bv, bcn) = pr
            web <- webOpt
            wnst <- web.head.toTree.treeFold({
              case addr => bcn.elementAt(addr)
            })({
              case (a, cn) => Some(SBox(a, cn))
            })

            nst <- hd.ctxt.g match {
              case Nil => None
              case (av, SDeriv(sh, gma)) :: prs => {
                for {
                  gres <- pd.map[SNesting[A]](SDot(_)).graftWith(sh)
                } yield SNstCtxt(prs).close(SBox(av, gma.close(gres)))
              }
            }

          } yield tl.withFocus(wnst).close >> nst
        }
      }

    //
    //  Contraction
    //

    def contractInCanopy(tgt: A, pd: STree[A], addr: SAddr): Option[SComplex[A]] =
      z match {
        case ||(hd) => Some(||(hd.withFocus(hd.focus.withBase(tgt)).close))  // Just swap!
        case tl >> hd => {
          for {
            tailZp <- tl.seek(addr)
            // The ugly cast is to avoid calculating the
            // derivative *three times*!
            d <- tailZp.focusDeriv[STree[SNesting[A]]] 
            dd = d.asInstanceOf[SDeriv[SNesting[A]]]
            ddd = d.asInstanceOf[SDeriv[STree[A]]]

            pr <- hd.focus.boxOption
            (a, cn) = pr

            cz <- cn.seekTo(addr)
            cut <- cz.focus.takeWithMask(pd, d)
            (exTr, cutSh) = cut

            newCn = cz.closeWith(SNode(SDot(tgt), cutSh))
            hdNst = hd.withFocus(SBox(a, newCn)).close

            gcut <- tailZp.focus.toTree.takeWithMask(pd, ddd)
            (gex, gsh) = gcut

            cmprsd <- tailZp.focus.compressWith(
              SNode(gex, (gsh : STree[STree[A]]).map(_.comultiply)), dd
            )

          } yield tailZp.withFocus(cmprsd).close >> hdNst
        }
      }

  }

  //============================================================================================
  // OPETOPIC SUBSTITUTION
  //

  sealed trait LeafMarker[A] {
    def addr: SAddr
    def deriv: SDeriv[LeafMarker[A]]
    def branch: Option[STree[SNesting[A]]]
    def attach(b: STree[SNesting[A]]): LeafMarker[A] 
  }

  case class BasicLeafMarker[A](
    val addr: SAddr,
    val deriv: SDeriv[LeafMarker[A]],
    val branch: Option[STree[SNesting[A]]] = None
  ) extends LeafMarker[A] {

    def attach(b: STree[SNesting[A]]): LeafMarker[A] =
      this.copy(branch = Some(b))

  }

  object LeafMarker {

    def apply[A](addr: SAddr, deriv: SDeriv[LeafMarker[A]]) : LeafMarker[A] =
      BasicLeafMarker(addr, deriv)

    def simpleJoin[A](mkSh : STree[LeafMarker[A]]) : Option[Shell[SNesting[A]]] =
      mkSh.traverse(_.branch)

  }

  def fixLeaves[A](guide: SNesting[LeafMarker[A]], nst: SNesting[A])(
    joinOp: STree[LeafMarker[A]] => Option[Shell[SNesting[A]]]
  ): Option[SNesting[A]] = {

    type Cn = STree[SNesting[A]]
    type Mp = Map[SAddr, SAddr]

    def mkMap[U, V](u: STree[U], v: STree[V]): Mp = {
      // val ul = u.toList
      // val vl = v.toList
      // println("U length " + ul.length.toString)
      // println("V length " + vl.length.toString)

      Map(u.addrTree.toList.zip(v.addrTree.toList): _*)
    }

    def vertical(n: SNesting[A], mk: LeafMarker[A]): Option[(SNesting[A], STree[LeafMarker[A]])] =
      n match {
        case SDot(a) => {
          // println("Vertical dot: " + a.toString)
          for {
            zp <- guide.seek(mk.addr)
            cn <- zp.focus match {
              case SDot(_) => None
              case SBox(_, cn) => Some(cn)
            }
          } yield (SDot(a), cn.map(_.baseValue))
        }
        case SBox(a, cn) => {
          // println("Vertical box: " + a.toString)
          for {
            hres <- horizontal(cn, mk)
            (newCn, dataTr) = hres
          } yield (SBox(a, newCn), dataTr)
        }
      }

    // Think of d as the derivative on the rising edge.
    def horizontal(t: STree[SNesting[A]], mk: LeafMarker[A]): Option[(Cn, STree[LeafMarker[A]])] = {

      def getLeaf[B](addr: SAddr, t: STree[B], m: Map[SAddr, SAddr]) : Option[B] =
        t.elementAt(m(addr)) match {
          case None => {
            println("Lookup failed for address: " + addr.toString)
            println("Tree was: " + t.toString)
            None
          }
          case Some(el) => {
            // println("Lookup success")
            Some(el)
          }
        }

      t match {
        case SLeaf => Some((SLeaf, mk.deriv.plug(mk)))
        case SNode(n, sh) => 
          for {
            vres <- vertical(n, mk)
            // _ = println("Finished vertical for box " + n.baseValue.toString)
            (ln, lsh) = vres
            fwdMp = mkMap(lsh, sh)
            bwkMp = fwdMp.map(_.swap)
            hres <- sh.traverseWithAddr({
              case (b, ad) => getLeaf(ad, lsh, bwkMp).flatMap(mk => horizontal(b, mk))
            })
            // _ = println("Finished horizontal for box " + n.baseValue.toString)
            lprs <- lsh.traverseWithAddr({
              case (mk, ad) => getLeaf(ad, hres, fwdMp).map({
                case (b, j) => (mk.attach(b), j)
              })
            })
            // _ = println("Crossed the bridge with pair calculation")
            (mkdSh, toJn) = STree.unzip(lprs)
            // _ = println("About to join ...")
            mkTr <- toJn.join
            // _ = println("About to call custom join operation")
            newSh <- joinOp(mkdSh)
            // _ = println("Done with box " + n.baseValue.toString)
          } yield (SNode(ln, newSh), mkTr)
      }
    }

    vertical(nst, guide.baseValue).map(_._1)

  }

  def fixAllLeaves[A](guideCmplx: SComplex[A], nsts: List[SNesting[A]]): Option[SComplex[A]] =
    nsts match {
      case Nil => Some(guideCmplx)
      case n :: ns => {
        for {
          bd <- SCmplxZipper(guideCmplx).focusDeriv[LeafMarker[A]]
          // _ = println("Got derivative")
          bmkNst = guideCmplx.head.mapWithData[LeafMarker[A], LeafMarker[A]](bd)(
            (_, ad, dr) => LeafMarker(ad, dr)
          )
          // _ = println("About to fixup")
          fixup <- fixLeaves(bmkNst, n)(LeafMarker.simpleJoin)
          // _ = println("Passing to next dimension")
          r <- fixAllLeaves(guideCmplx >> fixup, ns)
        } yield r
      }
    }

  def expandAt[A](c: SComplex[A], e: SComplex[A], fa: FaceAddr): Option[SComplex[A]] = {

    case class ExpandMarker(
      val addr: SAddr,
      val deriv: SDeriv[LeafMarker[A]],
      val branch: Option[STree[SNesting[A]]] = None,
      val pd: STree[STree[SNesting[A]]]
    ) extends LeafMarker[A] {

      def attach(b: STree[SNesting[A]]): LeafMarker[A] =
        this.copy(branch = Some(b))

    }

    def expandJoin(mkSh: STree[LeafMarker[A]]): Option[Shell[SNesting[A]]] =
      mkSh match {
        case SLeaf => Some(SLeaf)
        case SNode(b: BasicLeafMarker[A], sh) =>
          for {
            branch <- b.branch
            bsh <- sh.traverse(expandJoin(_))
          } yield SNode(branch, bsh)
        case SNode(e: ExpandMarker, sh) => {
          // println("Expanding!")
          for {
            bsh <- sh.traverse(expandJoin(_))
            r <- e.pd.graftWith(bsh)
          } yield r
        }
      }

    for {
      isCFree <- c.isCFreeFace(fa)
      if (isCFree) 
      face <- c.face(fa)
      etgt <- e.target
      _ <- face.matchWith(etgt)

      pr = c.grab(fa.codim)
      (base, upper) = pr

      frmNst <- e.tail.map(_.head)
      frmInfo <- e.cellFrame
      (pd, tgt) = frmInfo

      addr = fa.address
      bz = SCmplxZipper(base)
      zp <- bz.seek(addr)
      exBase <- zp.expandWith(pd, e.tail.flatMap(_.tail))

      // Here we create "guide", a nesting containing the markers
      // which we'll be using for fixing the leaves in the expansion
      bd <- bz.focusDeriv[LeafMarker[A]]
      bmkNst = base.head.mapWithData[LeafMarker[A], LeafMarker[A]](bd)(
        (_, ad, dr) => LeafMarker(ad, dr)
      )
      nz <- bmkNst.seek(addr)
      emk <- nz.focus.dotOption
      guide = nz.withFocus(SDot(ExpandMarker(emk.addr, emk.deriv, None, pd.map(_ => SLeaf)))).close

      firstUpper <- upper.headOption
      firstFixup <- fixLeaves(guide, firstUpper)(expandJoin)
      result <- fixAllLeaves(exBase >> firstFixup, upper.tail)

    } yield result

  }

  // The expand routine does a number of checks:
  //
  //  1) codomain freeness
  //  2) shape compatibility
  //
  // which I am going to put aside for a second while I design this ...

  def contractAt[A](c: SComplex[A], g: SComplex[A], fa: FaceAddr): Option[SComplex[A]] = {


    val codim = fa.codim
    val lclAddr = fa.address
    val (base, uppder) = c.grab(codim)

    for {

      tail <- base.tail
      frmInfo <- g.cellFrame
      (pd, tgt) = frmInfo

      // The leaf case will take special attention.
      // We ban it for the time being .....
      if (! pd.isLeaf)

      // tailDeriv <- SCmplxZipper(tail).focusDeriv[SAddr]
      addrNst = base.head.addrNesting
      cnpyAddr <- lclAddr.headOption
      baseAddr = lclAddr.tail

      zp <- addrNst.seek(baseAddr)
      bInfo <- zp.focus.boxOption
      (bv, cn) = bInfo

      cz <- cn.seekTo(cnpyAddr.dir)
      // BUG! You have to use a reasonable derivative here for the leaf case ...
      cut <- cz.focus.takeWithMask(pd, SDeriv(SLeaf))
      (cutPd, cutSh) = cut

      cfrTr <- cutPd.traverse((n : SNesting[SAddr]) =>
        n.dotOption.flatMap(ad => c.isCFreeFace(FaceAddr(codim, ad)))
      )
      _ <- if (cfrTr.toList.forall(b => b)) Some(()) else {
        println("There is a non-cfree face!")
        None
      }

      bzip <- SCmplxZipper(base).seek(baseAddr)
      result <- bzip.contractInCanopy(tgt, pd, cnpyAddr.dir)

      //def contractInCanopy(tgt: A, pd: STree[A], addr: SAddr): Option[SComplex[A]] =

      // So, the basic idea is that we need to find the pasting
      // diagram *in the spine* and then write over it with a guy.
      // Right, we're going to comultiply in the spine.  Compressing
      // the previous nesting with this guy should given the same thing
      // back.  As an intermediate step, we're going to write over that
      // part of the spine that we are going to be composing with the
      // pasting diagram.  Then we call compress and we should be
      // golden.

      // So, first step: locate the pasting diagram in the spine.

      // At this point, we should know that we have a compatible pasting
      // diagram of cfree faces.

      // Now, what's next?  We need to figure out how we're going to
      // "compress" the lower dimension and replace the pasting diagram
      // in the upper dimension with a single cell.

      // d <- SCmplxZipper(g).focusDeriv[STree[A]]
      // tz <- SCmplxZipper(base).seek(lclAddr)
      // ttl <- tz.tail  // We must have a tail?

      // test <- ttl.head.focus.compressWith(???, ???)

    } yield result

  }

  //============================================================================================
  // COMPLEX GRAFTING
  //

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

