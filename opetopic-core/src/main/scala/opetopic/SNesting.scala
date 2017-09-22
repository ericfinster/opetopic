/**
  * SNesting.scala - Stable Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

sealed trait SNesting[+A] 
case class SDot[+A](a: A) extends SNesting[A]
case class SBox[+A](a: A, cn: STree[SNesting[A]]) extends SNesting[A]

case class SNstDeriv[+A](c: STree[SNesting[A]], g: SNstCtxt[A]) {

  def plug[B >: A](b: B): SNesting[B] = 
    g.close(SBox(b, c))

}

case class SNstCtxt[+A](val g: List[(A, SDeriv[SNesting[A]])]) {

  def close[B >: A](nst: SNesting[B]): SNesting[B] = 
    g match {
      case Nil => nst
      case (a, d) :: gs =>
        SNstCtxt(gs).close(SBox(a, d.plug(nst)))
    }

  def ::[B >: A](pr: (B, SDeriv[SNesting[B]])): SNstCtxt[B] = 
    SNstCtxt(pr :: g)

  def address: SAddr =
    g match {
      case Nil => Nil
      case (_,d) :: ds => SDir(d.g.address) :: SNstCtxt(ds).address
    }

}

case class SNstZipper[+A](val focus: SNesting[A], val ctxt: SNstCtxt[A] = SNstCtxt(Nil)) {

  def withFocus[B >: A](f: SNesting[B]): SNstZipper[B] = 
    SNstZipper(f, ctxt)

  def close: SNesting[A] = 
    ctxt.close(focus)

  def closeWith[B >: A](n: SNesting[B]): SNesting[B] = 
    ctxt.close(n)

  def address: SAddr =
    ctxt.address

  def visit(d: SDir): Option[SNstZipper[A]] =
    (focus, d) match {
      case (SDot(_), _) => None
      case (SBox(a, cn), SDir(ds)) =>
        for {
          z <- cn.seekTo(ds)
          r <- z match {
            case SZipper(SLeaf, _) => None
            case SZipper(SNode(n, sh), g) =>
              Some(SNstZipper(n, (a, SDeriv(sh, g)) :: ctxt))
          }
        } yield r
    }

  def seek(a: List[SDir]): Option[SNstZipper[A]] = 
    a match {
      case Nil => Some(this)
      case d :: ds => 
        for {
          z <- seek(ds)
          zz <- z.visit(d)
        } yield zz
    }

  def sibling(dir: SDir): Option[SNstZipper[A]] = 
    ctxt.g match {
      case Nil => None
      case (a, SDeriv(vs, hcn)) :: cs => 
        for {
          vzip <- vs.seekTo(dir.dir)
          res <- vzip.focus match {
            case SLeaf => None
            case SNode(SLeaf, _) => None
            case SNode(SNode(nfcs, vrem), hmask) => 
              Some(SNstZipper(nfcs, SNstCtxt((a, SDeriv(vrem, (focus, SDeriv(hmask, vzip.ctxt)) :: hcn)) :: cs)))
          }
        } yield res
    }

  def hasParent: Boolean =
    ctxt.g != Nil

  def parent: Option[SNstZipper[A]] =
    ctxt.g match {
      case Nil => None
      case (a, d) :: cs => Some(SNstZipper(SBox(a, d.plug(focus)), SNstCtxt(cs)))
    }

  def predecessor: Option[SNstZipper[A]] =
    ctxt.g match {
      case (a, SDeriv(verts, SCtxt((pred, deriv) :: vs))) :: cs => 
        Some(SNstZipper(pred, SNstCtxt((a, SDeriv(deriv.plug(SNode(focus, verts)), SCtxt(vs))) :: cs)))
      case _ => None
    }

  def predecessorWhich(f: A => Boolean): Option[SNstZipper[A]] = 
    if (f(focus.baseValue)) Some(this) else
      for {
        pred <- predecessor
        res <- pred.predecessorWhich(f)
      } yield res

}


object SNesting {

  implicit object NestingTraverse extends Traverse[SNesting] {
    
    def traverse[G[_], A, B](n: SNesting[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[SNesting[B]] = 
      n.lazyTraverse[G, Unit, B](f)
    
  }

  implicit class SNestingOps[A](nst: SNesting[A]) {

    // Okay, I don't really know about the default derivative used here ....
    def lazyTraverse[G[_], B, C](
      f: LazyTraverse[G, A, B, C],
      addr: => SAddr = Nil, 
      deriv: => SDeriv[B] = SDeriv(SNode(SLeaf, SLeaf))
    )(implicit isAp: Applicative[G]): G[SNesting[C]] =
      nst match {
        case SDot(a) => isAp.ap(f(a, addr, deriv))(isAp.pure(SDot(_)))
        case SBox(a, cn) => {

          import isAp._

          lazy val gc: G[C] = f(a, addr, deriv)
          lazy val gcn: G[STree[SNesting[C]]] =
            cn.traverseWithData[G, B, SNesting[C]](
              (n, dir, drv) => {
                lazy val eaddr = SDir(dir) :: addr
                n.lazyTraverse(f, eaddr, drv)
              }
            )

          ap2(gc, gcn)(pure(SBox(_, _)))

        }
      }

    def mapWithAddr[B](f: (A, => SAddr) => B): SNesting[B] = 
      lazyTraverse(funcAddrToLt[Id, A, B](f))

    def mapWithDeriv[B, C](d: SDeriv[B])(f: (A, => SDeriv[B]) => C) : SNesting[C] =
      lazyTraverse(funcDerivToLt[Id, A, B, C](f), Nil, d)

    def mapWithData[B, C](d: SDeriv[B])(f: (A, => SAddr, => SDeriv[B]) => C) : SNesting[C] =
      lazyTraverse(funcAddrDerivToLt[Id, A, B, C](f), Nil, d)

    def addrNesting: SNesting[SAddr] =
      nst.mapWithAddr({ case (a, addr) => addr })

    def traverseWithAddr[G[_], B](f: (A, => SAddr) => G[B])(implicit isAp: Applicative[G]): G[SNesting[B]] =
      lazyTraverse(funcAddrToLt[G, A, B](f))

    def foldNesting[B](dot: A => B)(box: (A, STree[B]) => B) : B =
      nst match {
        case SDot(a) => dot(a)
        case SBox(a, cn) => box(a, cn.map(_.foldNesting(dot)(box)))
      }

    def foldNestingWithAddr[B](addr: SAddr = Nil)(dot: (A, SAddr) => B)(box: (A, SAddr, STree[B]) => B) : B = 
      nst match {
        case SDot(a) => dot(a, addr)
        case SBox(a, cn) => box(a, addr, cn.mapWithAddr(
          (nn, dir) => nn.foldNestingWithAddr(SDir(dir) :: addr)(dot)(box)
        ))
      }

    def matchWith[B](n: SNesting[B]): Option[SNesting[(A, B)]] =
      (nst, n) match {
        case (SDot(a), SDot(b)) => Some(SDot((a, b)))
        case (SBox(a, acn), SBox(b, bcn)) =>
          for {
            abcn <- acn.matchWith(bcn)
            rcn <- abcn.traverse({
              case (x, y) => x.matchWith(y)
            })
          } yield SBox((a, b), rcn)
        case _ => None
      }

    def boxOption: Option[(A, STree[SNesting[A]])] =
      nst match {
        case SBox(a, cn) => Some(a, cn)
        case _ => None
      }

    def dotOption: Option[A] = 
      nst match {
        case SDot(a) => Some(a)
        case _ => None
      }

    def isDot: Boolean =
      dotOption.isDefined

    def isBox: Boolean =
      boxOption.isDefined

    def isLoop: Boolean =
      nst match {
        case SDot(_) => false
        case SBox(_, SLeaf) => true
        case SBox(_, cn) => cn.toList.forall(_.isLoop)
      }

    def toTree: STree[A] =
      foldNesting[STree[A]](a => SLeaf)((a, sh) => SNode(a, sh))

    def toTreeWith[B](f: A => B): STree[B] = 
      foldNesting[STree[B]](a => SLeaf)((a, sh) => SNode(f(a), sh))

    def comultiply: SNesting[SNesting[A]] =
      nst match {
        case SDot(a) => SDot(SDot(a))
        case SBox(a, cn) => {
          val lcl = SBox(a, cn.map(ln => SDot(ln.baseValue)))
          SBox(lcl, cn.map(_.comultiply))
        }
      }

    def foreach(op: A => Unit): Unit =
      nst match {
        case SDot(a) => op(a)
        case SBox(a, cn) => {
          for { n <- cn } { n.foreach(op) }
          op(a)
        }
      }

    def foreachWithAddr(op: (A, SAddr) => Unit, addr: SAddr = Nil): Unit = 
      nst match {
        case SDot(a) => op(a, addr)
        case SBox(a, cn) => {
          cn.foreachWithAddr((n, d) => 
            n.foreachWithAddr(op, SDir(d) :: addr)
          )
          op(a, addr)
        }
      }

    def seek(addr: SAddr) : Option[SNstZipper[A]] = 
      SNstZipper(nst).seek(addr)

    def replaceAt(addr: SAddr, a: A): Option[SNesting[A]] =
      for {
        zp <- nst.seek(addr)
      } yield zp.withFocus(zp.focus.withBase(a)).close

    def elementAt(addr: SAddr): Option[A] =
      for {
        zp <- nst.seek(addr)
      } yield zp.focus.baseValue
    
    def baseValue: A =
      nst match {
        case SDot(a) => a
        case SBox(a, _) => a
      }

    def firstDotValue: Option[A] =
      nst match {
        case SDot(a) => Some(a)
        case SBox(_, cn) =>
          cn match {
            case SLeaf => None
            case SNode(n, _) => n.firstDotValue
          }
      }

    def withBase(a: A): SNesting[A] =
      nst match {
        case SDot(_) => SDot(a)
        case SBox(_, cn) => SBox(a, cn)
      }

    def splitWith[B, C](f: A => (B, C)): (SNesting[B], SNesting[C]) =
      nst match {
        case SDot(a) => {
          val (b, c) = f(a)
          (SDot(b), SDot(c))
        }
        case SBox(a, cn) => {
          val (b, c) = f(a)
          val (bcn, ccn) = STree.unzip(cn.map(_.splitWith(f)))
          (SBox(b, bcn), SBox(c, ccn))
        }
      }

    def spine(d: SDeriv[A]): Option[STree[A]] = 
      nst match {
        case SDot(a) => Some(d.plug(a))
        case SBox(a, cn) => cn.spine
      }

    // Total canopy?
    def canopy(d: SDeriv[SNesting[A]]): Option[STree[SNesting[A]]] = 
      nst match {
        case SDot(a) => Some(d.plug(SDot(a)))
        case SBox(a, cn) => cn.canopy
      }

    def canopyWithGuide[B](g: STree[B], dOpt: => Option[SDeriv[SNesting[A]]]): Option[STree[SNesting[A]]] = 
      (nst, g) match {
        case (_, SLeaf) => dOpt.map(d => d.plug(nst))
        case (SBox(_, cn), SNode(_, sh)) => 
          for {
            toJn <- cn.matchWithDeriv(sh)({
              case (nn, gg, dd) => nn.canopyWithGuide(gg, Some(dd))
            })
            jn <- toJn.join
          } yield jn
        case _ => { println("bad canopy") ; None }
      }

    // Follow the guide tree, extracting the boxes which match it.
    // Return the resulting nesting, as well as simultaneously calculating
    // the spine which remains above the crop point.
    def exciseWith[B](tr: STree[B], d: SDeriv[SNesting[A]]): Option[(SNesting[A], STree[SNesting[A]])] = 
      (nst, tr) match {
        case (_, SLeaf) => {
          for {
            cn <- nst.canopy(d)
            v = nst.baseValue
          } yield (SDot(v), d.plug(SBox(v, cn)))
        }
        case (SBox(a, cn), SNode(_, sh)) => {
          for {
            pr <- cn.matchWithDeriv(sh)({
              case (nn, tt, dd) => nn.exciseWith(tt, dd)
            })
            (ncn, toJn) = STree.unzip(pr)
            jn <- toJn.join
          } yield (SBox(a, ncn), jn)
        }
        case (SDot(_), SNode(_, _)) => { println("bad excise") ; None }
      }

    def compressWith[B](sh: Shell[B], d: SDeriv[SNesting[A]]): Option[SNesting[A]] = 
      sh match {
        case SNode(SLeaf, sh) => 
          for {
            root <- sh.rootValue
            nn <- nst.compressWith(root, d) // Use the same derivative?
          } yield SBox(nst.baseValue, d.plug(nn))
        case SNode(sk, sh) => 
          for {
            cn <- nst.canopyWithGuide(sk, Some(d))
            nnn <- cn.matchWithDeriv(sh)({
              case (nn, gg, dd) => nn.compressWith(gg, dd)
            })
          } yield SBox(nst.baseValue, nnn)
        case SLeaf => Some(nst)
      }

    //
    // Nesting Fold
    //

    def nestingFold[B](dotRec: (SAddr, SAddr, A) => Option[B])(boxRec: (SAddr, A, STree[B]) => Option[B]): Option[B] = {

      def foldPass(canopy : STree[SNesting[A]], spineAddr: SAddr, addrDeriv: SDeriv[SAddr])(lclAddr: SAddr, vertAddr: SAddr): Option[(STree[B], STree[SAddr])] =
        canopy match {
          case SLeaf => Some(SLeaf, addrDeriv.plug(spineAddr))
          case SNode(SDot(a), sh) => {

            for {
              hr <- sh.traverseWithData[Option, SAddr, (STree[B], STree[SAddr])]({
                (hbr, dir, deriv) => foldPass(hbr, SDir(dir) :: spineAddr, deriv)(SDir(dir) :: lclAddr, vertAddr)
              })
              (bs, adJn) = STree.unzip(hr) 
              adTr <- STree.join(adJn)
              b <- dotRec(spineAddr, SDir(lclAddr) :: vertAddr, a) 
            } yield (SNode(b, bs), adTr)

          }
          case SNode(SBox(a, cn), sh) => {

            for {
              pr <- foldPass(cn, spineAddr, addrDeriv)(Nil, SDir(lclAddr) :: vertAddr)
              (bTr, adOutTr) = pr
              cr <- sh.matchWithDeriv[SAddr, SAddr, (STree[B], STree[SAddr])](adOutTr)({
                (hbr, dir, deriv) => foldPass(hbr, dir, deriv)(SDir(dir) :: lclAddr, vertAddr)
              })
              (bs, adJn) = STree.unzip(cr)
              adTr <- STree.join(adJn)
              b <- boxRec(SDir(lclAddr) :: vertAddr, a, bTr)
            } yield (SNode(b, bs), adTr)

          }
        }

      def horizInit(a: A, h: STree[STree[SNesting[A]]], vertAddr: SAddr)(initCalc: Option[(B, STree[SAddr])]): Option[(B, STree[SAddr])] =
        for {
          pr <- initCalc
          (b, adOutTr) = pr
          hr <- h.matchWithDeriv[SAddr, SAddr, (STree[B], STree[SAddr])](adOutTr)(foldPass(_, _, _)(Nil, vertAddr))
          (bs, adJn) = STree.unzip(hr)
          adTr <- STree.join(adJn)
          b <- boxRec(vertAddr, a, SNode(b, bs))
        } yield (b, adTr)
      
      def vertInit(a: A, n: SNesting[A], h: STree[STree[SNesting[A]]], vertAddr: SAddr): Option[(B, STree[SAddr])] =
        n match {
          case SDot(aa) => horizInit(a, h, vertAddr)(
            for {
              b <- dotRec(Nil, vertAddr, aa)
            } yield (b, h.mapWithAddr((_, d) => SDir(d) :: Nil))
          )
          case SBox(aa, SLeaf) => horizInit(a, h, vertAddr)(
            for {
              b <- boxRec(vertAddr, aa, SLeaf)
            } yield (b, h.map(_ => Nil))
          )
          case SBox(aa, SNode(nn, hh)) =>
            horizInit(a, h, vertAddr)(vertInit(aa, nn, hh, SDir(Nil) :: vertAddr))
        }

      nst match {
        case SDot(a) => dotRec(Nil, Nil, a)
        case SBox(a, SLeaf) => boxRec(Nil, a, SLeaf)
        case SBox(a, SNode(n, sh)) => vertInit(a, n, sh, Nil).map(_._1)
      }

    }


    // Okay, despite the implementation of the nesting fold function, the
    // bond traversal barely improved.

    //
    //  Bond traverse: traverse with a nesting which is bonded with
    //  this one.  I believe the layout routine could be rewritten
    //  using this idea....
    // 

    def bondTraverse[B, C](web: SNesting[B], webD: SDeriv[B])(f: (A, STree[B], B) => Option[C]): Option[SNesting[C]] = {

      def bondPass(n: SNesting[(A, SAddr)], lvs: STree[B]): Option[(SNesting[C], B)] =
        n match {
          case SDot((a, addr)) => {
            for {
              b <- web.elementAt(addr)
              c <- f(a, lvs, b)
            } yield (SDot(c), b)
          }
          case SBox((a, _), cn) => {
            for {
              pr <- cn.treeFold[(STree[SNesting[C]], B)]({
                case extAddr => for {
                  b <- lvs.elementAt(extAddr)
                } yield (SLeaf, b)
              })({
                case (ln, horiz) => {

                  val (hcn, bTr) = STree.unzip(horiz)

                  for {
                    lRes <- bondPass(ln, bTr)
                    (thisNst, outB) = lRes
                  } yield (SNode(thisNst, hcn), outB)

                }
              })
              (newCn, outB) = pr
              c <- f(a, lvs, outB)
            } yield (SBox(c, newCn), outB)

          }
        }

      for {
        nstWithAddr <- nst.nestingFold[SNesting[(A, SAddr)]](
          (spineAddr, vertAddr, a) => Some(SDot(a, spineAddr))
        )(
          (vertAddr, a, cn) => Some(SBox((a, Nil), cn))
        )
        webSpine <- web.spine(webD)
        res <- bondPass(nstWithAddr, webSpine)
      } yield res._1

    }

  }

  implicit class SCanopyOps[A](cn: STree[SNesting[A]]) {

    def spine: Option[STree[A]] = 
      cn.traverseWithData[Option, A, STree[A]]({
        case (nst, _, deriv) => nst.spine(deriv)
      }).flatMap(STree.join(_))

    def canopy: Option[STree[SNesting[A]]] = 
      cn.traverseWithData[Option, SNesting[A], STree[SNesting[A]]]({
        case (nst, _, deriv) => nst.canopy(deriv)
      }).flatMap(STree.join(_))


    // I believe this can now be either drastically simplified out
    // completely removed using the nesting fold function implemented
    // above....
    def spineToCanopyAddr(addr: SAddr): Option[SAddr] = {

      val lr: SAddr => Option[SNesting[SAddr]] =
        (addr: SAddr) => Some(SDot(addr))

      val nr: (SNesting[A], STree[SNesting[SAddr]]) => Option[SNesting[SAddr]] =
        (nst, newCn) => {
          for {
            sp <- nst.spine(SDeriv(newCn.map(_ => SLeaf)))
            res <- sp.treeFold[SNesting[SAddr]](newCn.elementAt(_))(
              (a:A, canp: STree[SNesting[SAddr]]) => Some(SBox(Nil, canp))  
            ) // The node recursor gives a dummy value, since we won't use it...
          } yield res
        }

      for {
        nst <- STree.treeFoldVertical[SNesting[A], SNesting[SAddr]](cn)(lr)(nr)
        nz <- nst.seek(addr)
      } yield nz.focus.baseValue
    }

  }

  //============================================================================================
  // NESTING JOIN
  //

  // This does a join without regard to the equality between the external
  // dots and the base of new incoming guy.  Perhaps enhance with a discrimination
  // function ...
  def join[A](nn: SNesting[SNesting[A]]): Option[SNesting[A]] = 
    nn match {
      case SDot(n) => Some(n)
      case SBox(n, cn) => 
        for {
          trNst <- cn.traverse(join(_))
          r <- n.toTree.treeFold[SNesting[A]](trNst.elementAt(_))({
            case (a, c) => Some(SBox(a, c))
          })
        } yield r
    }

  //============================================================================================
  // UNZIP
  //

  // Is there a generic version of this using traverse?
  def unzip[A, B](n: SNesting[(A, B)]): (SNesting[A], SNesting[B]) =
    n.splitWith({ case (a, b) => (a, b) })

  //============================================================================================
  // PICKLING
  //

  import upickle.Js
  import upickle.default._

  import scala.{PartialFunction => PF}

  def nestingWriter[A](implicit w: Writer[A]): Writer[SNesting[A]] =
    new Writer[SNesting[A]] {
      def write0: SNesting[A] => Js.Value = {
        case SDot(a) => Js.Obj(("type", Js.Str("dot")), ("lbl", w.write(a)))
        case SBox(a, cn) => {
          val cnw : Writer[STree[SNesting[A]]] = STree.treeWriter(this)
          Js.Obj(("type", Js.Str("box")), ("lbl", w.write(a)), ("cn", cnw.write(cn)))
        }
      }
    }

  def nestingReader[A](implicit r: Reader[A]): Reader[SNesting[A]] =
    new Reader[SNesting[A]] {
      def read0: PF[Js.Value, SNesting[A]] = {
        case Js.Obj(("type", Js.Str("dot")), ("lbl", a)) => SDot(r.read(a))
        case Js.Obj(("type", Js.Str("box")), ("lbl", a), ("cn", cn)) => {
          val cnr : Reader[STree[SNesting[A]]] = STree.treeReader(this)
          SBox(r.read(a), cnr.read(cn))
        }
      }
    }


}

