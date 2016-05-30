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

}

case class SNstZipper[+A](val focus: SNesting[A], val ctxt: SNstCtxt[A] = SNstCtxt(Nil)) {

  def withFocus[B >: A](f: SNesting[B]): SNstZipper[B] = 
    SNstZipper(f, ctxt)

  def close: SNesting[A] = 
    ctxt.close(focus)

  def closeWith[B >: A](n: SNesting[B]): SNesting[B] = 
    ctxt.close(n)

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
      deriv: => SDeriv[B] = SDeriv(SNode(SLeaf, SNode(SLeaf, SLeaf)))
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

    def toTree: STree[A] = 
      foldNesting[STree[A]](a => SLeaf)((a, sh) => SNode(a, sh))

    def toTreeWith[B](f: A => B): STree[B] = 
      foldNesting[STree[B]](a => SLeaf)((a, sh) => SNode(f(a), sh))

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

    def baseValue: A = 
      nst match {
        case SDot(a) => a
        case SBox(a, _) => a
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

    def canopyWithGuide[B](g: STree[B], d: SDeriv[SNesting[A]]): Option[STree[SNesting[A]]] = 
      (nst, g) match {
        case (_, SLeaf) => Some(d.plug(nst))
        case (SBox(_, cn), SNode(_, sh)) => 
          for {
            toJn <- cn.matchWithDeriv(sh)({
              case (nn, gg, dd) => nn.canopyWithGuide(gg, dd)
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
            cn <- nst.canopyWithGuide(sk, d)
            nnn <- cn.matchWithDeriv(sh)({
              case (nn, gg, dd) => nn.compressWith(gg, dd)
            })
          } yield SBox(nst.baseValue, nnn)
        case SLeaf => Some(nst)
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

  }

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

