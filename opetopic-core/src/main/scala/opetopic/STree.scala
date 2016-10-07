/**
  * STree.scala - Stable Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

sealed trait STree[+A]
case object SLeaf extends STree[Nothing]
case class SNode[A](a: A, as: STree[STree[A]]) extends STree[A]

case class SDir(val dir: List[SDir]) 

case class SDeriv[+A](val sh: STree[STree[A]], gma: SCtxt[A] = SCtxt(Nil)) {

  def plug[B >: A](b: B) : STree[B] = 
    gma.close(SNode(b, sh))

}

case class SCtxt[+A](val g: List[(A, SDeriv[STree[A]])]) {

  def close[B >: A](t: STree[B]) : STree[B] = 
    g match {
      case Nil => t
      case (a, d) :: gs => 
        SCtxt(gs).close(SNode(a, d.plug(t)))
    }

  def ::[B >: A](pr : (B, SDeriv[STree[B]])): SCtxt[B] = 
    SCtxt(pr :: g)

}

case class SZipper[+A](val focus: STree[A], val ctxt: SCtxt[A] = SCtxt[A](Nil)) {

  def close: STree[A] = 
    ctxt.close(focus)

  def closeWith[B >: A](t: STree[B]): STree[B] = 
    ctxt.close(t)

  def predecessor: Option[SZipper[A]] = 
    ctxt.g match {
      case Nil => None
      case (a, SDeriv(ts, g)) :: cs => 
        Some(SZipper(SNode(a, g.close(SNode(focus, ts))), SCtxt(cs)))
    }

  def predecessorWhich(pred: A => Boolean): Option[SZipper[A]] = 
    ctxt.g match {
      case Nil => None
      case (a, SDeriv(ts, g)) :: cs => {
        val pz = SZipper(SNode(a, g.close(SNode(focus, ts))), SCtxt(cs))
        if (pred(a)) Some(pz) else pz.predecessorWhich(pred)
      }
    }

  def visit(d: SDir): Option[SZipper[A]] = 
    (focus, d) match {
      case (SLeaf, _) => None
      case (SNode(a, as), SDir(ds)) => 
        for {
          z <- SZipper(as).seek(ds)
          r <- z match {
            case SZipper(SLeaf, _) => None
            case SZipper(SNode(t, ts), g) => 
              Some(SZipper(t, (a, SDeriv(ts, g)) :: ctxt))
          }
        } yield r
    }

  def seek(addr: SAddr): Option[SZipper[A]] = 
    addr match {
      case Nil => Some(this)
      case d :: ds => 
        for {
          z <- seek(ds)
          zz <- z.visit(d)
        } yield zz
    }

}

object STree {

  //============================================================================================
  // TRAVERSE INSTANCE
  //

  implicit object STreeTraverse extends Traverse[STree] {

    def traverse[G[_], A, B](st: STree[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[STree[B]] = 
      st.lazyTraverse[G, Unit, B](f)

  }

  //============================================================================================
  // OPS
  //

  implicit class STreeOps[A](st: STree[A]) {

    def lazyTraverse[G[_], B, C](f: LazyTraverse[G, A, B, C], addr: => SAddr = Nil)(implicit isAp: Applicative[G]): G[STree[C]] = 
      st match {
        case SLeaf => isAp.pure(SLeaf)
        case SNode(a, sh) => {

          import isAp._

          lazy val ld: SDeriv[B] = SDeriv(sh.asShell) 
          lazy val gc: G[C] = f(a, addr, ld)

          lazy val gcs: G[Shell[C]] = {
            sh.lazyTraverse(new LazyTraverse[G, STree[A], B, STree[C]] {
              def apply(b: STree[A], dir: => SAddr, der: => SDeriv[B]): G[STree[C]] = {
                lazy val eaddr = SDir(dir) :: addr
                b.lazyTraverse(f, eaddr)
              }
            })
          }

          ap2(gc, gcs)(pure(SNode(_, _)))

        }
      }

    def mapWithAddr[B](f: (A, => SAddr) => B): STree[B] = 
      lazyTraverse(funcAddrToLt[Id, A, B](f))

    def mapWithDeriv[B, C](f: (A, => SDeriv[B]) => C): STree[C] = 
      lazyTraverse(funcDerivToLt[Id, A, B, C](f))

    def mapWithData[B, C](f: (A, => SAddr, => SDeriv[B]) => C): STree[C] = 
      lazyTraverse(funcAddrDerivToLt[Id, A, B, C](f))

    def traverseWithAddr[G[_], B](f: (A, => SAddr) => G[B])(implicit isAp: Applicative[G]): G[STree[B]] = 
      lazyTraverse(funcAddrToLt[G, A, B](f))

    def traverseWithDeriv[G[_], B, C](f: (A, => SDeriv[B]) => G[C])(implicit isAp: Applicative[G]): G[STree[C]] = 
      lazyTraverse(funcDerivToLt[G, A, B, C](f))

    def traverseWithData[G[_], B, C](f: (A, => SAddr, => SDeriv[B]) => G[C])(implicit isAp: Applicative[G]): G[STree[C]] = 
      lazyTraverse(funcAddrDerivToLt[G, A, B, C](f))

    // Rewrite these using laziness as with the traverse implementation ....
    def matchWith[B](tt: STree[B]): Option[STree[(A, B)]] = 
      (st, tt) match {
        case (SLeaf, SLeaf) => Some(SLeaf)
        case (SNode(a, as), SNode(b, bs)) => {
          for {
            abs <- as.matchWith(bs)
            rs <- abs.traverse({
              case (x, y) => x.matchWith(y)
            })
          } yield SNode((a, b), rs)
        }
        case _ => None
      }

    def matchTraverse[B, C](tt: STree[B])(f: (A, B) => Option[C]): Option[STree[C]] = 
      (st, tt) match {
        case (SLeaf, SLeaf) => Some(SLeaf)
        case (SNode(a, as), SNode(b, bs)) => 
          for {
            c <- f(a, b)
            cs <- as.matchTraverse(bs)({ case (r, s) => r.matchTraverse(s)(f) })
          } yield SNode(c, cs)
        case _ => None
      }

    def matchWithDeriv[B, C, D](tt: STree[B])(f: (A, B, SDeriv[C]) => Option[D]): Option[STree[D]] =
      (st, tt) match {
        case (SLeaf, SLeaf) => Some(SLeaf)
        case (SNode(a, as), SNode(b, bs)) => 
          for {
            d <- f(a, b, SDeriv(bs.asShell, SCtxt(Nil)))
            ds <- as.matchTraverse(bs)({ case (r, s) => r.matchWithDeriv(s)(f) })
          } yield SNode(d, ds)
        case _ => None
      }

    def matchWithAddr[B, C](tt: STree[B])(f: (A, B, SAddr) => Option[C], addr: SAddr = Nil): Option[STree[C]] = 
      (st, tt) match {
        case (SLeaf, SLeaf) => Some(SLeaf)
        case (SNode(a, as), SNode(b, bs)) => 
          for {
            d <- f(a, b, addr)
            ds <- as.matchWithAddr(bs)({ case (r, s, dir) => r.matchWithAddr(s)(f, SDir(dir) :: addr) })
          } yield SNode(d, ds)
        case _ => None
      }

    def isLeaf: Boolean =
      st match {
        case SLeaf => true
        case _ => false
      }
    
    def rootValue: Option[A] =
      st match {
        case SLeaf => None
        case SNode(a, _) => Some(a)
      }

    def nodeOption: Option[(A, Shell[A])] = 
      st match {
        case SNode(a, sh) => Some(a, sh)
        case _ => None
      }

    def leafOption: Option[Unit] = 
      st match {
        case SLeaf => Some(())
        case _ => None
      }

    def elementAt(addr: SAddr): Option[A] = 
      for {
        z <- st.seekTo(addr)
        v <- z.focus.rootValue
      } yield v

    def seekTo(addr: SAddr): Option[SZipper[A]] = 
      SZipper(st).seek(addr)

    def toNesting(f: SAddr => Option[A], addr: SAddr = Nil): Option[SNesting[A]] = 
      st match {
        case SLeaf => for { a <- f(addr) } yield SDot(a)
        case SNode(a, sh) => 
          for {
            cn <- sh.traverseWithAddr[Option, SNesting[A]](
              (t, d) => t.toNesting(f, SDir(d) :: addr)
            )
          } yield SBox(a, cn)
      }

    def asShell[B]: STree[STree[B]] = 
      st.map(_ => SLeaf)

    def addrTree: STree[SAddr] =
      st.mapWithAddr({ case (_, addr) => addr })

    def comultiply: STree[STree[A]] =
      st.mapWithDeriv[A, STree[A]]({
        case (a, d) => SNode(a, d.sh)
      })

    def treeFold[B](lr: SAddr => Option[B])(nr: (A, STree[B]) => Option[B]): Option[B] =
      STree.treeFold(st)(lr)(nr)

    def graftWith(brs: STree[STree[A]]): Option[STree[A]] =
      graft(st, brs)

    // The Kleisli version of join from below ...
    def bind[B](f : (A, SAddr) => Option[STree[B]], addr: SAddr = Nil) : Option[STree[B]] = 
      st match {
        case SLeaf => Some(SLeaf)
        case SNode(a, sh) => 
          for {
            here <- f(a, addr) 
            there <- sh.traverseWithAddr((b, dir) => {
              b.bind(f, SDir(dir) :: addr)
            })
            res <- graft(here, there)
          } yield res
      }

    // Fix for laziness ...
    def flattenWith[B](d: SDeriv[B], addr: SAddr = Nil)(f: SAddr => Option[B]): Option[STree[B]] = 
      st match {
        case SLeaf => f(addr).map(d.plug(_)) 
        case SNode(a, sh) => 
          for {
            toJn <- sh.traverseWithData[Option, B, STree[B]](
              (t, dir, deriv) => t.flattenWith(deriv, SDir(dir) :: addr)(f)
            )
            res <- join(toJn)
          } yield res
      }


    def takeWhile(prop: A => Boolean, deriv: SDeriv[STree[A]] = SDeriv(SLeaf)): Option[(STree[A], Shell[A])] = 
      st match {
        case SLeaf => Some(SLeaf, deriv.plug(SLeaf))
        case SNode(a, sh) => 
          if (prop(a)) {
            // println("Taking: " + a.toString)
            for {
              pr <- sh.traverseWithDeriv[Option, STree[A], (STree[A], Shell[A])](
                (b, d) => b.takeWhile(prop, d)
              )
              (newSh, toJn) = STree.unzip(pr)
              cropping <- STree.join(toJn)
            } yield (SNode(a, newSh), cropping)
          } else Some(SLeaf, deriv.plug(SNode(a, sh)))
      }

    def takeWithMask[B](msk: STree[B], deriv: SDeriv[STree[A]] = SDeriv(SLeaf)): Option[(STree[A], Shell[A])] =
      (st, msk) match {
        case (tr, SLeaf) => Some(SLeaf, deriv.plug(tr))
        case (SNode(a, sh), SNode(_, msh)) => 
          for {
            pr <- sh.matchWithDeriv[STree[B], STree[A], (STree[A], Shell[A])](msh)(
              (b, mb, d) => b.takeWithMask(mb, d)
            )
            (nsh, toJn) = STree.unzip(pr)
            crp <- toJn.join
          } yield (SNode(a, nsh), crp)
        case _ => None
      }

    def treeSplit[B, C](f: A => (B, C)): (STree[B], STree[C]) =
      st match {
        case SLeaf => (SLeaf, SLeaf)
        case SNode(a, sh) => {
          val (b, c) = f(a)
          val (bs, cs) = sh.treeSplit(_.treeSplit(f))
          (SNode(b, bs), SNode(c, cs))
        }
      }

    def foreach(op: A => Unit): Unit = 
      st match {
        case SLeaf => ()
        case SNode(a, sh) => {
          for { b <- sh } { b.foreach(op) }
          op(a)
        }
      }

    def foreachWithAddr(op: (A, SAddr) => Unit, addr: SAddr = Nil): Unit = 
      st match {
        case SLeaf => ()
        case SNode(a, sh) => {
          sh.foreachWithAddr({
            case (b, dir) => b.foreachWithAddr(op, SDir(dir) :: addr)
          })
          op(a, addr)
        }
      }

  }

  //============================================================================================
  // SPLIT AND UNZIP
  //

  def unzip[A, B](tr: STree[(A, B)]): (STree[A], STree[B]) = 
    tr.treeSplit({ case (a, b) => (a, b) })

  //============================================================================================
  // GRAFTING AND JOINING
  //

  case class STreeFold[A, B](lr: SAddr => Option[B])(nr: (A, STree[B]) => Option[B]) {

    def unzipAndJoin(t: STree[(STree[B], STree[SAddr])]): Option[(STree[STree[B]], STree[SAddr])] = {
      val (bs, adJn) = unzip(t)
      join(adJn).map((bs, _))
    }

    def unzipJoinAndAppend(t: STree[(STree[B], STree[SAddr])], mb: Option[B]): Option[(STree[B], STree[SAddr])] = 
      for {
        pr <- unzipAndJoin(t)
        (bs, at) = pr
        b <- mb
      } yield (SNode(b, bs), at)


    def foldPass(h: STree[STree[A]], addr: SAddr, d: SDeriv[SAddr]): Option[(STree[B], STree[SAddr])] = 
      h match {
        case SLeaf => Some(SLeaf, d.plug(addr))
        case SNode(SLeaf, hs) => 
          for {
            hr <- hs.traverseWithData[Option, SAddr, (STree[B], STree[SAddr])]({
              (hbr, dir, deriv) => foldPass(hbr, SDir(dir) :: addr, deriv)
            })
            r <- unzipJoinAndAppend(hr, lr(addr))
          } yield r
        case SNode(SNode(a, vs), hs) =>
          for {
            pr <- foldPass(vs, addr, d)
            (bs, at) = pr
            mr <- hs.matchWithDeriv[SAddr, SAddr, (STree[B], STree[SAddr])](at)(foldPass(_, _, _))
            r <- unzipJoinAndAppend(mr, nr(a, bs))
          } yield r
      }

    def initHorizontal(a: A, h: STree[STree[STree[A]]])(m: Option[(B, STree[SAddr])]): Option[(B, STree[SAddr])] = 
      for {
        pa <- m
        (c, at) = pa
        r <- h.matchWithDeriv[SAddr, SAddr, (STree[B], STree[SAddr])](at)(foldPass(_, _, _))
        pb <- unzipAndJoin(r)
        (cs, atr) = pb
        b <- nr(a, SNode(c, cs))
      } yield (b, atr)

    def initVertical(a: A, v: STree[A], h: STree[STree[STree[A]]]): Option[(B, STree[SAddr])] = 
      v match {
        case SLeaf => initHorizontal(a, h)(
          for {
            b <- lr(Nil)
          } yield (b, h.mapWithAddr((_, d) => SDir(d) :: Nil))
        )
        case SNode(aa, SLeaf) => initHorizontal(a, h)(
          for {
            b <- nr(aa, SLeaf)
          } yield (b, h.map(_ => Nil))
        )
        case SNode(aa, SNode(vv, hh)) => 
          initHorizontal(a, h)(initVertical(aa, vv, hh))
      }

  }

  // A fold with leaves called with the horizontal address 
  def treeFold[A, B](t: STree[A])(lr: SAddr => Option[B])(nr: (A, STree[B]) => Option[B]): Option[B] =
    t match {
      case SLeaf => lr(Nil)
      case SNode(a, SLeaf) => nr(a, SLeaf)
      case SNode(a, SNode(v, hs)) => STreeFold(lr)(nr).initVertical(a, v, hs).map(_._1)
    }

  // The same as above, but use the vertical leaf address...
  def treeFoldVertical[A, B](t: STree[A], addr: SAddr = Nil)(lr: SAddr => Option[B])(nr: (A, STree[B]) => Option[B]): Option[B] =
    t match {
      case SLeaf => lr(addr)
      case SNode(a, sh) => 
        for {
          shRes <- sh.traverseWithAddr[Option, B](
            (b, dir) => treeFoldVertical(b, SDir(dir) :: addr)(lr)(nr)
          )
          res <- nr(a, shRes)
        } yield res
    }

  def graft[A](st: STree[A], bs: STree[STree[A]]): Option[STree[A]] =
    treeFold(st)(bs.elementAt(_))({ case (a, as) => Some(SNode(a, as)) })

  def join[A](st: STree[STree[A]]): Option[STree[A]] = 
    st match {
      case SLeaf => Some(SLeaf)
      case SNode(a, as) => 
        for {
          blorp <- as.traverse(join(_))
          res <- graft(a, blorp)
        } yield res
    }

  implicit class ShellOps[A](sh: STree[STree[A]]) {

    def join: Option[STree[A]] = 
      STree.join(sh)

    def extents: Option[STree[SAddr]] = 
      extents(Nil)

    def extents(addr: SAddr): Option[STree[SAddr]] = 
      for {
        jnSh <- sh.traverseWithData[Option, SAddr, STree[SAddr]]({
          case (SLeaf, dir, deriv) => Some(deriv.plug(SDir(dir) :: addr))
          case (SNode(_, ssh), dir, deriv) => ssh.extents(SDir(dir) :: addr)
        })
        res <- jnSh.join
      } yield res

  }

  //============================================================================================
  // CONSTRUCTORS
  //

  def obj[A](a: A): STree[A] = SNode(a, SLeaf)

  def lst[A](l: List[A]): STree[A] = 
    l match {
      case Nil => SLeaf
      case a :: as => SNode(a, obj(lst(as)))
    }

  //============================================================================================
  // PICKLING
  //

  import upickle.Js
  import upickle.default._

  import scala.{PartialFunction => PF}

  def treeWriter[A](implicit w: Writer[A]): Writer[STree[A]] = 
    new Writer[STree[A]] {
      def write0: STree[A] => Js.Value = {
        case SLeaf => Js.Obj(("type", Js.Str("lf")))
        case SNode(a, sh) => {
          val shw : Writer[Shell[A]] = treeWriter(this)
          Js.Obj(("type", Js.Str("nd")), ("lbl", w.write(a)), ("sh", shw.write(sh)))
        }
      }
    }

  def treeReader[A](implicit r: Reader[A]): Reader[STree[A]] = 
    new Reader[STree[A]] {
      def read0: PF[Js.Value, STree[A]] = {
        case Js.Obj(("type", Js.Str("lf"))) => SLeaf
        case Js.Obj(("type", Js.Str("nd")), ("lbl", a), ("sh", sh)) => {
          val shr : Reader[Shell[A]] = treeReader(this)
          SNode(r.read(a), shr.read(sh))
        }
      }
    }


}
