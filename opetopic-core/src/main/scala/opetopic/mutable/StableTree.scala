/**
  * StableTree.scala - Stable, infinite dimensional trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mutable

import scalaz.Traverse
import scalaz.Applicative
import scalaz.syntax.traverse._
import scalaz.std.option._

import opetopic._
import syntax.tree._

sealed trait STree[+A]
case object SLeaf extends STree[Nothing]
case class SNode[A](a: A, as: STree[STree[A]]) extends STree[A]

case class SDir(pth: List[SDir])

case class SDeriv[A](sh: STree[STree[A]], gma: SCtxt[A]) {

  def plug(a: A) : STree[A] = 
    gma.close(SNode(a, sh))

}

case class SCtxt[A](val g: List[(A, SDeriv[STree[A]])]) {

  def close(t: STree[A]) : STree[A] = 
    g match {
      case Nil => t
      case (a, d) :: gs => 
        SCtxt(gs).close(SNode(a, d.plug(t)))
    }

  def ::(pr : (A, SDeriv[STree[A]])): SCtxt[A] = 
    SCtxt(pr :: g)

}

case class SZipper[A](val head: STree[A], val ctxt: SCtxt[A] = SCtxt[A](Nil)) {

  def visit(d: SDir): Option[SZipper[A]] = 
    (head, d) match {
      case (SLeaf, SDir(Nil)) => Some(this) // Allow for this?
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

  def seek(a: List[SDir]): Option[SZipper[A]] = 
    a match {
      case Nil => Some(this)
      case d :: ds => 
        for {
          z <- seek(ds)
          zz <- z.visit(d)
        } yield zz
    }

}

object STree {

  type SAddr = List[SDir]

  //============================================================================================
  // TRAVERSE INSTANCE
  //

  implicit object StableTreeTraverse extends Traverse[STree] {

    def traverseImpl[G[_], A, B](st: STree[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[STree[B]] = {

      import isAp._

      st match {
        case SLeaf => pure(SLeaf)
        case SNode(a, as) => {

          val gb: G[B] = f(a)
          val gbs: G[STree[STree[B]]] = 
            traverseImpl(as)(traverseImpl(_)(f))

          ap2(gb, gbs)(pure(SNode(_, _)))

        }
      }

    }
  }

  //============================================================================================
  // OPS
  //

  implicit class STreeOps[A](st: STree[A]) {

    def mapWithAddr[B](f: (A, SAddr) => B, addr: SAddr = Nil): STree[B] = 
      st match {
        case SLeaf => SLeaf
        case SNode(a, as) => {

          val bs = as.mapWithAddr((b, dir) => {
            b.mapWithAddr(f, SDir(dir) :: addr)
          })

          SNode(f(a, addr), bs)

        }
      }

    def traverseWithAddr[G[_], B](f: (A, SAddr) => G[B], addr: SAddr = Nil)(implicit isAp: Applicative[G]): G[STree[B]] = 
      st match {
        case SLeaf => isAp.pure(SLeaf)
        case SNode(a, as) => {

          import isAp._

          val gb: G[B] = f(a, addr)
          val gbs: G[STree[STree[B]]] = 
            as.traverseWithAddr({
              case (b, dir) => b.traverseWithAddr(f, SDir(dir) :: addr)
            })

          ap2(gb, gbs)(pure(SNode(_, _)))

        }
      }

    def traverseWithData[G[_], B, C](f: (A, SAddr, SDeriv[B]) => G[C], addr: SAddr = Nil)(implicit isAp: Applicative[G]): G[STree[C]] = 
      st match {
        case SLeaf => isAp.pure(SLeaf)
        case SNode(a, as) => {

          import isAp._

          val gc: G[C] = f(a, addr, SDeriv(as.map(_ => SLeaf), SCtxt(Nil)))
          val gcs: G[STree[STree[C]]] = 
            as.traverseWithAddr({
              case (b, dir) => b.traverseWithData(f, SDir(dir) :: addr)
            })

          ap2(gc, gcs)(pure(SNode(_, _)))

        }
      }

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
            d <- f(a, b, SDeriv(bs.map(_ => SLeaf), SCtxt(Nil)))
            ds <- as.matchTraverse(bs)({ case (r, s) => r.matchWithDeriv(s)(f) })
          } yield SNode(d, ds)
        case _ => None
      }

    def rootValue: Option[A] = 
      st match {
        case SLeaf => None
        case SNode(a, _) => Some(a)
      }

    def elementAt(addr: SAddr): Option[A] = 
      for {
        z <- SZipper(st).seek(addr)
        v <- z.head.rootValue
      } yield v

    def graftWith(brs: STree[STree[A]]): Option[STree[A]] = 
      graft(st, brs)

    def graftRec[B](lr: SAddr => Option[B])(nr: (A, STree[B]) => Option[B]): Option[B] =
      STree.graftRec(st)(lr)(nr)

    def unstableOfDim[N <: Nat](n: N): Option[Tree[A, N]] = 
      unstably(n)(st)

  }

  //============================================================================================
  // SPLIT AND UNZIP
  //

  def splitWith[A, B, C](tr: STree[C])(f: C => (A, B)) : (STree[A], STree[B]) = 
    tr match {
      case SLeaf => (SLeaf, SLeaf)
      case SNode(c, cs) => {
        val (a, b) = f(c)
        val (as, bs) = splitWith(cs)(splitWith(_)(f))
        (SNode(a, as), SNode(b, bs))
      }
    }

  def unzip[A, B](tr: STree[(A, B)]): (STree[A], STree[B]) = 
    splitWith(tr)({ case (a, b) => (a, b) })

  //============================================================================================
  // GRAFTING AND JOINING
  //

  case class STreeGrafter[A, B](lr: SAddr => Option[B])(nr: (A, STree[B]) => Option[B]) {

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


    def graftPass(h: STree[STree[A]], addr: SAddr, d: SDeriv[SAddr]): Option[(STree[B], STree[SAddr])] = 
      h match {
        case SLeaf => Some(SLeaf, d.plug(addr))
        case SNode(SLeaf, hs) => 
          for {
            hr <- hs.traverseWithData[Option, SAddr, (STree[B], STree[SAddr])]({
              case (hbr, dir, deriv) => graftPass(hbr, SDir(dir) :: addr, deriv)
            })
            r <- unzipJoinAndAppend(hr, lr(addr))
          } yield r
        case SNode(SNode(a, vs), hs) =>
          for {
            pr <- graftPass(vs, addr, d)
            (bs, at) = pr
            mr <- hs.matchWithDeriv[SAddr, SAddr, (STree[B], STree[SAddr])](at)(graftPass(_, _, _))
            r <- unzipJoinAndAppend(mr, nr(a, bs))
          } yield r
      }

    def initHorizontal(a: A, h: STree[STree[STree[A]]])(m: Option[(B, STree[SAddr])]): Option[(B, STree[SAddr])] = 
      for {
        pa <- m
        (c, at) = pa
        r <- h.matchWithDeriv[SAddr, SAddr, (STree[B], STree[SAddr])](at)(graftPass(_, _, _))
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

  def graftRec[A, B](t: STree[A])(lr: SAddr => Option[B])(nr: (A, STree[B]) => Option[B]): Option[B] = 
    t match {
      case SLeaf => lr(Nil)
      case SNode(a, SLeaf) => nr(a, SLeaf)
      case SNode(a, SNode(v, hs)) => STreeGrafter(lr)(nr).initVertical(a, v, hs).map(_._1)
    }

  def graft[A](st: STree[A], bs: STree[STree[A]]): Option[STree[A]] = 
    graftRec(st)(bs.elementAt(_))({ case (a, as) => Some(SNode(a, as)) })

  def join[A](st: STree[STree[A]]): Option[STree[A]] = 
    st match {
      case SLeaf => Some(SLeaf)
      case SNode(a, as) => 
        for {
          blorp <- as.traverse(join(_))
          res <- graft(a, blorp)
        } yield res
    }

  //============================================================================================
  // CONSTRUCTORS
  //

  @natElim
  def apply[A, N <: Nat](n: N)(t: Tree[A, N]): STree[A] = {
    case (Z, Pt(a)) => SNode(a, SNode(SLeaf, SLeaf))
    case (S(p), Leaf(_)) => SLeaf
    case (S(p), Node(a, sh)) => SNode(a, STree(sh).map(STree(_)))
  }

  def apply[A, N <: Nat](t: Tree[A, N]): STree[A] = 
    STree(t.dim)(t)

  @natElim
  def unstably[A, N <: Nat](n: N)(st: STree[A]) : Option[Tree[A, N]] = {
    case (Z, SLeaf) => None
    case (Z, SNode(a, _)) => Some(Pt(a))
    case (S(p: P), SLeaf) => Some(Leaf(S(p)))
    case (S(p: P), SNode(a, as)) => 
      for {
        bs <- as.traverse(unstably(S(p))(_))
        cs <- unstably(p)(bs)
      } yield Node(a, cs)
  }


}
