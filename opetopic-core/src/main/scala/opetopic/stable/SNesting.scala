/**
  * SNesting.scala - Stable Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Traverse
import scalaz.Applicative
import scalaz.syntax.traverse._
import scalaz.std.option._

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

}


object SNesting {

  implicit object NestingTraverse extends Traverse[SNesting] {
    
    def traverseImpl[G[_], A, B](n: SNesting[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[SNesting[B]] = 
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

    // It seems one way to get rid of the double join here
    // is to have another spine implementation which returns
    // the spind as a canopy, that is, as a tree of nestings.

    // Follow the guide tree, extracting the boxes which match it.
    // Return the resulting nesting, as well as simultaneously calculating
    // the spine which remains above the crop point.
    def exciseWith[B](tr: STree[B], d: SDeriv[A]): Option[(SNesting[A], STree[SNesting[A]], STree[A])] = 
      (nst, tr) match {
        case (_, SLeaf) => 
          for {
            sp <- nst.spine(d)
            v = nst.baseValue
          } yield (SDot(v), d.plug(v).map(SDot(_)), sp)
        case (SBox(a, cn), SNode(_, sh)) => {
          for {
            trplTr <- cn.matchWithDeriv(sh)({
              case (nn, tt, dd) => nn.exciseWith(tt, dd)
            })
            (ncn, ljn, gjn) = STree.unzip3(trplTr)
            lcn <- ljn.join  // Can you think of a way to avoid the double join????
            gcn <- gjn.join
          } yield (SBox(a, ncn), lcn, gcn)
        }
        case (SDot(_), SNode(_, _)) => None
      }

    def compressWith[B](sh: Shell[B]): Option[SNesting[A]] = None

  }

  implicit class SCanopyOps[A](cn: STree[SNesting[A]]) {

    def spine: Option[STree[A]] = 
      cn.traverseWithData[Option, A, STree[A]]({
        case (nst, _, deriv) => nst.spine(deriv)
      }).flatMap(STree.join(_))

  }

  //============================================================================================
  // CONSTRUCTOR
  //

  import opetopic._

  @natElim
  def apply[A, N <: Nat](n: N)(nst: Nesting[A, N]): SNesting[A] = {
    case (Z, Obj(a)) => SDot(a)
    case (Z, Box(a, cn)) => SBox(a, STree(cn).map(SNesting(_)))
    case (S(p), Dot(a, _)) => SDot(a)
    case (S(p), Box(a, cn)) => SBox(a, STree(cn).map(SNesting(_)))
  }

  def apply[A, N <: Nat](nst: Nesting[A, N]): SNesting[A] = 
    SNesting(nst.dim)(nst)

}

