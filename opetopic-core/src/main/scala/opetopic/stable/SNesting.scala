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

    def traverseImpl[G[_], A, B](n: SNesting[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[SNesting[B]] = {

      import isAp._

      n match {
        case SDot(a) => ap(f(a))(pure(SDot(_)))
        case SBox(a, cn) => ap2(f(a), cn.traverse(traverseImpl(_)(f)))(pure(SBox(_, _)))
      }
    }
    
  }



  implicit class SNestingOps[A](nst: SNesting[A]) {

    def spine(d: SDeriv[A]): Option[STree[A]] = 
      nst match {
        case SDot(a) => Some(d.plug(a))
        case SBox(a, cn) => cn.spine
      }

  }

  implicit class SCanopyOps[A](cn: STree[SNesting[A]]) {

    def spine: Option[STree[A]] = 
      cn.traverseWithData[Option, A, STree[A]]({
        case (nst, _, deriv) => nst.spine(deriv)
      }).flatMap(STree.join(_))

  }

}

