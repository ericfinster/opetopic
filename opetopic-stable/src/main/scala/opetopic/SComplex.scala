/**
  * SComplex.scala - Stable Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

trait ComplexTypes {

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

    //
    //  Source Calculation
    //

    def truncateToDim(i: Int): SComplex[A] = 
      c.take(i + 1)

    def sourceAt(addr: SAddr): Option[SComplex[A]] = 
      for {
        z <- SCmplxZipper(c).seek(addr)
        f <- z.focusFace
      } yield f

    def face(i: Int)(addr: SAddr): Option[SComplex[A]] = 
      truncateToDim(i).sourceAt(addr)

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

    def foreachWithAddr(op: (A, SAddr) => Unit): Unit = 
      c.foreach((n: SNesting[A]) => 
        n.foreachWithAddr(op)
      )

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
        case ||(_) => Some(SDeriv(SNode(SLeaf, SLeaf)))
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
            pr <- hd.focus.exciseWith(guide, SDeriv(SNode(SLeaf, SLeaf)))
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
  // PICKLING
  //

  import upickle.Js
  import upickle.default._

  def complexToJson[A](c: SComplex[A])(implicit w: Writer[A]): String =
    upickle.json.write(Suite.suiteWriter(SNesting.nestingWriter(w)).write(c))

  def complexFromJson[A](c: Js.Value)(implicit r: Reader[A]): SComplex[A] = 
    Suite.suiteReader(SNesting.nestingReader(r)).read(c)

}
