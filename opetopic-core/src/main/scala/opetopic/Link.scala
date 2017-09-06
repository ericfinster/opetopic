/**
  * Link.scala - Calculating the link
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

class LinkCalculator[A](val cmplx: SComplex[A]) {

  sealed trait LinkMarker
  case object LinkLeaf extends LinkMarker
  case class LinkRoot(n: SNesting[A]) extends LinkMarker
  case class LinkInternal(nz: SNstZipper[A]) extends LinkMarker

  // So our little zipper guy is a list of markers
  type LinkZipper = List[LinkMarker]

  implicit class LinkZipperOps(lz: LinkZipper) {

    // Now, the idea is that there are a couple of different
    // operations: we can ascend, descend, enter, etc, etc.

    // These operations should all maintain a kind of invariant
    // about the positioning of the zippers.

  }


  def link(fa: FaceAddr) : Option[SComplex[A]] = None

  // Right, so a couple of different things are unwinding at
  // this point, and perhaps it will be best to go back and
  // visit a couple of them.

  // 1) There is what seems like the fundamental idea of the
  //    bond traverse.  This already shows some of the difficulties
  //    with your approach to the grafting functions, and these
  //    should be resolved.
  //
  // 2) Once this is done, I think traversing the cofaces can be
  //    done much, much simpler, that is, using the algorithm that
  //    you would first think of: during a bond traverse, you simply
  //    look if at least one of the faces has been previously flagged
  //    as being a coface.  Easy peasy.
  //
  // 3) Possibly this can even be adapted to the link calculation.
  //    Dunno.
  //

  def traverseCofaces[B](codim: Int, sel: A => Boolean)(f: Either[A, A] => Option[B]): Option[SComplex[B]] = {


    // The hart of the routine: do a bond traverse and just check if any of the source
    // or target faces of a given cell are cofaces.  If any are, so is the current cell.
    // Do this for each nesting, passing further down the list
    def cofacePass(c: SComplex[A], l: List[SNesting[A]], s: SNesting[Boolean]): Option[List[SNesting[B]]] =
      l match {
        case Nil => Some(Nil)
        case n :: ns => {
          for {
            // Use the complex to calculate the required derivative
            d <- SCmplxZipper(c).focusDeriv[Boolean]
            prNst <- n.bondTraverse(s, d)({
              case (a, bsrcs, btgt) => {
                if (bsrcs.toList.exists(b => b) || btgt) {
                  f(Left(a)).map(r => (r, true)) // is a coface
                } else {
                  f(Right(a)).map(r => (r, false)) // isn't a coface
                }
              }
            })
            (rNst, bnst) = SNesting.unzip(prNst)
            ll <- cofacePass(c >> n, ns, bnst)
          } yield rNst :: ll

        }
      }

    val (left, right) = cmplx.grab(codim)

    for {

      // Traverse in the left half of the complex
      newLeft <- left.traverseComplex((a : A) =>
        if (sel(a)) f(Left(a)) else f(Right(a))
      )

      // Generate the seed boolean nesting to use
      boolNst = left.head.map(sel(_))

      // Now traverse the right half using the tagging info.
      newRight <- cofacePass(left, right, boolNst)

    } yield newLeft ++ newRight

  }


  // def traverseCofaces[B](codim: Int, sel: A => Boolean)(
  //   f: Either[A, A] => Option[B]
  // ): Option[SComplex[B]] = {

  //   val (web, coweb) = c.grab(codim)

  //   val dispatch : A => Option[B] =
  //     (a: A) => if (sel(a)) f(Right(a)) else f(Left(a))

  //   // Map in the lower dimensions and the
  //   // head dimension, returning the seed boolean complex
  //   val (resWebOpt, bcmplx) =
  //     web match {
  //       case ||(hd) => (hd.traverse(dispatch).map(||(_)), ||(hd.map(sel)))
  //       case tl >> hd => {
  //         val rwo =
  //           for {
  //             dt <- (tl : SComplex[A]).traverse(dispatch)
  //             dh <- hd.traverse(dispatch)
  //           } yield dt >> dh

  //         (rwo, (tl : SComplex[A]).map(_ => false) >> hd.map(sel))
  //       }
  //     }

  //   // Actually, we don't need the whole boolean complex because
  //   // we can annotate the focusDeriv call below with a type

  //   def traverseCoweb(cw: List[SNesting[A]], bc: SComplex[Boolean]): Option[List[SNesting[B]]] =
  //     cw match {
  //       case Nil => Some(Nil)
  //       case n :: ns => {
  //         for {
  //           bd <- SCmplxZipper(bc).focusDeriv[Boolean]
  //           newBnst <- n.bondTraverse(bc.head, bd)({
  //             case (a, srcBs, tgtB) => {
  //               Some(tgtB || srcBs.toList.exists(b => b))
  //             }
  //           })  // Uh, yuck.  Two traversals instead of
  //               // one because the match function is to specific
  //           prNst <- n.matchWith(newBnst)
  //           r <- prNst.traverse({
  //             case (a, true) => f(Right(a))
  //             case (a, false) => f(Left(a))
  //           })
  //           rs <- traverseCoweb(ns, bc >> newBnst)
  //         } yield r :: rs
  //       }
  //     }

  //   for {
  //     resWeb <- resWebOpt
  //     resCw <- traverseCoweb(coweb, bcmplx)
  //   } yield resWeb ++ resCw

  // }



  // Here is the idea about the link: recall that a major outstanding
  // question was *when* the traversal of a particular box was finished.
  // That is, in higher dimensions, we end up traversing many times.
  // (And this is annoying from an efficiency standpoint ....)
  // Anyway, the idea is that this is somehow determined by the
  // coface structure.  Not sure how, but it's something to look into.


}

