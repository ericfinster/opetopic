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

  def traverseCofaces[B](fa: FaceAddr)(f: Either[A, A] => Option[B]): Option[SComplex[B]] = {

    val (left, right) = cmplx.grab(fa.codim)

    for {
      z <- SCmplxZipper(left).seek(fa.address)
      d <- z.focusDeriv[Boolean]

      // Now make a copy of the web with the guy highlighted.
    } yield (z, right)

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

    None

  }

  // Here is the idea about the link: recall that a major outstanding
  // question was *when* the traversal of a particular box was finished.
  // That is, in higher dimensions, we end up traversing many times.
  // (And this is annoying from an efficiency standpoint ....)
  // Anyway, the idea is that this is somehow determined by the
  // coface structure.  Not sure how, but it's something to look into.


}

