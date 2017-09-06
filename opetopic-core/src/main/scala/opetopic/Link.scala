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
  case class LinkLeaf(d: SDir) extends LinkMarker
  case class LinkRoot(n: SNesting[A]) extends LinkMarker
  case class LinkNode(z: SNstZipper[A]) extends LinkMarker

  // So our little zipper guy is a list of markers
  type LinkZipper = List[LinkMarker]

  implicit class LinkZipperOps(lz: LinkZipper) {

    def foci: List[String] = 
      lz.map({
        case LinkLeaf(_) => "Leaf"
        case LinkRoot(_) => "Root"
        case LinkNode(z) => z.focus.baseValue.toString
      })

    // Enter a box along the root edge, continuing until
    // the internal root dot is reached.
    def ascend: Except[LinkZipper] =
      lz match {
        case Nil => succeed(Nil)
        case LinkLeaf(_) :: zs => throwError("Attempting to ascend at a leaf")
        case LinkRoot(n) :: zs => {

          // We are trying to ascend in from a root.  Recurse up the complex
          // and then ascend into the trivial zipper.
          for {
            uz <- zs.ascend
            az <- (LinkNode(SNstZipper(n)) :: uz).ascend
          } yield az

        }
        case LinkNode(z) :: zs => {

          z.focus match {
            case SDot(a) => {

              // We have arrived at a dot.  Simply return
              // the current zipper as there is nothing to do.
              succeed(lz)

            }
            case SBox(a, SLeaf) => {

              // We have arrived at a loop.  We should pass through
              // the other side.  I think this should mean following
              // the nil direction.

              lz.follow(SDir(Nil))

            }
            case SBox(a, SNode(n, sh)) => {

              // There is a further box to attempt to ascend.  Construct
              // the appropriate zipper and continue.

              val rz = SNstZipper(n, (a, SDeriv(sh)) :: z.ctxt)

              for {
                uz <- zs.follow(SDir(Nil))
                az <- (LinkNode(rz) :: uz).ascend
              } yield az

            }
          }

        }
      }

    // Enter a box along a given direction until the first
    // internal dot is encountered
    def descend(dir: SDir): Except[LinkZipper] =
      throwError("Unimplemented")

    // Trace the edge emanating in the given direction until a dot is reached
    def follow(dir: SDir): Except[LinkZipper] =
      lz match {
        case Nil => succeed(Nil)
        case LinkLeaf(_) :: zs => throwError("Attempting to follow at a leaf")
        case LinkRoot(_) :: zs => throwError("Attempting to follow at a root")
        case LinkNode(z) :: zs => {

          if (z.hasParent) {
            z.sibling(dir) match {
              case Some(sibZip) => {

                // This should be the direction of the newly found sibling
                // with respect to its parent.
                val myDir = sibZip.address.head

                for {
                  rz <- zs.rewind
                  pz <- rz.follow(myDir)
                  az <- (LinkNode(sibZip) :: pz).ascend
                } yield az

              }
              case None => {

                // If there is no sibling, then the idea is that we should pass
                // to the parent.

                ???

              }
            }
          } else {
            // If we are at the base box, we exit to a leaf marker and
            // rewind the rest of the zipper 
            zs.rewind.map(LinkLeaf(dir) :: _)
          }

        }
      }

    // Trace backwards along the root edge emanating from this cell
    def rewind: Except[LinkZipper] =
      lz match {
        case Nil => succeed(Nil)
        case LinkLeaf(_) :: zs => throwError("Attempting to rewind at a leaf")
        case LinkRoot(_) :: zs => throwError("Attempting to rewind at a root")
        case LinkNode(z) :: zs => {



          throwError("Unimplemented")


        }
      }

  }


  def link(fa: FaceAddr) : Option[SComplex[A]] = None

}

