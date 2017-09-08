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
  case class LinkLeaf(n: SNesting[A]) extends LinkMarker
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
      lz match {
        case Nil => succeed(Nil)
        case LinkRoot(_) :: zs => throwError("Attempting to descend at the root")
        case LinkLeaf(n) :: zs => {

          // To descend from a leaf, we should ascend in the upper
          // complex and the descend using the Node case on the trivial zipper.
          for {
            uz <- zs.ascend
            az <- (LinkNode(SNstZipper(n)) :: uz).descend(dir)
          } yield az

        }
        case LinkNode(z) :: zs => {

          z.focus match {
            case SDot(a) => {

              // We have arrived.  Return the current zipper
              succeed(lz)

            }
            case SBox(a, SLeaf) => {

              // We are passing through a loop.
              lz.rewind

            }
            case SBox(a, cn) => {

              attempt(
                STree.treeFold[SNesting[A], List[SAddr]](cn)((hAddr, vAddr) =>
                  if (hAddr == dir.dir) Some(List(vAddr)) else Some(Nil)
                )((_, _, lstTr) => Some(lstTr.toList.flatten)),
                "Edge address map calculation failed."
              ).flatMap({
                case vAddr :: Nil => {

                  // vAddr should now be the address in the canopy of
                  // the leaf corresponding to the direction we are entering on.
                  for {
                    cz <- attempt(cn.seekTo(vAddr), "Vertical address was invalid")
                    _ <- attempt(cz.focus.leafOption, "Vertical address was not a leaf")
                    p <- attempt(cz.predecessor, "Leaf has no parent")

                    vDir = SDir(p.address)
                    uz <- zs.follow(vDir)
                    nz <- attempt(z.visit(vDir), "Local visit failed")

                  } yield LinkNode(nz) :: uz

                }
                case _ => throwError("Unexpected address list")
              })

            }
          }

        }
      }

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

                // The lack of a sibling should mean we hit a leaf
                // (Should you check this?), so we calculate the corresponding
                // external direction and then continue in the parent

                val lfAddr = dir :: z.ctxt.g.head._2.g.address

                for {
                  // First calculate the external address
                  p <- attempt(z.parent, "Failed to get parent")
                  cn <- attempt(p.focus.boxOption.map(_._2), "No canopy")
                  addrNst <- attempt(
                    cn.treeFold[SNesting[SAddr]](a => Some(SDot(a)))((_, nwCn) => Some(SBox(Nil, nwCn))),
                    "Tree fold failed"
                  )
                  az <- attempt(addrNst.seek(lfAddr), "Invalid leaf address")
                  extAddr <- attempt(az.focus.dotOption, "Leaf was not a dot")

                  // Now continue along that address in the parent
                  uz <- zs.rewind
                  az <- (LinkNode(p) :: uz).follow(SDir(extAddr))

                } yield az

              }
            }
          } else {

            // If we are at the base box, we exit to a leaf marker and
            // rewind the rest of the zipper

            zs.rewind.map(LinkLeaf(z.close) :: _)

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

          if (z.hasParent) {

            z.predecessor match {
              case Some(predZip) => {


                // Here we want to "turn the corner" as it were.  So we need
                // to calculate the address we are coming from and then descend
                // in that direction ...

                val descDir = z.ctxt.g.head._2.g.address.head

                for {
                  uz <- zs.rewind
                  az <- uz.follow(predZip.address.head)
                  rz <- (LinkNode(predZip) :: az).descend(descDir)
                } yield rz

              }
              case None => {

                // If there is no predecessor, we rewind in higer dimensions
                // passing to the parent and continue recursively.

                for {
                  p <- attempt(z.parent, "Failed to get parent")
                  uz <- zs.rewind
                  rz <- (LinkNode(p) :: uz).rewind
                } yield rz

              }
            }

          } else {

            // Exit to the root and rewind the rest
            zs.rewind.map(LinkRoot(z.close) :: _)


          }

        }
      }

  }


  def link(fa: FaceAddr) : Option[SComplex[A]] = None

}

