/**
  * Link.scala - Calculating the link
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

object LinkCalculator {

  sealed trait LinkMarker[A]
  case class LinkLeaf[A](n: SNesting[A], d: SDir) extends LinkMarker[A]
  case class LinkRoot[A](n: SNesting[A]) extends LinkMarker[A]
  case class LinkNode[A](z: SNstZipper[A]) extends LinkMarker[A]

  // So our little zipper guy is a list of markers
  type LinkZipper[A] = List[LinkMarker[A]]

  def apply[A](cmplx: SComplex[A]): LinkZipper[A] =
    (cmplx : Suite[SNesting[A]]).toList.map(n => LinkRoot(n))

  implicit class LinkZipperOps[A](lz: LinkZipper[A]) {

    def printFoci: Unit =
      println("Foci: " + foci.mkString(", "))

    def fociString: String =
      foci.mkString(", ")

    def foci: List[String] = 
      lz.map({
        case LinkLeaf(_, _) => "Leaf"
        case LinkRoot(n) => "Root (" ++ n.baseValue.toString ++ ")"
        case LinkNode(z) => z.focus.baseValue.toString
      })

    // Enter a box along the root edge, continuing until
    // the internal root dot is reached.
    def ascend: Except[LinkZipper[A]] = {

      println("(Ascend) " + fociString)

      lz match {
        case Nil => succeed(Nil)
        case LinkLeaf(_, _) :: zs => throwError("Attempting to ascend at a leaf")
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

              // println("Ascend finishing at: " + a.toString)

              succeed(lz)

            }
            case SBox(a, SLeaf) => {

              // We have arrived at a loop.  We should pass through
              // the other side.  I think this should mean following
              // the nil direction.

              // println("Ascending past loop: " + a.toString)

              lz.follow(SDir(Nil))

            }
            case SBox(a, SNode(n, sh)) => {

              // There is a further box to attempt to ascend.  Construct
              // the appropriate zipper and continue.

              // println("Ascending past box: " + a.toString)

              val rz = SNstZipper(n, (a, SDeriv(sh)) :: z.ctxt)

              for {
                uz <- zs.follow(SDir(Nil))
                az <- (LinkNode(rz) :: uz).ascend
              } yield az

            }
          }

        }
      }
    }

    // Enter a box along a given direction until the first
    // internal dot is encountered
    def descend(dir: SDir): Except[LinkZipper[A]] = {

      println("(Descend) " + fociString)

      lz match {
        case Nil => succeed(Nil)
        case LinkRoot(_) :: zs => throwError("Attempting to descend at the root")
        case LinkLeaf(n, _) :: zs => {

          // Note: a sanity check would be that d == dir ...

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

              // println("Descending finished at: " + a.toString)

              succeed(lz)

            }
            case SBox(a, SLeaf) => {

              // We are passing through a loop.

              // println("Descending past loop: " + a.toString)

              lz.rewind

            }
            case SBox(a, cn) => {

              // println("Descending into box: " + a.toString)

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

                    // Now, we have entered the box at the appropriate
                    // child.  Continue descending along the last direction
                    // for the leaf.
                    rz <- (LinkNode(nz) :: uz).descend(cz.address.head)

                  } yield rz

                }
                case _ => throwError("Unexpected address list")
              })

            }
          }

        }
      }
    }

    // Trace the edge emanating in the given direction until a dot is reached
    def follow(dir: SDir): Except[LinkZipper[A]] = {

      println("(Follow) " + fociString)

      lz match {
        case Nil => succeed(Nil)
        case LinkLeaf(_, _) :: zs => throwError("Attempting to follow at a leaf")
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

            zs.rewind.map(LinkLeaf(z.close, dir) :: _    )

          }

        }
      }
    }

    // Trace backwards along the root edge emanating from this cell
    def rewind: Except[LinkZipper[A]] = {

      println("(Rewind) " + fociString)

      lz match {
        case Nil => succeed(Nil)
        case LinkRoot(_) :: zs => throwError("Attempting to rewind at a root")
        case LinkLeaf(n, d) :: zs => {

          // Descend back along the stored direction.
          for {
            uz <- zs.ascend
            rz <- (LinkNode(SNstZipper(n)) :: uz).descend(d)
          } yield rz

        }
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

  }


  def link[A](cmplx: SComplex[A], fa: FaceAddr) : Option[SComplex[A]] = None

}

