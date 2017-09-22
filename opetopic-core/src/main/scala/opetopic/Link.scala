/**
  * Link.scala - Calculating the link
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

trait LinkTracer[A] {

  sealed trait LinkMarker
  case class LinkLeaf(n: SNesting[A], d: SDir) extends LinkMarker
  case class LinkRoot(n: SNesting[A]) extends LinkMarker
  case class LinkNode(z: SNstZipper[A]) extends LinkMarker

  // So our little zipper guy is a list of markers
  type LinkZipper = List[LinkMarker]

  //============================================================================================
  // CALLBACKS
  //

  sealed trait LinkStep
  case class AtLeaf(dir: SDir) extends LinkStep
  case object AtRoot extends LinkStep

  def step(src: LinkMarker, dest: LinkMarker, codim: Int) : Except[Unit]

  // def enterRoot(nz: SNstZipper[A], lz: LinkZipper): Except[Unit]
  // def enterLeaf(nz: SNstZipper[A], lz: LinkZipper, dir: SDir): Except[Unit]

  // def exitRoot(nz: SNstZipper[A], lz: LinkZipper): Except[Unit]
  // def exitLeaf(nz: SNstZipper[A], lz: LinkZipper, dir: SDir): Except[Unit]

  // def ascendLoop(nz: SNstZipper[A], lz: LinkZipper): Except[Unit]
  // def descendLoop(nz: SNstZipper[A], lz: LinkZipper): Except[Unit]

  //============================================================================================
  // ZIPPER OPERATIONS
  //

  implicit class LinkZipperOps(lz: LinkZipper) {

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

    def seek(addr: SAddr): Except[LinkZipper] =
      lz match {
        case Nil => succeed(Nil)
        case LinkLeaf(_, _) :: zs => throwError("Visiting leaf")
        case LinkRoot(n) :: zs => {

          for {
            uz <- zs.ascend
            rz <- (LinkNode(SNstZipper(n)) :: uz).seek(addr)
          } yield rz

        }
        case LinkNode(z) :: zs => {

          addr match {
            case Nil => succeed(lz)
            case d :: ds => {

              lz.seek(ds).flatMap({
                case LinkNode(sz) :: usz => {
                  for {
                    nz <- attempt(sz.visit(d), "Visit failed")
                    uz <- usz.follow(d)
                  } yield LinkNode(nz) :: uz
                }
                case _ => throwError("Unexpected seek result")
              })

            }
          }

        }
      }

    // Enter a box along the root edge, continuing until
    // the internal root dot is reached.
    def ascend: Except[LinkZipper] = {

      // println("(Ascend) " + fociString)

      lz match {
        case Nil => succeed(Nil)
        case LinkLeaf(_, _) :: zs => throwError("Attempting to ascend at a leaf")
        case (src @ LinkRoot(n)) :: zs => {

          // We are trying to ascend in from a root.  Recurse up the complex
          // and then ascend into the trivial zipper.
          val dest = LinkNode(SNstZipper(n))

          for {
            _ <- step(src, dest, zs.length)
            uz <- zs.ascend
            az <- (dest :: uz).ascend
          } yield az

        }
        case (src @ LinkNode(z)) :: zs => {

          z.focus match {
            case SDot(a) => succeed(lz)
            case SBox(a, SLeaf) => {

              // We have arrived at a loop.  We should pass through
              // the other side.  I think this should mean following
              // the nil direction.

              for {
                _ <- step(src, src, zs.length)
                rz <- lz.follow(SDir(Nil))
              } yield rz

            }
            case SBox(a, SNode(n, sh)) => {

              // There is a further box to attempt to ascend.  Construct
              // the appropriate zipper and continue.

              val dest = LinkNode(SNstZipper(n, (a, SDeriv(sh)) :: z.ctxt))

              for {
                _ <- step(src, dest, zs.length)
                uz <- zs.follow(SDir(Nil))
                az <- (dest :: uz).ascend
              } yield az

            }
          }

        }
      }
    }

    // Enter a box along a given direction until the first
    // internal dot is encountered
    def descend(dir: SDir): Except[LinkZipper] = {

      // println("(Descend) " + fociString)

      lz match {
        case Nil => succeed(Nil)
        case LinkRoot(_) :: zs => throwError("Attempting to descend at the root")
        case (src @ LinkLeaf(n, _)) :: zs => {

          // Note: a sanity check would be that d == dir ...

          // To descend from a leaf, we should ascend in the upper
          // complex and the descend using the Node case on the trivial zipper.

          val dest = LinkNode(SNstZipper(n))

          for {
            _ <- step(src, dest, zs.length)
            uz <- zs.ascend
            az <- (dest :: uz).descend(dir)
          } yield az

        }
        case (src @ LinkNode(z)) :: zs => {

          z.focus match {
            case SDot(a) => succeed(lz)
            case SBox(a, SLeaf) => {

              // We are passing through a loop.

              for {
                _ <- step(src, src, zs.length)
                rz <- lz.rewind
              } yield rz

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
                    nz <- attempt(z.visit(vDir), "Local visit failed")
                    dest = LinkNode(nz)

                    _ <- step(src, dest, zs.length)

                    uz <- zs.follow(vDir)

                    // Now, we have entered the box at the appropriate
                    // child.  Continue descending along the last direction
                    // for the leaf.
                    rz <- (dest :: uz).descend(cz.address.head)

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
    def follow(dir: SDir): Except[LinkZipper] = {

      // println("(Follow) " + fociString)

      lz match {
        case Nil => succeed(Nil)
        case LinkLeaf(_, _) :: zs => throwError("Attempting to follow at a leaf")
        case LinkRoot(_) :: zs => throwError("Attempting to follow at a root")
        case (src @ LinkNode(z)) :: zs => {

          if (z.hasParent) {
            z.sibling(dir) match {
              case Some(sibZip) => {

                // This should be the direction of the newly found sibling
                // with respect to its parent.

                val myDir = sibZip.address.head
                val dest = LinkNode(sibZip)

                for {
                  _ <- step(src, dest, zs.length)

                  rz <- zs.rewind
                  pz <- rz.follow(myDir)
                  az <- (dest :: pz).ascend
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
                  dest = LinkNode(p)

                  cn <- attempt(p.focus.boxOption.map(_._2), "No canopy")
                  addrNst <- attempt(
                    cn.treeFold[SNesting[SAddr]](a => Some(SDot(a)))((_, nwCn) => Some(SBox(Nil, nwCn))),
                    "Tree fold failed"
                  )
                  az <- attempt(addrNst.seek(lfAddr), "Invalid leaf address")
                  extAddr <- attempt(az.focus.dotOption, "Leaf was not a dot")

                  _ <- step(src, dest, zs.length)

                  // Now continue along that address in the parent
                  uz <- zs.rewind
                  az <- (LinkNode(p) :: uz).follow(SDir(extAddr))

                } yield az

              }
            }
          } else {

            // If we are at the base box, we exit to a leaf marker and
            // rewind the rest of the zipper
            val dest = LinkLeaf(z.close, dir)

            for {
              _ <- step(src, dest, zs.length)
              rz <- zs.rewind
            } yield (dest :: rz)

          }

        }
      }
    }

    // Trace backwards along the root edge emanating from this cell
    def rewind: Except[LinkZipper] = {

      // println("(Rewind) " + fociString)

      lz match {
        case Nil => succeed(Nil)
        case LinkRoot(_) :: zs => throwError("Attempting to rewind at a root")
        case (src @ LinkLeaf(n, d)) :: zs => {

          // Descend back along the stored direction.
          val dest = LinkNode(SNstZipper(n))

          for {
            _ <- step(src, dest, zs.length)
            uz <- zs.ascend
            rz <- (dest :: uz).descend(d)
          } yield rz

        }
        case (src @ LinkNode(z)) :: zs => {

          if (z.hasParent) {

            z.predecessor match {
              case Some(predZip) => {


                // Here we want to "turn the corner" as it were.  So we need
                // to calculate the address we are coming from and then descend
                // in that direction ...

                val descDir = z.ctxt.g.head._2.g.address.head
                val dest = LinkNode(predZip)

                for {
                  _ <- step(src, dest, zs.length)

                  uz <- zs.rewind
                  az <- uz.follow(predZip.address.head)
                  rz <- (dest :: az).descend(descDir)
                } yield rz

              }
              case None => {

                // If there is no predecessor, we rewind in higer dimensions
                // passing to the parent and continue recursively.

                for {
                  p <- attempt(z.parent, "Failed to get parent")
                  dest = LinkNode(p)

                  _ <- step(src, dest, zs.length)

                  uz <- zs.rewind
                  rz <- (dest :: uz).rewind
                } yield rz

              }
            }

          } else {

            // Exit to the root and rewind the rest
            val dest = LinkRoot(z.close)

            for {
              _ <- step(src, dest, zs.length)
              rz <- zs.rewind
            } yield dest :: rz

          }

        }
      }
    }

  }


}

// object Link {

//   def link[A](fa: FaceAddr, cmplx: SComplex[A]): Except[SComplex[FaceAddr]] = {


//     val (lower, upper) = cmplx.grab(fa.codim)

//     upper match {
//       case Nil => throwError("No link for top cell.")
//       case hd :: Nil => {

//         // This should mean that we are taking the link of an
//         // immediate face.  This is always an object.  Simply
//         // return it, since there is nothing to do.
//         succeed(||(SDot(ThisDim(Nil))))

//       }
//       case hd :: tl => {

//         var calculating = false
//         var cardinal: SCardinal[FaceAddr] =
//           SCardinal(ThisDim(Nil))

//         object Tracer extends LinkTracer[A] {

//           def done = succeed(())

//           def enterRoot(nz: SNstZipper[A], lz: LinkZipper): Except[Unit] =
//             if (calculating) {

//               if (lz.length > 0) {
//                 // We should be in the base case
//                 println("Entering base cell: " + nz.focus.baseValue.toString)

//                 if (! nz.focus.isDot) { 

//                   // So, last thing is how to calculate the correct addresses.
//                   // And what's the idea?

//                   // Right, well, the issue is that what we actually want is
//                   // the *next* zipper, not the current one.

//                   for {
//                     newCard <- attempt(
//                       cardinal.extrudeObject(ThisDim(Nil), ThisDim(Nil)),
//                       "Extrusion failed"
//                     )
//                   } yield {
//                     println("Extrusion successful")
//                   }

//                 } else done

//               } else {
//                 // We should be in the upper case
//                 // println("Entering upper cell: " + nz.focus.baseValue.toString)
//                 done
//               }

//             } else done

//           def exitRoot(nz: SNstZipper[A], lz: LinkZipper): Except[Unit] = 
//             if (calculating) {

//               if (lz.length > 0) {
//                 // We should be in the base case
//                 println("Exiting base cell: " + nz.focus.baseValue.toString)
//                 if (nz.hasParent && ! nz.focus.isLoop) { 

//                   for {
//                     newCard <- attempt(
//                       cardinal.extrudeObject(ThisDim(Nil), ThisDim(Nil)),
//                       "Extrusion failed"
//                     )
//                   } yield {
//                     println("Extrusion successful")
//                   }

//                 } else done


//               } else {
//                 // We should be in the upper case
//                 // println("Exiting upper cell: " + nz.focus.baseValue.toString)
//                 done
//               }

//             } else done


//           def ascendLoop(nz: SNstZipper[A], lz: LinkZipper): Except[Unit] = 
//             if (calculating) {

//               if (lz.length > 0) {
//                 // We should be in the base case
//                 println("Passing base loop: " + nz.focus.baseValue.toString)
//                 done
//               } else {
//                 // We should be in the upper case
//                 // println("Passing upper loop: " + nz.focus.baseValue.toString)
//                 done
//               }

//             } else done


//           // These guys just repeat the above
//           def enterLeaf(nz: SNstZipper[A], lz: LinkZipper, dir: SDir): Except[Unit] =
//             enterRoot(nz, lz)

//           def exitLeaf(nz: SNstZipper[A], lz: LinkZipper, dir: SDir): Except[Unit] =
//             exitRoot(nz, lz)

//           def descendLoop(nz: SNstZipper[A], lz: LinkZipper): Except[Unit] =
//             ascendLoop(nz, lz)
          
//         }

//         import Tracer._

//         // Okay, this is where the action is.  We want to do the same as below,
//         // but focus on the completely trivial case.  Also, I guess you want to
//         // go ahead and try to actually do the extrusions.  This should be much
//         // more interesting.

//         val triple = List(lower.head, hd, tl.head).map(LinkRoot(_))

//         // Right, so we are basically only interested in traversing these guys.
//         // The coface tagging only needs to happen on the last entry.  But I
//         // guess we do need to do this, right?

//         for {
//           z <- triple.seek(fa.address)

//           _ = println("Arrived at link cell.")
//           _ = z.printFoci

//           // Turn on callbacks ...
//           _ = calculating = true

//           // Set the correct initial address in the cardinal
//           _ <- z.tail.head match {
//             case LinkRoot(_) => throwError("Malformed link zipper")
//             case LinkLeaf(_, _) =>
//               // Should be the root, just need the right codimension
//               succeed({ cardinal = SCardinal(FaceAddr(fa.codim - 1, Nil)) }) 
//             case LinkNode(nz) =>
//               // Use the address of this zipper
//               succeed({ cardinal = SCardinal(FaceAddr(fa.codim - 1, nz.address)) }) 
//           }

//           rz <- z.tail.rewind

//         } yield {

//           println("Link trace complete.")
//           ||(SDot(ThisDim(Nil)))

//         }

//       }
//     }

//   }

// }

// class LinkCalculator[A](fa: FaceAddr, cmplx: SComplex[A]) extends LinkTracer[A] {

//   var calculating = false
//   def done: Except[Unit] = succeed(())

//   var boolComplex:SComplex[Boolean] =
//     ||(SDot(false))

//   def link: Except[SComplex[A]] = {

//     val (lower, upper) = cmplx.grab(fa.codim)

//     cmplx.traverseCofacesOf[Boolean](fa)({
//       case Left(_) => Some(true)
//       case Right(_) => Some(false)
//     }) match {
//       case None => throwError("Boolean initialization failed!")
//       case Some(boolUpper) => {

//         // Initialize the boolean state complex
//         boolComplex = (lower: SComplex[A]).map(_ => true) ++
//           (boolUpper.head.map(_ => true) :: boolUpper.tail)

//         val lz = (lower.head :: upper).map(LinkRoot(_))

//         for {
//           z <- (lower.head :: upper).map(LinkRoot(_)).seek(fa.address)

//           _ = println("Arrived at link cell.")
//           _ = z.printFoci

//           // Turn on callbacks ...
//           _ = calculating = true

//           // Now, we should toss out the lowest dimension
//           // and rewind? descend? the tail.

//           rz <- z.tail.rewind

//         } yield {

//           println("Link trace complete.")
//           cmplx // Dummy return

//         }

//       }
//     }


//   }

//   def updateAt(fa: FaceAddr): Option[Boolean] = {

//     val (lower, upper) = boolComplex.grab(fa.codim)

//     for {
//       el <- cmplx.elementAt(fa)
//       face <- boolComplex.face(fa)
//       frm <- face.cellFrame
//       (srcs, tgt) = frm

//       isComplete = (tgt :: srcs.toList).forall(b => b) && (! face.head.baseValue)

//       newNst <- if (isComplete) {
//         println("Completed a face! Element: " + el.toString)
//         lower.head.replaceAt(fa.address, true)
//       } else Some(lower.head)

//     } yield {

//       boolComplex = lower.withHead(newNst) ++ upper
//       isComplete

//     }

//   }

//   def enterRoot(nz: SNstZipper[A], lz: LinkZipper): Except[Unit] = 
//     if (calculating && (lz.length == fa.codim - 1)) {
//     // if (calculating) {
//       println("Entering root of: " + nz.focus.baseValue.toString)
//       attempt(updateAt(FaceAddr(lz.length, nz.address)), "Update failed").map(_ => ())
//     } else done


//   def exitRoot(nz: SNstZipper[A], lz: LinkZipper): Except[Unit] =
//     if (calculating) {

//       if (lz.length == fa.codim -1) {
//         println("Exiting root of: " + nz.focus.baseValue.toString)
//       }

//       for {
//         isComplete <- attempt(updateAt(FaceAddr(lz.length, nz.address)), "Update failed")
//       } yield ()

//     } else done


//   def ascendLoop(nz: SNstZipper[A], lz: LinkZipper): Except[Unit] =
//     if (calculating) {

//       println("Passing a loop: " + nz.focus.baseValue.toString)

//       for {
//         isComplete <- attempt(updateAt(FaceAddr(lz.length, nz.address)), "Update failed")
//       } yield ()
      
//     } else done


//   // These guys just repeat the above
//   def enterLeaf(nz: SNstZipper[A], lz: LinkZipper, dir: SDir): Except[Unit] =
//     enterRoot(nz, lz)

//   def exitLeaf(nz: SNstZipper[A], lz: LinkZipper, dir: SDir): Except[Unit] =
//     exitRoot(nz, lz)

//   def descendLoop(nz: SNstZipper[A], lz: LinkZipper): Except[Unit] =
//     ascendLoop(nz, lz)

// }
