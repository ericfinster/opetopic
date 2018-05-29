/**
  * Flags.scala - Trace the flags of a given complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

object Flags {

  sealed trait Facet[A] {
    val focus: SNstZipper[A]
    val isSrc: Boolean
  }
  case class TgtFacet[A](val focus: SNstZipper[A]) extends Facet[A] { val isSrc = false }
  case class SrcFacet[A](val focus: SNstZipper[A], dir: SDir) extends Facet[A] { val isSrc = true }

  type FlagZipper[A] = List[Facet[A]]

  object FlagZipper {

    // Creating this guy should be as simple as a map
    def apply[A](cmplx: SComplex[A]): FlagZipper[A] =
      cmplx.asList.map((n : SNesting[A]) => TgtFacet(SNstZipper(n)))

    def next[A](flagz: FlagZipper[A]): Except[Option[FlagZipper[A]]] =
      flagz match {
        case Nil => succeed(None)
        case _ :: Nil => succeed(None)
        case f :: g :: gs =>
          (if (f.isSrc) prev(g :: gs) else next(g :: gs)).flatMap(r => 
            if (! r.isDefined) {
              if (g.focus.focus.isDot) {

                (f, g) match {
                  case (SrcFacet(fz, d), TgtFacet(gz)) => succeed(None) // Blocked
                  case (TgtFacet(fz), SrcFacet(gz, d)) => succeed(None) // Blocked
                  case (SrcFacet(fz, d), SrcFacet(gz, _)) => {

                    if (fz.hasParent) {

                      fz.sibling(d) match {
                        case None => {

                          // Okay, this should mean we need to pass to the parent
                          // This is where we need to do some kind of address calculation
                          // in order to generate the next direction in the parent.

                          // This is the address of the leaf we're looking for
                          // in the canopy of our parent
                          val lfAddr = d :: fz.ctxt.g.head._2.g.address

                          for {
                            // First calculate the external address
                            p <- attempt(fz.parent, "Failed to get parent")

                            cn <- attempt(p.focus.boxOption.map(_._2), "No canopy")
                            addrNst <- attempt(
                              cn.treeFold[SNesting[SAddr]](a => Some(SDot(a)))((_, nwCn) => Some(SBox(Nil, nwCn))),
                              "Tree fold failed"
                            )

                            az <- attempt(addrNst.seek(lfAddr), "Invalid leaf address")
                            extAddr <- attempt(az.focus.dotOption, "Leaf was not a dot")
                          } yield {
                            // Now, we should pass to the parent.
                            Some(SrcFacet(p, SDir(extAddr)) :: TgtFacet(gz) :: gs)
                          }

                        }
                        case Some(sz) => {

                          // And this should mean we pass to the sibling.
                          // We just need to know the new address in the
                          // next dimension.  What is it?

                          val srcDir = sz.address.head

                          succeed(Some(TgtFacet(sz) :: SrcFacet(gz, srcDir) :: gs))
                        }
                      }
                    } else {
                      // This should mean we are at an external guy.
                      // No possible way to advance.
                      succeed(None)
                    }


                  }
                  case (TgtFacet(fz), TgtFacet(gz)) => {
                    fz.focus match {
                      case SDot(_) => succeed(None) // Blocked
                      case SBox(_, SLeaf) => {
                        // Crossing a loop
                        succeed(Some(SrcFacet(fz, SDir(Nil)) :: TgtFacet(gz) :: gs))
                      }
                      case _ => {

                        // Default is now a box with non-trivial canopy
                        for {
                          rz <- attempt(fz.visit(SDir(Nil)), "Internal error")
                        } yield {
                          // Okay, we remain a target, but with a new focus
                          // while g keeps its focus but becomes the first src
                          Some(TgtFacet(rz) :: SrcFacet(gz, SDir(Nil)) :: gs)
                        }

                      }
                    }
                  }
                }

              } else {
                // The only possible way we can move is if the next dimension
                // is a dot.  So this is the first thing to exclude
                succeed(None)
              }

            } else {
              // If a higher dimension succeeds, we prepend the head in
              // this dimension.  Hence the length of the flag is always
              // preserved.
              succeed(r.map(gr => f :: gr))
            }
          )
      }

    def prev[A](flagz: FlagZipper[A]): Except[Option[FlagZipper[A]]] =
      flagz match {
        case Nil => succeed(None)
        case _ :: Nil => succeed(None)
        case f :: g :: gs =>
          (if (f.isSrc) next(g :: gs) else prev(g :: gs)).flatMap(r => 
            if (! r.isDefined) {
              if (g.focus.focus.isDot) {

                (f, g) match {
                  case (SrcFacet(fz, d), TgtFacet(gz)) => {

                    // The case would be entering along a source line
                    // or a loop.

                    fz.focus match {
                      case SDot(_) => succeed(None) // Blocked
                      case SBox(_, SLeaf) => {
                        // Traverse a loop
                        succeed(Some(TgtFacet(fz) :: TgtFacet(gz) :: gs))
                      }
                      case SBox(a, cn) => {

                        // We need to pass to the leaf corresponding to
                        // the edge we're coming in on ...

                        attempt(
                          STree.treeFold[SNesting[A], List[SAddr]](cn)((hAddr, vAddr) =>
                            if (hAddr == d.dir) Some(List(vAddr)) else Some(Nil)
                          )((_, _, lstTr) => Some(lstTr.toList.flatten)),
                          "Edge address map calculation failed."
                        ).flatMap({
                          case (lfDir :: addr) :: Nil => {

                            // The resulting address splits into the address of
                            // the box we are looking for and the direction of the
                            // associated leaf.

                            // We could, of course, be a bit more thorough in checking
                            // that the result of the calculation is valid ....

                            for {
                              vz <- attempt(fz.visit(SDir(addr)), "Local visit failed")  
                            } yield {
                              Some(SrcFacet(vz, lfDir) :: SrcFacet(gz, SDir(addr)) :: gs)
                            }

                          }
                          case _ => throwError("Unexpected address list")
                        })

                      }
                    }

                  }
                  case (TgtFacet(fz), SrcFacet(gz, d)) => {

                    // Pass to either a parent or predecessor
                    if (fz.hasParent) {

                      fz.predecessor match {
                        case None => {

                          // Right, so pass to the parent.
                          for {
                            pz <- attempt(fz.parent, "No parent")
                          } yield {
                            Some(TgtFacet(pz) :: TgtFacet(gz) :: gs)
                          }

                        }
                        case Some(pz) => {

                          // And finally, just need to calculate
                          // the correct address ....

                          val lfAddr = fz.ctxt.g.head._2.g.address.head

                          succeed(Some(SrcFacet(pz, lfAddr) :: SrcFacet(gz, pz.address.head) :: gs))

                        }
                      }

                    } else {
                      // We are at a target box. No way to back up.
                      succeed(None)
                    }

                  }
                  case (SrcFacet(fz, d), SrcFacet(gz, _)) => succeed(None)  // Blocked
                  case (TgtFacet(fz), TgtFacet(gz)) => succeed(None)        // Blocked
                }

              } else succeed(None)
            } else succeed(r.map(gr => f :: gr))
          )
      }

  }

}
