/**
  * Flags.scala - Trace the flags of a given complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

trait FlagTracer[A] { tracer => 

  sealed trait FlagMarker
  case class FlagLeaf(n: SNesting[A], d: SDir) extends FlagMarker
  case class FlagRoot(n: SNesting[A]) extends FlagMarker
  case class FlagNode(z: SNstZipper[A]) extends FlagMarker

  implicit class FlagMarkerOps(mk: FlagMarker) {

    def isNode: Boolean =
      mk match {
        case FlagNode(_) => true
        case _ => false
      }

  }

  sealed trait FlagOp
  case object AscendFirst extends FlagOp
  case object AscendSecond extends FlagOp
  case object DescendFirst extends FlagOp
  case object DescendSecond extends FlagOp
  case object FollowFirst extends FlagOp
  case object FollowSecond extends FlagOp
  case object FollowThird extends FlagOp
  case object FollowFourth extends FlagOp
  case object RewindFirst extends FlagOp
  case object RewindSecond extends FlagOp
  case object RewindThird extends FlagOp
  case object RewindFourth extends FlagOp
  case object RewindFifth extends FlagOp

  def markFlag(lz: FlagZipper, op: FlagOp): Except[Unit]


  // NOTE! - The complex gets reversed in the first part of the zipper
  //         so as not to run into the efficiency problem of appending
  //         elements to lists.

  type FlagZipper = (List[FlagMarker], FlagMarker, List[FlagMarker])


  // Enter a box along the root edge, continuing until
  // the internal root dot is reached.

  def ascend(lz: FlagZipper) : Except[FlagZipper] =
    if (lz.isTopDim)
      succeed(lz)
    else lz.focus match {
      case FlagLeaf(_, _) => throwError("Attempting to ascend at a leaf")
      case FlagRoot(n) =>
        for {
          asc <- lz.withFocus(FlagNode(SNstZipper(n)))(AscendFirst).flatMap(_.inNextDim(ascend))
          res <- asc.ascend
        } yield res
      case FlagNode(z) =>
        z.focus match {
          case SDot(a) => succeed(lz)
          case SBox(a, SLeaf) => lz.follow(SDir(Nil))
          case SBox(a, SNode(n, sh)) => 
            for {
              flag <- lz.withFocus(FlagNode(SNstZipper(n, (a, SDeriv(sh)) :: z.ctxt)))(AscendSecond)
              flw <- flag.inNextDim(follow(SDir(Nil)))
              res <- flw.ascend
            } yield res
        }
    }


  // Enter a box along a given direction until the first
  // internal dot is encountered
  def descend(dir: SDir)(lz: FlagZipper): Except[FlagZipper] =
    if (lz.isTopDim)
      succeed(lz)
    else lz.focus match {
      case FlagRoot(_) => throwError("Attempting to descend at the root")
      case FlagLeaf(n, _) => // We should check that d == dir
        for {
          flag <- lz.withFocus(FlagNode(SNstZipper(n)))(DescendFirst)
          asc <- flag.inNextDim(ascend)
          res <- asc.descend(dir)
        } yield res
      case FlagNode(z) =>
        z.focus match {
          case SDot(a) => succeed(lz)
          case SBox(a, SLeaf) => lz.rewind
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

                  flag <- lz.withFocus(FlagNode(nz))(DescendSecond)
                  flw <- flag.inNextDim(follow(vDir))

                  // Now, we have entered the box at the appropriate
                  // child.  Continue descending along the last direction
                  // for the leaf.

                  res <- flw.descend(cz.address.head)

                } yield res

              }
              case _ => throwError("Unexpected address list")
            })

          }
        }
    }

  // Trace the edge emanating in the given direction until a dot is reached
  def follow(dir: SDir)(lz: FlagZipper): Except[FlagZipper] =
    if (lz.isTopDim)
      succeed(lz)
    else lz.focus match {
      case FlagLeaf(_, _) => throwError("Attempting to follow at a leaf")
      case FlagRoot(_) => throwError("Attempting to follow at a root")
      case FlagNode(z) =>
        if (z.hasParent) {

          z.sibling(dir) match {
            case Some(sibZip) => {
              for {
                rw <- lz.inNextDim(rewind)
                ff <- rw.mark(FollowFirst)
                sf <- ff.withFocus(FlagNode(sibZip))(FollowSecond)
                flw <- sf.inNextDim(follow(sibZip.address.head))
                res <- flw.ascend
              } yield res
            }
            case None => {

              // The lack of a sibling should mean we hit a leaf
              // (Should you check this?), so we calculate the corresponding
              // external direction and then continue in the parent

              for {
                // First calculate the external address
                p <- attempt(z.parent, "Failed to get parent")

                cn <- attempt(p.focus.boxOption.map(_._2), "No canopy")
                addrNst <- attempt(
                  cn.treeFold[SNesting[SAddr]](a => Some(SDot(a)))((_, nwCn) => Some(SBox(Nil, nwCn))),
                  "Tree fold failed"
                )

                // Get from the our leaf address
                lfAddr = dir :: z.ctxt.g.head._2.g.address

                az <- attempt(addrNst.seek(lfAddr), "Invalid leaf address")
                extAddr <- attempt(az.focus.dotOption, "Leaf was not a dot")

                rw <- lz.inNextDim(rewind)
                flag <- rw.withFocus(FlagNode(p))(FollowThird)
                res <- flag.follow(SDir(extAddr))
              } yield res

            }
          }

        } else lz.withFocus(FlagLeaf(z.close, dir))(FollowFourth).flatMap(_.inNextDim(rewind))

    }


  // Trace backwards along the root edge emanating from this cell
  def rewind(lz: FlagZipper): Except[FlagZipper] = 
    if (lz.isTopDim)
      succeed(lz)
    else lz.focus match {
      case FlagRoot(_) => throwError("Attempting to rewind at a root")
      case FlagLeaf(n, d) =>
        for {
          asc <- lz.withFocus(FlagNode(SNstZipper(n)))(RewindFirst).flatMap(_.inNextDim(ascend))
          res <- asc.descend(d)
        } yield res
      case FlagNode(z) => {
        if (z.hasParent) {

          z.predecessor match {
            case Some(predZip) => 
              for {
                rw <- lz.inNextDim(rewind)
                flag <- rw.withFocus(FlagNode(predZip))(RewindThird)
                flw <- flag.inNextDim(follow(predZip.address.head))

                descDir = z.ctxt.g.head._2.g.address.head

                res <- flw.descend(descDir)
              } yield res
            case None => 
              for {
                p <- attempt(z.parent, "Failed to get parent")
                rw <- lz.inNextDim(rewind)
                flag <- rw.withFocus(FlagNode(p))(RewindFourth)
                res <- flag.rewind
              } yield res
          }
        } else lz.withFocus(FlagRoot(z.close))(RewindFifth).flatMap(_.inNextDim(rewind))
      }
    }

  //============================================================================================
  // ZIPPER OPERATIONS
  //

  implicit class FlagZipperOps(lz: FlagZipper) {
    
    def fociString: String =
      foci.mkString(", ")

    def foci: List[String] = 
      lz.close.map({
        case FlagLeaf(n, _) => "Leaf (" ++ n.baseValue.toString ++ ")"
        case FlagRoot(n) => "Root (" ++ n.baseValue.toString ++ ")"
        case FlagNode(z) => z.focus.baseValue.toString
      })

    def stack: List[FlagMarker] = lz._1
    def focus: FlagMarker       = lz._2
    def queue: List[FlagMarker] = lz._3

    def isTopDim: Boolean =
      lz.queue.isEmpty

    def nextDim: Except[FlagZipper] =
      lz match {
        case (s, f, hd :: tl) => succeed(f :: s, hd, tl)
        case _ => throwError("Next dim called with empty queue")
      }

    def prevDim: Except[FlagZipper] =
      lz match {
        case (hd :: tl, f, q) => succeed(tl, hd, f :: q)
        case _ => throwError("Prev dim called with empty stach")
      }

    def inNextDim(f : FlagZipper => Except[FlagZipper]) : Except[FlagZipper] =
      for {
        nxt <- lz.nextDim
        res <- f(nxt)
        prv <- res.prevDim
      } yield prv

    def withFocus(mk: FlagMarker)(op: FlagOp): Except[FlagZipper] =
      (lz._1, mk, lz._3).mark(op)

    def mark(op: FlagOp): Except[FlagZipper] =
      for {
        _ <- markFlag(lz, op)
      } yield lz

    def close: List[FlagMarker] =
      lz._1.reverse ++ (lz.focus :: lz._3)

    def ascend: Except[FlagZipper] =
      tracer.ascend(lz)

    def descend(dir: SDir): Except[FlagZipper] =
      tracer.descend(dir)(lz)

    def rewind: Except[FlagZipper] =
      tracer.rewind(lz)

    def follow(dir: SDir): Except[FlagZipper] =
      tracer.follow(dir)(lz)


    // def seek(addr: SAddr): Except[FlagZipper] =
  //     lz match {
  //       case Nil => succeed(Nil)
  //       case FlagLeaf(_, _) :: zs => throwError("Visiting leaf")
  //       case FlagRoot(n) :: zs => {

  //         for {
  //           uz <- zs.ascend
  //           rz <- (FlagNode(SNstZipper(n)) :: uz).seek(addr)
  //         } yield rz

  //       }
  //       case FlagNode(z) :: zs => {

  //         addr match {
  //           case Nil => succeed(lz)
  //           case d :: ds => {

  //             lz.seek(ds).flatMap({
  //               case FlagNode(sz) :: usz => {
  //                 for {
  //                   nz <- attempt(sz.visit(d), "Visit failed")
  //                   uz <- usz.follow(d)
  //                 } yield FlagNode(nz) :: uz
  //               }
  //               case _ => throwError("Unexpected seek result")
  //             })

  //           }
  //         }

  //       }
  //     }

  }

}
