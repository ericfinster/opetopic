/**
  * Link.scala - Calculating the link
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

object Link {


  def link[A](c: SComplex[A], fa: FaceAddr): Option[SComplex[A]] = {

    val codim = fa.codim
    val addr = fa.address

    // def linkStep(zp: SNstZipper[(A, Boolean)]): Option[SNstZipper[(A, Boolen)]] =
    //   zp match {

    //   }

    c.grab(fa.codim) match {
      case (fcmplx, nst :: _) => {
        for {
          bd <- SCmplxZipper(fcmplx).focusDeriv[Boolean]
          bn = fcmplx.head.mapWithAddr({
            case (_, baddr) => baddr == addr
          })
          // _ = println("Boolean coface nesting: " + bn.toString)
          cofaces <- nst.bondTraverse(bn, bd)({
            case (a, srcBs, tgtB) => {
              Some((a, tgtB || srcBs.toList.exists(b => b)))
            }
          })

          mp <- (fcmplx >> nst).bondingMap

          pr <- addr match {
            case Nil => Some((SNstZipper(cofaces), true))
            case d :: ds => cofaces.seek(mp(ds)).map(z => (z, false))
          }

          (zp, asc) = pr

          _ = println("Starting link calculation at: " + zp.focus.baseValue.toString)

          rz <- traceLink(zp, asc)({ case (_, b) => b }) match {
            case None => { println("There was an error") ; None }
            case Some(z) => { println("Trace complete") ; Some(z) }
          }

          // This looks good.  The next step is to have a kind of loop
          // which steps through the cofaces in the proper direction and
          // calls some function as we go so that we can watch the progress.

          // So I think you are going to want to do this on the nesting but
          // appended with the boolean flag.  Let's try this

        } yield c
      }
      case _ => None
    }

  }

  def traceLink[A](z: SNstZipper[A], ascending: Boolean)(f: A => Boolean): Option[SNstZipper[A]] =
    if (ascending) {
      z.focus match {
        case SDot(a) => {
          println("Finished at: " + a.toString)
          Some(z)
        }
        case SBox(a, SLeaf) => {
          println("Crossing box: " + a.toString)
          traceLink(z, false)(f)
        }
        case SBox(a, SNode(n, vs)) => {
          println("Entering box: " + a.toString)
          traceLink(SNstZipper(n, SNstCtxt((a, SDeriv(vs)) :: z.ctxt.g)), true)(f)
        }
      }
    } else {
      z.ctxt.g match {
        case (a, SDeriv(verts, _)) :: cs => {

          verts.mapWithAddr({
            case (SLeaf, addr) => None
            case (SNode(n, sh), addr) =>
              if (f(n.baseValue)) Some(addr) else None
          }).toList.filter(_.isDefined) match {
            case Nil => {
              // In this case, there is no outgoing guy and
              // we are going to move on to the next.

              if (f(a)) {
                println("Descending past " + a.toString)
                z.parent.flatMap(p => traceLink(p, false)(f))
              } else {
                println("Error: parent is not a coface in descent")
                None
              }

            }
            case dOpt :: _ => {
              z.sibling(SDir(dOpt.get)).flatMap(s => traceLink(s, true)(f))
            }
          }

        }
        case Nil => {
          println("Exiting nesting at " + z.focus.baseValue.toString)
          Some(z)
        }
      }
    }

  // Given a predicate, search for the first sibling which satisfies it,
  // or else given back the parent, if it satisfies it.
  def siblingWhich[A](n: SNstZipper[A])(f: A => Boolean): Option[SNstZipper[A]] =
    n.ctxt.g match {
      case (a, SDeriv(verts, _)) :: cs => {

        // Idea: map with the address over the verticals, the use the "sibling"
        // function above to step in that direction if a match is found.  Otherwise,
        // test that the parent satisfies the predicate and step there

        verts.mapWithAddr({
          case (SLeaf, addr) => None
          case (SNode(n, sh), addr) =>
            if (f(n.baseValue)) Some(addr) else None
        }).toList.filter(_.isDefined) match {
          case Nil => if (! f(a)) None else n.parent
          case dOpt :: _ => n.sibling(SDir(dOpt.get))
        }

      }
      case _ => None
    }


}
