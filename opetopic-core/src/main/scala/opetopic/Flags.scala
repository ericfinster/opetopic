/**
  * Flags.scala - Trace the flags of a given complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

sealed trait SignedList[+A] {

  def :-:[B >: A](b: B): SignedList[B] =
    new :-:(b, this)

  def :+:[B >: A](b: B): SignedList[B] =
    new :+:(b, this)

}

case object <> extends SignedList[Nothing]
case class :-:[+A](a: A, sl: SignedList[A]) extends SignedList[A]
case class :+:[+A](a: A, sl: SignedList[A]) extends SignedList[A]

object SignedList {

  implicit class SignedListOps[A](sl: SignedList[A]) {

    def head: A =
      sl match {
        case <> => ???
        case a :-: _ => a
        case a :+: _ => a
      }

    def tail: SignedList[A] =
      sl match {
        case <> => ???
        case _ :-: tl => tl
        case _ :+: tl => tl
      }

    def sign: Boolean =
      sl match {
        case <> => true
        case _ :-: tl => ! (tl.sign)
        case _ :+: tl => tl.sign
      }

  }

}

object FlagTraverse {

  type FlagPtr[A] = SignedList[SNstZipper[A]]

  sealed trait RidgeType
  case object SrcSrc extends RidgeType
  case object SrcTgt extends RidgeType
  case object TgtSrc extends RidgeType
  case object TgtTgt extends RidgeType

  case class FlagZipper[A](
    val lower: FlagPtr[A],
    val focus: SNstZipper[A],
    val upper: FlagPtr[A],
    val ridgeType: RidgeType
  )

  implicit class FlagZipperOps[A](fz: FlagZipper[A]) {

    def hasNextDim: Boolean =
      fz.upper != <>

    def hasPrevDim: Boolean =
      fz.lower != <>

    def nextDim: Except[FlagZipper[A]] =
      fz match {
        case FlagZipper(l, f, <>, rt) => throwError("nextDim")
        case FlagZipper(l, f, u :-: us, SrcSrc) => succeed(FlagZipper(f :-: l, u, us, SrcSrc))
        case FlagZipper(l, f, u :-: us, SrcTgt) => succeed(FlagZipper(f :-: l, u, us, TgtSrc))
        case FlagZipper(l, f, u :-: us, TgtSrc) => succeed(FlagZipper(f :+: l, u, us, SrcSrc))
        case FlagZipper(l, f, u :-: us, TgtTgt) => succeed(FlagZipper(f :+: l, u, us, TgtSrc))
        case FlagZipper(l, f, u :+: us, rt) => ???
      }

    def prevDim: Except[FlagZipper[A]] = ???

    def sign: Boolean =
      fz.lower.sign

    def advance: Except[FlagZipper[A]] = {

      if (fz.hasNextDim) {

        fz.nextDim.flatMap(nxt =>
          nxt.advance.handle(_ => {

            // With this setup, the next dimension is in scope
            // and we know that handle has failed, since we are
            // inside it's error routine.

            // Hmmm.  Yeah, but this isn't so great, since there could be
            // another reason for the failure.  But anyway, I think you
            // have to finish a first version before you see how to find
            // an optimal implementation.

            val sgn = fz.sign

            if (sgn) {
              //  This means we are trying to move forward.
              (fz.ridgeType, fz.focus.focus) match {
                case (SrcSrc, SDot(_)) => {
                  // Okay, first version.

                  // Possible outcomes are SrcTgt and TgtSrc in accordance with
                  // the requirement that the ridges have opposite sign.

                  // Right, so to get TgtSrc, this means the next guy should be
                  // a dot and we are passing to a sibling.

                  // For SrcTgt, yes, exactly, this means the next guy should be
                  // a dot, and you should be passing to a parent.

                  if (fz.focus.hasParent) {
                    fz.focus.sibling(???) match {
                      case None => {
                        // Yeah, umm, this is a problem because it's a *real* error.
                        // We shouldn't be using return errors to signal how this is
                        // supposed to work ....
                        ???
                      }
                      case Some(s) => {

                        // Even worst, sibling doesn't really distinguish between
                        // hitting a leaf and not having a parent.  Fuckbuckets.

                      }
                    }
                  } else throwError("")


                }
                case (SrcTgt, SDot(_)) => ???
                case (TgtSrc, SDot(_)) => throwError("")  // These two are blocked.
                case (TgtTgt, SDot(_)) => throwError("")
                case (SrcSrc, SBox(_, _)) => ???
                case (SrcTgt, SBox(_, _)) => ???
                case (TgtSrc, SBox(_, _)) => ???
                case (TgtTgt, SBox(_, _)) => ???
              }

            } else {

              //  We are trying to move backwards.

              ???

            }

          })
        )

      } else throwError("")

    }

    // Here is the idea: first, we see if we can raise the dimension.
    // We recursively call the function: if we can advance in a higher
    // dimension, we can advance.
    // If the result is negative for higher dimensions, then we check
    // the current dimension.

    // In the current dimension, we will look at the total sign of our
    // lower piece. This should determine for us which direction we should
    // go.  Based on this direction, there will be a couple of possibilities
    // to check.  And that should be it!


  }

}

