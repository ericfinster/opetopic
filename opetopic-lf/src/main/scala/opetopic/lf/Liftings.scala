/**
  * Expr.scala - Some basic ideas about generic lifting rules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.lf

import opetopic._
import opetopic.mtl._

// trait LiftValidator {

//   def validateLeftLift(web: SComplex[Expr], pd: STree[Expr]): Except[Unit] 
//   def validateRightLift(web: SComplex[Expr], deriv: SDeriv[Expr], tgt: Expr): Except[Unit] 

// }

// object PalmValidator {

//   def isUniversal(e: Expr): Boolean =
//     e match {
//       case Obj => false
//       case Var(frame, id) => false
//       case LeftComp(web, pd) => pd.forall(isUniversal(_))
//       case LeftFill(web, pd) => true
//       case RightComp(web, deriv, tgt) => isUniversal(tgt)
//       case RightFill(web, deriv, tgt) => true
//     }

//   def validateLeftLift(web: SComplex[Expr], pd: STree[Expr]): Except[Unit] =
//     succeed(())

//   // This should be rewritten to have much better error reporting
//   def validateRightLift(web: SComplex[Expr], deriv: SDeriv[Expr], tgt: Expr): Except[Unit] = {

//     // def checkShell(sh: STree[STree[Expr]]): Except[Unit] =
//     //   for {
//     //     _ <- sh.traverse(_.traverse(expr =>
//     //       verify(isUniversal(expr),
//     //         "Expression: " + expr.toString + " is not universal."
//     //       )))
//     //   } yield ()

//     // def checkCtxt(g: List[(Expr, SDeriv[STree[Expr]])]): Except[Unit] =
//     //   g match {
//     //     case Nil => succeed(())
//     //     case (e, d) :: h =>
//     //       for {
//     //         _ <- verify(e.hasSourceLiftingAt(d.g.address),
//     //           "Expression: " + e.toString + " does not have source lifting at " + d.g.address)
//     //         _ <- checkShell(d.plug(SLeaf))
//     //         _ <- checkCtxt(h)
//     //       } yield ()
//     //   }

//     // if (web.dim >= 2)
//     //   succeed(())  // Kan range ...
//     // else for {
//     //   _ <- checkShell(deriv.sh)
//     //   _ <- checkCtxt(deriv.g.g)
//     // } yield ()

//     ???

//   }

// }



// trait LiftSpec {

//   def hasTargetLifting(e: Expr): Boolean
//   def hasSourceLiftingAt(e: Expr, addr: SAddr): Boolean

// }

// object PalmLiftSpec {

//     def hasTargetLifting(e: Expr): Boolean =
//       e match {
//         case Obj => false
//         case Var(frame, id) => false
//         case LeftComp(web, pd) => pd.forall(hasTargetLifting(_))
//         case LeftFill(web, pd) => true
//         case RightComp(web, deriv, tgt) => {
//           // Is the following sufficient?
//           hasTargetLifting(tgt)

//           // The question is what are the assumptions in this routine.
//           // If, for example, we are assuming that this is already a
//           // validated lift, then we should have already made sure that
//           // there is source lifting all the way to any base cell below.

//           // I guess the one question left in my head is, if there is a
//           // root cell down there, should it *also* have target lifting?
//           // Or is target lifting of the tgt cell enough?

//         }
//         case RightFill(web, deriv, tgt) => true
//       }

//     def hasSourceLiftingAt(e: Expr, addr: SAddr): Boolean =
//       e match {
//         case Obj => false
//         case Var(frame, id) => false
//         case LeftComp(web, pd) => {

//           // Well, is this going to be the same?  I guess we should put it
//           // somewhere in that case ....
//           def checkCtxt(g: List[(Expr, SDeriv[STree[Expr]])]): Except[Unit] =
//             g match {
//               case Nil => succeed(())
//               case (e, d) :: h =>
//                 for {
//                   _ <- verify(e.hasSourceLiftingAt(d.g.address),
//                     "Expression: " + e.toString + " does not have source lifting at " + d.g.address)
//                   // _ <- checkShell(d.plug(SLeaf))  // ***  Leaving this out for now. See below.
//                   _ <- checkCtxt(h)
//                 } yield ()
//             }

//           // *** Well, exactly.  That's now kind of an interesting question.  If this
//           // left comp is supposed to have source lifting, should I *also* verify
//           // that all the other cells along the path back were target universal?

//           val m =
//             for {
//               vAddr <- attempt(pd.horizToVertAddr(addr), "Faile to find incoming leaf.")
//               z <- attempt(pd.seekTo(vAddr), "Failed seek for incoming leaf.")
//             } yield ???

//           // Basically should be when the wire all the way to
//           // the root following this input direction has the
//           // appropriate source liftings.  We'll need a routine
//           // which picks out the internal address somehow....

//           // I'm sure we are going to need this routine, so maybe
//           // you should add it...

//           ???
//         }
//         case LeftFill(web, pd) => false
//         case RightComp(web, deriv, tgt) => ???
//         case RightFill(web, deriv, tgt) => {
//           ???
//         }
//       }
  
// }


