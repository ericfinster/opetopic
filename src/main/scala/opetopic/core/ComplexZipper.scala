/**
  * ComplexZipper.scala - Zippers for Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

import Nat._
import Tree._
import Nesting._
import Complex._

object ComplexZipper {

//   type ComplexZipper[N <: Nat, A] = DimSeq[NestingZipper, N, A]

//   def EmptyZ[A] : IncSeq[NestingZipper, _0, _0, A] = IncNil[NestingZipper, _0, A]()

//   //============================================================================================
//   // SEAL
//   //

//   def seal[N <: Nat, A](cz : ComplexZipper[N, A]) : Complex[N, A] = 
//     (new NatCaseSplit {

//       type In[M <: Nat] = ComplexZipper[M, A]
//       type Out[M <: Nat] = Complex[M, A]

//       def caseZero(cz : ComplexZipper[_0, A]) : Complex[_0, A] =
//         EmptyC :>> head(cz).close

//       def caseSucc[P <: Nat](cz : ComplexZipper[S[P], A]) : Complex[S[P], A] =
//         seal(tail(cz)) :>> head(cz).close

//     })(dim(cz), cz)

//   //============================================================================================
//   // FROM COMPLEX
//   //

//   def fromComplex[N <: Nat, A](cmplx : Complex[N, A]) : ComplexZipper[N, A] = 
//     (new NatCaseSplit {

//       type In[M <: Nat] = Complex[M, A]
//       type Out[M <: Nat] = ComplexZipper[M, A]

//       def caseZero(cmplx : Complex[_0, A]) : ComplexZipper[_0, A] =
//         EmptyZ :>> NestingZipper(head(cmplx), Bottom())

//       def caseSucc[P <: Nat](cmplx : Complex[S[P], A]) : ComplexZipper[S[P], A] =
//         fromComplex(tail(cmplx)) :>> NestingZipper(head(cmplx), Bottom())

//     })(dim(cmplx), cmplx)

//   //============================================================================================
//   // VISIT
//   //

//   def visitComplex[N <: Nat, A](dir : Direction[S[N]], cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] =
//     (new NatCaseSplitTwo {

//       type In0[M <: Nat] = Direction[S[M]]
//       type In1[M <: Nat] = ComplexZipper[M, A]
//       type Out[M <: Nat] = Option[ComplexZipper[M, A]]

//       def caseZero(dir : Direction[_1], cz : ComplexZipper[_0, A]) =
//         for {
//           zp <- NestingZipper.visit(dir, head(cz))
//         } yield EmptyZ :>> zp

//       def caseSucc[P <: Nat](dir : Direction[S[S[P]]], cz : ComplexZipper[S[P], A]) : Option[ComplexZipper[S[P], A]] =
//         dir match {
//           case Dir(Root()) =>
//             for {
//               zp <- NestingZipper.visit(dir, head(cz))
//             } yield tail(cz) :>> zp

//           case Dir(Step(d, ds)) =>
//             for {

//               prefixZipper <- visitComplex(ds, cz)
//               nestingSibling <- NestingZipper.sibling(d, head(prefixZipper))
//               prefixSpine <- focusSpine(prefixZipper)

//               res <- (
//                 prefixSpine match {
//                   case Leaf(_) => Some(tail(prefixZipper) :>> nestingSibling)
//                   case Node(_, shell) =>
//                     for {
//                       extents <- Tree.shellExtents(shell)
//                       recAddr <- Tree.valueAt(extents, d)
//                       fixupLower <- seekComplex(recAddr, tail(prefixZipper))
//                     } yield (fixupLower :>> nestingSibling)
//                 }
//               )

//             } yield res
//         }

//     })(dim(cz), dir, cz)

//   //============================================================================================
//   // SEEK
//   //

//   def seekComplex[N <: Nat, A](addr : Address[S[N]], cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] =
//     addr match {
//       case Root() => Some(cz)
//       case Step(d, ds) =>
//         for {
//           zp <- seekComplex(ds, cz)
//           zr <- visitComplex(d, zp)
//         } yield zr
//     }

//   //============================================================================================
//   // FOCUS ROUTINES
//   //


//   def updateFocus[N <: Nat, A](cz : ComplexZipper[N, A], nst : Nesting[N, A]) : ComplexZipper[N, A] =
//     (new NatCaseSplitTwo {

//       type In0[M <: Nat] = ComplexZipper[M, A]
//       type In1[M <: Nat] = Nesting[M, A]
//       type Out[M <: Nat] = ComplexZipper[M, A]

//       def caseZero(cz : ComplexZipper[_0, A], nst : Nesting[_0, A]) : ComplexZipper[_0, A] =
//         EmptyZ :>> head(cz).withFocus(nst)

//       def caseSucc[P <: Nat](cz : ComplexZipper[S[P], A], nst : Nesting[S[P], A]) : ComplexZipper[S[P], A] =
//         tail(cz) :>> head(cz).withFocus(nst)

//     })(dim(cz), cz, nst)

//   def focusValue[N <: Nat, A](cz : ComplexZipper[N, A]) : A = 
//     baseValue(head(cz).focus)

//   def focusDeriv[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[Derivative[S[N], A]] = 
//     (new NatCaseSplit {

//       type In[M <: Nat] = ComplexZipper[M, A]
//       type Out[M <: Nat] = Option[Derivative[S[M], A]]

//       def caseZero(cz : ComplexZipper[_0, A]) : Option[Derivative[_1, A]] =
//         cz match {
//           case _ :>> NestingZipper(Obj(a), _) => Some(Open(Pt(Leaf(S(Z))), Empty()))
//           case _ :>> NestingZipper(Box(a, canopy), _) => Some(Open(const(canopy)(Leaf(S(canopy.dim))), Empty()))
//         }

//       def caseSucc[P <: Nat](cz : ComplexZipper[S[P], A]) : Option[Derivative[S[S[P]], A]] =
//         cz match {
//           case _ :>> NestingZipper(Dot(a, _), _) => None
//           case _ :>> NestingZipper(Box(a, canopy), _) => Some(Open(const(canopy)(Leaf(S(canopy.dim))), Empty()))
//         }

//     })(dim(cz), cz)


//   def focusSpine[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[Tree[N, A]] = 
//     (new NatCaseSplit {

//       type In[M <: Nat] = ComplexZipper[M, A]
//       type Out[M <: Nat] = Option[Tree[M, A]]

//       def caseZero(cz : ComplexZipper[_0, A]) : Option[Tree[_0, A]] =
//         cz match {
//           case _ :>> NestingZipper(Obj(a), _) => Some(Pt(a))
//           case _ :>> NestingZipper(Box(a, canopy), _) => spineFromCanopy(canopy)
//         }

//       def caseSucc[P <: Nat](cz : ComplexZipper[S[P], A]) : Option[Tree[S[P], A]] =
//         cz match {
//           case _ :>> NestingZipper(Dot(a, _), _) =>
//             for {
//               deriv <- focusDeriv(tail(cz))
//             } yield Derivative.plug(deriv, a)
//           case _ :>> NestingZipper(Box(a, canopy), _) => spineFromCanopy(canopy)
//         }


//     })(dim(cz), cz)

//   def focusCanopy[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[Tree[N, Address[N]]] =
//     (new NatCaseSplit {

//       type In[M <: Nat] = ComplexZipper[M, A]
//       type Out[M <: Nat] = Option[Tree[M, Address[M]]]

//       def caseZero(cz : ComplexZipper[_0, A]) : Option[Tree[_0, Address[_0]]] =
//         Some(Pt(Root()))

//       def caseSucc[P <: Nat](cz : ComplexZipper[S[P], A]) : Option[Tree[S[P], Address[S[P]]]] =
//         cz match {
//           case (_ :>> NestingZipper(Dot(_, _), _)) => None
//           case (_ :>> NestingZipper(Box(_, int), _)) => Some(addressTree(int))
//         }

//     })(dim(cz), cz)

//   // The idea here is to embed the current focus of this nesting zipping in exactly the
//   // unit tree which surrounds it.  Notice that I think this calculation could be eliminated
//   // if you were more careful in the compression routine to pass along the required derivative
//   // as the tree was compressed.  Have to look into that ...

//   def focusUnit[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[Tree[N, Nesting[N, A]]] =
//     (new NatCaseSplit {

//       type In[M <: Nat] = ComplexZipper[M, A]
//       type Out[M <: Nat] = Option[Tree[M, Nesting[M, A]]]

//       def caseZero(cz : ComplexZipper[_0, A]) : Option[Tree[_0, Nesting[_0, A]]] =
//         Some(Pt(head(cz).focus))

//       def caseSucc[P <: Nat](cz : ComplexZipper[S[P], A]) : Option[Tree[S[P], Nesting[S[P], A]]] = {

//         val fcs = head(cz).focus

//         for {
//           spine <- focusSpine(cz)
//           res <- (
//             spine match {
//               case Leaf(d) =>
//                 for {
//                   unit <- focusUnit(tail(cz))
//                 } yield Node(fcs, const(unit)(Leaf(d)))
//               case Node(a, shell) =>
//                 for {
//                   extents <- shellExtents(shell)
//                 } yield Node(fcs, const(extents)(Leaf(S(shell.dim))))
//             }
//           )
//         } yield res

//       }

//     })(dim(cz), cz)

//   //============================================================================================
//   // RESTRICT FOCUS
//   //

//   def restrictFocus[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] = 
//     (new NatCaseSplit {

//       type In[M <: Nat] = ComplexZipper[M, A]
//       type Out[M <: Nat] = Option[ComplexZipper[M, A]]

//       def caseZero(cz : ComplexZipper[_0, A]) : Option[ComplexZipper[_0, A]] =
//         Some(EmptyZ :>> head(cz).withContext(Bottom()))

//       def caseSucc[P <: Nat](cz : ComplexZipper[S[P], A]) : Option[ComplexZipper[S[P], A]] = {

//         type SrcM[R] = SourceM[P, A, R]
//         type SrcS[S, R] = StateT[Option, S, R]

//         val MS = MonadState[SrcS, Complex[P, A]]
//         import MS._

//         for {
//           fnst <- focusSpine(cz)
//           newTail <- restrictFocus(tail(cz))
//           newComplex <- exciseLocal(Root()(fnst.dim), fnst).exec(seal(newTail))
//         } yield (fromComplex(newComplex) :>> NestingZipper(head(cz).focus, Bottom()))

//       }

//     })(dim(cz), cz)

//   //============================================================================================
//   // CONTRACT FOCUS
//   //

//   def contractFocus[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] = 
//     (new NatCaseSplit {

//       type In[M <: Nat] = ComplexZipper[M, A]
//       type Out[M <: Nat] = Option[ComplexZipper[M, A]]

//       def caseZero(cz : ComplexZipper[_0, A]) : Option[ComplexZipper[_0, A]] =
//         cz match {
//           case _ :>> NestingZipper(fcs, cn) =>
//             Some(EmptyZ :>> NestingZipper(Obj(baseValue(fcs)), cn))
//         }

//       def caseSucc[P <: Nat](cz : ComplexZipper[S[P], A]) : Option[ComplexZipper[S[P], A]] =
//         for {
//           spine <- focusSpine(cz)
//           newTail <- compressFocus(tail(cz), spine)
//           nstZp = head(cz)
//         } yield newTail :>> NestingZipper(Dot(baseValue(nstZp.focus), dim(cz)), nstZp.context)

//     })(dim(cz), cz)


//   //============================================================================================
//   // COMPRESS FOCUS
//   //

//   def compressFocus[N <: Nat, A](cz : ComplexZipper[N, A], tr : Tree[S[N], A]) : Option[ComplexZipper[N, A]] = 
//     for {
//       nst <- compressLocal(cz, tr)
//     } yield updateFocus(cz, Box(focusValue(cz), nst))

//   def compressLocal[N <: Nat, A](cz : ComplexZipper[N, A], tr : Tree[S[N], A]) : Option[Tree[N, Nesting[N, A]]] = 
//     tr match {
//       case Leaf(_) => focusUnit(cz)
//       case Node(_, sh) => {

//         import scalaz.std.option._

//         for {
//           cp <- focusCanopy(cz)
//           zsh <- Tree.zipComplete(cp, sh)
//           toJoin <- traverse(zsh)({ case (d, t) => 
//             for {
//               cz0 <- visitComplex(d, cz)
//               lr <- compressLocal(cz0, t)
//             } yield lr
//           })
//           result <- Tree.join(toJoin)
//         } yield result
//       }

//     }

}
