/**
  * Zipper.scala - Higher dimensional zippers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import Nats._

object Zippers {

  //============================================================================================
  // ADDRESSES
  //

  trait Address[N <: Nat[N]] {
    type Out
  }

  object Address {

    type Aux[N <: Nat[N], A] = Address[N] { type Out = A }

    implicit def zeroAddress : Aux[_0, Unit] = 
      new Address[_0] {
        type Out = Unit
      }

    implicit def succAddress[P <: Nat[P], A](implicit addr : Aux[P, A]) : Aux[S[P], List[addr.Out]] = 
      new Address[S[P]] {
        type Out = List[addr.Out]
      }

  }

  //============================================================================================
  // DERIVATIVES
  //

  trait DerivativeClass[N <: Nat[N]] {
    type Out[+_]
    def plug[A](deriv : Out[A], a : A) : Tree[N, A]
    def caseSplit[A](deriv : Out[A], split : DerivativeDimCase) : split.Out[N]
  }

  object DerivativeClass {

    type Aux[N <: Nat[N], T[+_]] = DerivativeClass[N] { type Out[+A] = T[A] }

    implicit def zeroDerivative : Aux[_0, ({ type L[+A] = Unit })#L] = 
      new DerivativeClass[_0] {
        type Out[+A] = Unit
        def plug[A](deriv : Unit, a : A) : Tree[_0, A] = Pt(a)
        def caseSplit[A](deriv : Out[A], split : DerivativeDimCase) : split.Out[_0] = split.caseZero
      }

    implicit def succDerivative[P <: Nat[P], C[+_]](implicit cntxt : ContextClass.Aux[S[P], C]) 
        : Aux[S[P], ({ type L[+A] = (Tree[P, Tree[S[P], A]], C[A]) })#L] =
      new DerivativeClass[S[P]] {
        type Out[+A] = (Tree[P, Tree[S[P], A]], C[A])

        def plug[A](deriv : Out[A], a : A) : Tree[S[P], A] = 
          cntxt.close(deriv._2, Node(a, deriv._1))

        def caseSplit[A](deriv : Out[A], split : DerivativeDimCase) : split.Out[S[P]] = {
          split.caseSucc(deriv._1, deriv._2)
        }

      }

  }

  // What if you just write out the recursors for each of these type, and
  // then implement just that as part of the type class?  Then, presumably,
  // all other functions should be derivable from that idea.

  trait DerivativeDimCase {

    type Out[N <: Nat[N]]

    def caseZero : Out[_0]
    def caseSucc[P <: Nat[P], C[+_], A](sh : Tree[P, Tree[S[P], A]], cntxt : C[A])(implicit cls : ContextClass.Aux[S[P], C]) : Out[S[P]]


  }

  // abstract class Derivative[N <: Nat[N], D[+_], A](d : D[A])(implicit cls : DerivativeClass.Aux[N, D]) {

  //   def plugWith(a : A) : Tree[N, A] = 
  //     (new DerivativeDimCase {

  //       type Out[M <: Nat[M]] = A => Tree[M, A]

  //       // def caseZero : Out[_0] = Pt(_)
  //       // def caseSucc[P <: Nat[P], C[+_], A](sh : Tree[P, Tree[S[P], A]], cntxt : C[A])(implicit cls : ContextClass.Aux[S[P], C]) : Out[S[P]] = ???

  //     })

  // }

  //============================================================================================
  // CONTEXTS
  //

  trait ContextClass[N <: Nat[N]] {
    type Out[+_]
    def close[A](cntxt : Out[A], tr : Tree[N, A]) : Tree[N, A]
  }

  object ContextClass {

    type Aux[N <: Nat[N], T[+_]] = ContextClass[N] { type Out[+A] = T[A] }

    implicit def zeroContext : Aux[_0, ({ type L[+A] = Unit })#L] = 
      new ContextClass[_0] {
        type Out[+A] = Unit
        def close[A](cntxt : Out[A], tr : Tree[_0, A]) : Tree[_0, A] = tr
      }

    implicit def succContext[P <: Nat[P], D[+_]](implicit deriv : DerivativeClass.Aux[P, D])
        : Aux[S[P], ({ type L[+A] = List[(A, D[Tree[S[P], A]])] })#L] = 
      new ContextClass[S[P]] {
        type Out[+A] = List[(A, D[Tree[S[P], A]])]
        def close[A](cntxt : Out[A], tr : Tree[S[P], A]) : Tree[S[P], A] = 
          cntxt match {
            case Nil => tr
            case ((a, d) :: cs) => close(cs, Node(a, deriv.plug(d, tr)))
          }
      }

  }

  //============================================================================================
  // ZIPPERS
  //

  trait ZipperClass[N <: Nat[N]] {
    type Out[+_]

  }

  object ZipperClass {

    type Aux[N <: Nat[N], T[+_]] = ZipperClass[N] { type Out[+A] = T[A] }

    implicit def zipperPair[N <: Nat[N], C[+_]](implicit cntxt : ContextClass.Aux[N, C]) 
        : Aux[N, ({ type L[+A] = (Tree[N, A], C[A]) })#L] = 
      new ZipperClass[N] {
        type Out[+A] = (Tree[N, A], C[A])
      }

  }

}
