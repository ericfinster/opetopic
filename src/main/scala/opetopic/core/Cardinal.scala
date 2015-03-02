/**
  * Cardinal.scala - Opetopic Cardinal
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import scalaz.Leibniz._

import Nats._

object Cardinals {

  sealed trait TreeSeq[N <: Nat[N], K <: Nat[K], T[+_]] {

    def split[M <: Nat[M], D <: Nat[D], L <: Lte[M, K, D, L], A](ta : T[A])(implicit lte : L) 
        : Unit = ()

  }

  case class ZeroSeq[N <: Nat[N]]() extends TreeSeq[N, _0, ({ type L[+X] = Tree[N, X] })#L] {

  }

  case class SuccSeq[N <: Nat[N], K <: Nat[K], T[+_]](implicit isSeq : TreeSeq[N, K, T]) extends TreeSeq[N, S[K], ({ type L[+X] = T[Tree[S[K], X]] })#L] {

  }


  object TreeSeq {

    // implicit def treeIsSeq[N <: Nat[N], A] : Aux[N, Z, ({ type L[+X] = Tree[N, X] })#L] = 
    //   new TreeSeq[N, Z] { 

    //     type Out = Tree[N, A]

    //     type SeqType = A
    //     type SeqCons[+X] = Tree[N, X] 

    //     def leibniz : Out === SeqCons[SeqType] = refl

    //   }

    // implicit def succSeq[N <: Nat[N], K <: Nat[K]](implicit isSeq : TreeSeq[N, K]) 
    //     : Aux[N, S[K], ({ type L[+X] = isSeq.Out[Tree[S[K], X]] })#L] = 
    //   new TreeSeq[N, S[K]] { type Out[+A] = isSeq.Out[Tree[S[K], A]] }

  }

  import TreeSeq._

  // type CTree0[+A] = Tree[Z, A]
  // type CTree1[+A] = Tree[Z, Tree[S[Z], A]]
  // type CTree2[+A] = Tree[Z, Tree[S[Z], Tree[S[S[Z]], A]]]
  // type CTree3[+A] = Tree[Z, Tree[S[Z], Tree[S[S[Z]], Tree[S[S[S[Z]]], A]]]]

  // implicitly[Aux[Z, _0, CTree0]]
  // implicitly[Aux[Z, _1, CTree1]]
  // implicitly[Aux[Z, _2, CTree2]]
  // implicitly[Aux[Z, _3, CTree3]]

}
