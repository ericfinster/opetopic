/**
  * BaezDolan.scala - Some first attempts at the Baez-Dolan definition
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.bd

import opetopic._

//============================================================================================
// TERMS
//

sealed trait Term[N <: Nat]
case class Lam[K <: Nat, N <: Nat](val name: String, val ty: Type[K], val u: Term[N]) extends Term[N]
case class App[K <: Nat, N <: Nat](val u: Term[N], val v: Term[K]) extends Term[N]
case class Var[N <: Nat](val idx : Int) extends Term[N]

sealed trait Cell[N <: Nat] extends Term[N]
case class CompCell[N <: Nat](val pd: Tree[Term[N], N]) extends Cell[N]
case class FillCell[N <: Nat](val pd: Tree[Term[N], N]) extends Cell[S[N]]

sealed trait Proof[N <: Nat] extends Term[N]
case class IsFiller[N <: Nat](val term: Term[N]) extends Proof[N]
case class UnivComp[N <: Nat](val tree: Tree[Proof[N], N]) extends Proof[N]
case class UnivToBal[N <: Nat](val tgtCell: Term[N], val univCell: Term[S[N]], val ev: Term[S[N]]) extends Proof[S[N]]
case class BalToUniv[N <: Nat](val tgtCell: Term[N], val hole: Derivative[Term[N], N], val ev: Term[N]) extends Proof[S[N]]

case class UnivCon[N <: Nat](
  val cell: Term[N], 
  val proof: Term[S[N]]     // (x : TgtShellOf(cell)) -> IsBalanced (TargetExt(x, cell))
) extends Proof[N]

case class BalCon[N <: Nat](
  val pd: Derivative[Term[N], N],
  val lift: Term[N],        // (x : TargetShellOf(pd)) -> HoleShape(pd)
  val fill: Term[S[N]],     // (x : TargetShellOf(pd)) -> Shell(plug(pd, lift(x)), x)
  val isUniv: Term[S[N]],   // (x : TargetShellOf(pd)) -> IsUniversal(fill(x))
  val isBal: Term[S[N]]     // (x : TargetShellOf(pd)) -> (u : Shell(plug(pd, lift(x)), x)) -> (univ : IsUniversal u) -> (y : HoleShape(pd)) -> IsBalanced(SrcExt(y, u))
) extends Proof[N]

//============================================================================================
// TYPES
//

sealed trait Type[N <: Nat]
case class Pi[K <: Nat, N <: Nat](val nm: Int, val dom: Type[K], val cod: Type[N]) extends Type[N]

sealed trait Shell[N <: Nat] extends Type[N]
case object EmptyShell extends Shell[_0]
case class CompleteShell[N <: Nat](val srcTree: Tree[Term[N], N], val target : Term[N]) extends Shell[N]

sealed trait Property[N <: Nat] extends Type[N]
case class IsBananced[N <: Nat](val hole: Derivative[Term[N], N]) extends Property[N]
case class IsUniversal[N <: Nat](val expr: Term[N]) extends Property[N]
