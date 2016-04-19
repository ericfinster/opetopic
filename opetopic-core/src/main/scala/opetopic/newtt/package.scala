/**
  * package.scala - Globals for Scala MiniTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import opetopic._

package object newtt {

  type Ident = String
  type Name = String
  type SClos = (List[(Name, Expr)], Rho)
  type Nf = Val
  type TVal = Val
  type ConstExpr[N <: Nat] = Expr
  type ConstVal[N <: Nat] = Val
  type ValTree[N <: Nat] = Tree[Val, N]
  type ValComplex[N <: Nat] = Complex[ConstVal, N]
  type ExprComplex[N <: Nat] = Complex[ConstExpr, N]
  type ExprNesting[N <: Nat] = Nesting[Expr, N]

}
