/**
  * package.scala - Package object for OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

package object tt {

  type Ident = String
  type Name = String
  type Pos = ((Int, Int), String)
  type SClos = (List[(Name, Expr)], Rho)
  type Nf = Val
  type TVal = Val

  type CstExpr[N <: Nat] = Expr
  type CstVal[N <: Nat] = Val
  type ExprNst[N <: Nat] = Nesting[Expr, N]
  type NstList = List[Sigma[ExprNst]]

}
