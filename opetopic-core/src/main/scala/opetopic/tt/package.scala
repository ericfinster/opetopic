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

  type NstExpr[N <: Nat] = Nesting[Expr, N]
  type NstVal[N <: Nat] = Nesting[Val, N]
  type TrExpr[N <: Nat] = Tree[Expr, N]
  type TrVal[N <: Nat] = Tree[Val, N]

  type NstList = List[Sigma[NstExpr]]
  type NchExpr[N <: Nat] = (Suite[NstExpr, N], TrExpr[N])
}
