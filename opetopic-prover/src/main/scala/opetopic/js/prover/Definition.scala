/**
  * Definition.scala - Encapsulate a definition
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import opetopic._
import ott.OttSyntax._

abstract class Definition {

  def id: String
  def expr: ExpT
  def typeExpr: ExpT
  def context: List[(String, ExpT)]
  def environment: List[Definition]

  def declaration: DeclT = {

    val tele : List[TeleT] = context.map({
      case (pid, ty) => Tele(PVar(pid), ty) 
    }).reverse

    val whereDecls : List[DeclT] =
      environment.map(_.simpleDeclaration).reverse

    val whereExp =
      if (whereDecls.length > 0)
        Where(expr, whereDecls)
      else NoWhere(expr)

    Def(id, tele, typeExpr, NoWhere(expr))

  }

  def simpleDeclaration: DeclT = {
    Def(id, Nil, typeExpr, NoWhere(expr))
  }

}

//============================================================================================
// CELL DEFINITIONS
//

case class Cell(
  val id: String,
  val expr: ExpT,
  val typeExpr: ExpT,
  val context: List[(String, ExpT)],
  val environment: List[Definition]
) extends Definition


//============================================================================================
// PROPERTY DEFINITIONS
//

sealed trait Property extends Definition {
  def cellId: String
  def cellExp: ExpT
  def isTarget: Boolean
  def isSourceAt(a: SAddr): Boolean
}

case class TgtExtProperty(
  val id: String,
  val expr: ExpT,
  val typeExpr: ExpT,
  val cellId: String,
  val cellExp: ExpT,
  val context: List[(String, ExpT)],
  val environment: List[Definition]
) extends Property {
  def isTarget: Boolean = true
  def isSourceAt(a: SAddr): Boolean = false
}

case class SrcExtProperty(
  val id: String,
  val expr: ExpT,
  val typeExpr: ExpT,
  val addr: SAddr,
  val cellId: String,
  val cellExp: ExpT,
  val context: List[(String, ExpT)],
  val environment: List[Definition]
) extends Property {
  def isTarget: Boolean = false
  def isSourceAt(a: SAddr): Boolean = a == addr
}
