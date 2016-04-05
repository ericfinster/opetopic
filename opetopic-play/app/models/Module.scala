/**
  * Module.scala - Model of a Prover Module
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models

import java.util.UUID

case class Module(
  val moduleId: UUID,
  val authorId: UUID,
  val name: String,
  val description: String,
  val data: String
)

