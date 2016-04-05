/**
  * Sketch.scala - A model for an Opetopic Sketch
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models

import java.util.UUID

case class Sketch(
  val sketchId: UUID,
  val authorId: UUID,
  val name: String,
  val description: String,
  val path: String,
  val data: String
)


