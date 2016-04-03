/**
  * Sketch.scala - A model for an Opetopic Sketch
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models.sketchpad

case class Sketch(
  val name: String,
  val description: String,
  val path: String,
  val data: String
)


