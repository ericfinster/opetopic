/**
  * OpetopicApi.scala - Remote API
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.net

case class SaveSketchRequest(
  val name: String,
  val path: String,
  val description: String,
  val data: String
)

case class LoadSketchRequest(
  val id: String
)

case class SaveModuleRequest(
  val moduleId: Option[String],
  val name: String,
  val description: String,
  val data: String
)

case class LoadModuleRequest(
  val name: String,
  val uuid: String
)