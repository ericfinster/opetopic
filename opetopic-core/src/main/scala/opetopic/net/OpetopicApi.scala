/**
  * OpetopicApi.scala - Remote API
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.net

trait OpetopicApi {

  def createUser(id: String, passwd: String) : String
  def listUsers() : Seq[String]

}
