/**
  * ClientApi.scala - Common Clientside API
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.concurrent.Future
import scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.scalajs.dom

import upickle.default._
import upickle.Js
import autowire._

import opetopic.net._

object ClientApi extends autowire.Client[Js.Value, Reader, Writer] {

  override def doCall(req: Request) : Future[Js.Value] = 
    dom.ext.Ajax.post(
      url = "/api/" + req.path.mkString("/"),
      data = upickle.json.write(Js.Obj(req.args.toSeq:_*))
    ).map(_.responseText)
     .map(upickle.json.read)

  def read[Result: Reader](p: Js.Value) = readJs[Result](p)
  def write[Result: Writer](r: Result) = writeJs(r)

}


// // client-side implementation, and call-site
// object MyClient extends autowire.Client[String, upickle.Reader, upickle.Writer]{
//   def write[Result: Writer](r: Result) = upickle.write(r)
//   def read[Result: Reader](p: String) = upickle.read[Result](p)

//   override def doCall(req: Request) = {
//     println(req)
//     MyServer.routes.apply(req)
//   }
// }

// MyClient[MyApi].doThing(3, "lol").call().foreach(println)
// // List(lol, lol, lol)
