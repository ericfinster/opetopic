/**
  * Opetopic.scala - Main site entry and setup
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs.js.JSApp

import org.scalajs.jquery._
import org.scalajs.dom
import scalatags.JsDom.all._
import JQuerySemanticUI._
import JsDomFramework._

import opetopic._
import opetopic.ui._
import opetopic.stable._
import syntax.nesting._
import syntax.complex._
import opetopic.Examples._

object Opetopic extends JSApp {

  type OptString[N <: Nat] = Option[String]

  val fredStrComplex: Complex[OptString, _4] =
    fredComplex.map(new IndexedMap[ConstInt, OptString] {
      def apply[N <: Nat](n: N)(i: Int) : Option[String] = Some(i.toString)
    })

  val mainViewer = new JsStableViewer

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main: Unit = {

    println("Launched Opetopic Javscript ...")

    jQuery("#main-viewer-div").append(mainViewer.uiElement)

    mainViewer.loadComplex(fredStrComplex)

  }


}


