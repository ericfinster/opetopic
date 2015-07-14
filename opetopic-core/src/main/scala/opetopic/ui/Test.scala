/**
  * Test.scala - A main class for testing your setup
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import java.io._

import opetopic.syntax.nesting._

object Test {

  val output: String = "/home/ericfinster/Documents/svgs/fred3.svg"

  def main(args: Array[String]) : Unit = {

    println("Opetopic Testing Object")

    // import opetopic.Examples._
    // import ScalatagsTextFramework._

    // val panel = Panel(fred3, fred2)

    // val panelSvg = {
    //   import bundle.implicits._
    //   import bundle.svgTags._
    //   import bundle.svgAttrs._

    //   import opetopic.syntax.nesting._

    //   val baseBox = panel.boxNesting.baseValue

    //   val viewboxStr = 
    //     (baseBox.x - (panel.externalPadding * 4)).toString ++ " " ++ (baseBox.y - (panel.externalPadding * 4)).toString ++ " " ++
    //       (baseBox.width + (panel.externalPadding * 8)).toString ++ " " ++ (baseBox.height + (panel.externalPadding * 8)).toString

    //   svg(viewBox:=viewboxStr,xmlns:="http://www.w3.org/2000/svg")(panel.render)
    // }

    // val writer = new PrintWriter(new File(output))
    // writer.write(panelSvg.toString)
    // writer.close

  }

}
