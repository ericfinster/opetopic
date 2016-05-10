/**
  * VisualCell.scala - Cells containing visual, layout information
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import opetopic.ui._

trait VisualCell[A, F <: UIFramework, V <: VisualCell[A, F, V]] {

  val framework: F
  import framework._
  import isNumeric._



}
