/**
  * Pane.scala - Base Trait for Panes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

trait Pane {

  var hotkeysEnabled = true
  def env : Environment
  def focus: Unit

}
