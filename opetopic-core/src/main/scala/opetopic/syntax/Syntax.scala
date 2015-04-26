/**
  * Syntax.scala - Syntax Trait
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.syntax

trait Syntax {

  object tree extends ToTreeOps
  object complex extends ToComplexOps
  object nesting extends ToNestingOps
  object cardinal extends ToCardinalOps

}
