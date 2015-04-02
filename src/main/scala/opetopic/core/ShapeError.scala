/**
  * ShapeError.scala - Base class for shape errors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

trait ShapeError

class ShapeMatchError extends ShapeError
class ShapeLookupError extends ShapeError
