/**
  * ShapeError.scala - Base class for shape errors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

trait ShapeError

class ShapeMatchError extends ShapeError
class ShapeLookupError extends ShapeError
class ShapeRootEmptyError extends ShapeError
class ShapeSelectionError extends ShapeError
class ShapeLteError extends ShapeError
