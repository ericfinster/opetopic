/**
  * InteractiveFramework.scala - A Rendering Framework which provides element permanence
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

abstract class InteractiveFramework[U](implicit isNumeric: Numeric[U], isOrdered: Ordering[U]) extends RenderingFramework[U] {

  type ElementType <: Element

  type PathType <: ElementType with Path
  type TextType <: ElementType with Text
  type GroupType <: ElementType with Group
  type RectangleType <: ElementType with Rectangle

  abstract class Element 

  trait Rectangle 
  trait Group 
  trait Path 
  trait Text 

  // Now we need some subclasses of panel, etc which now retain links to their
  // rendered elements.

  // In order to do this, we should pull out the rendering methods on the panel
  // class and leave them abstract.  We should then re-implement them in a 
  // "static" panel class which uses the old rendering technique.  A subclass here
  // will then implement them differently ...

  // Also, the object and nesting distinctions should become *traits* which are 
  // then mixed in dependeing on the dimension by a constructor...

  // All this is now done.  Next you can either implement a gallery or move on 
  // to the interactive/stateful panels.

}
