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

  // Okay, we have static galleries.  Now it's time to start implementing the interactivity
  // portion.  The idea is that we are going to subclass the panel and the gallery in such
  // a way that they keep track of and modify the created elements.

  // Let's start looking at the panel first ...



}
