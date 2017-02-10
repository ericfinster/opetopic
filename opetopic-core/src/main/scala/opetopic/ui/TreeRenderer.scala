/**
  * TreeRenderer.scala - Render planar strees from addresses
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._

class TreeRenderer[F <: UIFramework](final val framework: F) {

  import framework._
  import isNumeric._

  implicit def toSize(i: Int): Size =
    fromInt(i)

  def double(s: Size) : Size = fromInt(2) * s

  val nodeRadius : Size = 150
  val nodeStroke : Size = 100
  val childDepth : Size = 1000
  val padding : Size = 100
  val offset : Size = nodeRadius + padding

  case class Subtree(
    val be: BoundedElement,
    val rootX: Size,
    val rootY: Size
  )

  def mkNd(x: Size, y: Size): RectangleType = 
    rect(x - nodeRadius, y - nodeRadius,
      double(nodeRadius),
      double(nodeRadius),
      nodeRadius, "black",
      nodeStroke, "black"
    )

  def renderAddr(addr: SAddr): Subtree =
    if (addr == Nil) {

      val node = mkNd(offset, -offset)
      val bnds = Bounds(0, -double(offset), double(offset), double(offset))

      Subtree(BoundedElement(node, bnds), offset, -offset)

    } else {


      val sts = addr.map(renderDir(_))

      val totalWidth : Size =
        (sts foldLeft fromInt(0))({
          case (curW, st) => curW + st.be.bounds.width
        })

      val cenX = half(totalWidth)
      val cenY = -offset

      val node = mkNd(cenX, cenY)
      val pathBase: String = "M " + cenX.toString + " " + cenY.toString + " " 

      val (maxHeight, _, els) : (Size, Size, List[Element]) =
        (sts foldLeft (fromInt(0), fromInt(0), List[Element]()))({
          case ((curH, curW, prvs), st) => {

            val trEl = translate(st.be.element, curW, -childDepth)
            
            val pStr = pathBase + (curW + st.rootX).toString + " " + (st.rootY - childDepth).toString
            val p = path(pStr, "black", 50, "none")

            (max(curH, st.be.bounds.height), curW + st.be.bounds.width, p :: trEl :: prvs)

          }
        })

      val grp = group((node :: els) : _*)
      val bnds = Bounds(0, -(childDepth + maxHeight), totalWidth, childDepth + maxHeight)

      Subtree(BoundedElement(grp, bnds), cenX, cenY)

    }

  def renderDir(dir: SDir): Subtree =
    renderAddr(dir.dir)


}
