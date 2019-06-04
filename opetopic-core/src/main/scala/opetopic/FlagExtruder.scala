/**
  * FlagExtruder.scala - Extrude a complex from the list of its flags
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._
import scala.collection.mutable.Map
import scala.collection.mutable.Set

object FlagExtruder {

  val debug: Boolean = false

  def log(str: String): Unit =
    if (debug) println(str) else ()

  // Assumes the iterator is iterating in the "extrusion order", which
  // is with the reversed flag set to true.
  def extrudeFrom[A](itr: FlagIterator[A], dummy: A): SCardinal[A] = {

    var cardinal: SCardinal[A] =
      SCardinal(dummy)

    // Make sure we have at least one flag
    if (itr.hasNext) {

      val initFlag = itr.next
      val dim = initFlag.length

      log("Initial flag: " ++ initFlag.map(_.toString).mkString(" "))

      // The predecessor map (think of a different name ....
      val predMap: Map[A, Set[A]] = Map[A, Set[A]]()

      // Setup our nesting stacks
      val stacks: Array[List[A]] = new Array(dim)
      stacks(0) = Nil // Dimension 0 is special
      for { i <- Range(1, dim) } { stacks(i) = List(initFlag(i).face) }

      // Initialize with the correct cardinal
      cardinal = SCardinal(initFlag.head.face)

      // We're going to keep a reference to the last flag
      // seen for purposes of comparison.
      var last: Flag[A] = initFlag

      // These are the cells which have been defined
      val cells: Set[A] = Set(initFlag.head.face)


      // Tools for manipulating the stack array
      def push(i: Int, a: A): Unit = {

        // As guys are pushed on the stack, mark
        // them as dependents
        stacks(i).headOption match {
          case None => ()
          case Some(hd) => {
            if (predMap.isDefinedAt(hd))
              predMap(hd) += a
            else predMap += ((hd, Set[A](a)))
          }
        }
        
        stacks(i) = a :: stacks(i)

      }


      def pop(i: Int): Unit = {
        stacks(i) = stacks(i).tail
      }


      for { flg <- itr } {

        log("Passing flag: " ++ flg.map(_.toString).mkString(" "))

        val (p, s) = (last, flg).zipped.span({
          case (f, g) => f == g
        })

        val (prefix, suffix) = (p.unzip._2.toList, s.unzip._2.toList)

        val dim = prefix.length

        s match {
          case (TgtFacet(lstTgt), TgtFacet(tgt)) :: (SrcFacet(lstDot, _), TgtFacet(dot)) :: _ => {
             log("  Pop(1): " ++ lstTgt.toString)


            // This is the case of a possible extrusion.

            if (dim > 0) {

              pop(dim)

              val canExtrude = prefix.forall(f =>
                cells.contains(f.face))

              if (canExtrude && predMap.isDefinedAt(tgt)) {

                val srcSet = predMap(tgt)
                val rAddr = rootCardinalAddr(dim)
                
                log("Extrusion about " ++ srcSet.toString ++ " with (" ++ tgt.toString ++ ", " ++ dot.toString ++ ")")
                // log("Extrusion address is: " ++ rAddr.toString)

                val extrudeCardinal =
                  if (cardinal.dim < dim + 1)
                    cardinal.extend(dummy)
                  else
                    cardinal

                extrudeCardinal.extrude(rAddr, tgt, dot)(a => srcSet.contains(a)) match {
                  case None => log("Extrusion failed")
                  case Some(c) => {
                    log("Extrusion successful")
                    // log("Excised tree: " ++ c._2.toString)
                    cardinal = c._1
                  }
                }

                cells += (tgt, dot)

              }

            } else {

              log("Object extrusion (" ++ tgt.toString ++ ", " ++ dot.toString ++ ")")

              cardinal.extrudeObject(tgt, dot) match {
                case None => log("Object extrusion failed")
                case Some(c) => {
                  log("Object extrusion successful")
                  cardinal = c
                }
              }

              cells += (tgt, dot)

            }


          }
          case (SrcFacet(lstTgt, _), TgtFacet(tgt)) :: (TgtFacet(lstDot), TgtFacet(dot)) :: _ => {

            log("  Loop(2): " ++ tgt.toString)  // Extrude a loop!
            log("Extruding loop with (" ++ tgt.toString ++ ", " ++ dot.toString ++ ")")

            // Okay, let's finish the loops and we should be good.

            // I'm not completely convinced this is correct.
            // Shouldn't you need a race case?
            val extrudeCardinal =
              if (cardinal.dim < dim)
                cardinal.extend(dummy).extend(dummy)
              else if (cardinal.dim < dim + 1)
                cardinal.extend(dummy)
              else
                cardinal

            // We extrude a drop using the previous dimension
            val rAddr = rootCardinalAddr(dim - 1)

            extrudeCardinal.extrudeLoop(rAddr, tgt, dot) match {
              case None => log("Loop extrusion failed.")
              case Some(c) => {
                log("Loop extrusion successful")
                cardinal = c
              }
            }

            cells += (tgt, dot)

          }
          case (TgtFacet(lstTgt), TgtFacet(tgt)) :: (TgtFacet(lstDot), SrcFacet(dot, _)) :: _ => {
            log("  Push(3): " ++ tgt.toString)
            push(dim, tgt)
          }
          case (TgtFacet(lstTgt), SrcFacet(tgt, _)) :: (TgtFacet(lstDot), TgtFacet(dot)) :: _ => {
            log("  Drop!(4)  ")
          }
          case (TgtFacet(lstTgt), SrcFacet(tgt, _)) :: (SrcFacet(lstDot, _), SrcFacet(dot, _)) :: _ => {
            log("  Pop(5): " ++ lstTgt.toString ++ " / Push: " ++ tgt.toString)

            pop(dim)
            push(dim, tgt)
          }
          case (SrcFacet(lstTgt, _), TgtFacet(tgt)) :: (SrcFacet(lstDot, _), SrcFacet(dot, _)) :: _ => {
            log("  Pop(6): " ++ lstTgt.toString ++ " / Push: " ++ tgt.toString)

            // This appears to be a left-over from vertex neutralization
            // I'm not sure if this is the best way to handle it, or if
            // there is something slightly broken with extrusion detection.
            if (dim == 1 && lstTgt == tgt) {
              log("Duplicate object detected ...")

              val dupObj = prefix.last.face

              cardinal.extrudeObject(dupObj, tgt) match {
                case None => log("Object extrusion failed")
                case Some(c) => {
                  log("Object extrusion successful")
                  cardinal = c
                }
              }

              cells += (dupObj, tgt)
              
            }

            pop(dim)
            push(dim, tgt)
          }
          case (SrcFacet(lstTgt, _), SrcFacet(tgt, _)) :: (TgtFacet(lstDot), SrcFacet(dot, _)) :: _ => {
            log("  Push(7): " ++ tgt.toString)
            push(dim, tgt)
          }
          case (SrcFacet(lstTgt, _), SrcFacet(tgt, _)) :: (SrcFacet(lstDot, _), TgtFacet(dot)) :: _ => {
            log("  Pop(8): " ++ lstTgt.toString)
            pop(dim)
          }
          case _ => log("Unknown!!!")
        }

        last = flg

      }

    }

    cardinal

  }

}
