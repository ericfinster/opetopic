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

  // Assumes the iterator is iterating in the "extrusion order", which
  // is with the reversed flag set to true.
  def extrudeFrom[A](itr: FlagIterator[A], dummy: A): SCardinal[A] = {

    var cardinal: SCardinal[A] =
      SCardinal(dummy)

    // Make sure we have at least one flag
    if (itr.hasNext) {

      val initFlag = itr.next
      val dim = initFlag.length

      println("Initial flag: " ++ initFlag.map(_.toString).mkString(" "))

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

        // print("Passing flag: " ++ flg.map(_.toString).mkString(" "))

        val (p, s) = (last, flg).zipped.span({
          case (f, g) => f == g
        })

        val (prefix, suffix) = (p.unzip._2.toList, s.unzip._2.toList)

        val dim = prefix.length

        s match {
          case (TgtFacet(lstTgt), TgtFacet(tgt)) :: (SrcFacet(lstDot, _), TgtFacet(dot)) :: _ => {
            // println("  Pop: " ++ lstTgt.toString)

            // This is the case of a possible extrusion.

            if (dim > 0) {

              pop(dim)

              val canExtrude = prefix.forall(f =>
                cells.contains(f.face))

              if (canExtrude && predMap.isDefinedAt(tgt)) {

                val srcSet = predMap(tgt)
                val rAddr = rootCardinalAddr(dim)
                
                println("Extrusion about " ++ srcSet.toString ++ " with (" ++ tgt.toString ++ ", " ++ dot.toString ++ ")")
                // println("Extrusion address is: " ++ rAddr.toString)

                val extrudeCardinal =
                  if (cardinal.dim < dim + 1)
                    cardinal.extend(dummy)
                  else
                    cardinal

                extrudeCardinal.extrude(rAddr, tgt, dot)(a => srcSet.contains(a)) match {
                  case None => println("Extrusion failed")
                  case Some(c) => {
                    println("Extrusion successful")
                    // println("Excised tree: " ++ c._2.toString)
                    cardinal = c._1
                  }
                }

                cells += (tgt, dot)

              }

            } else {

              println("Object extrusion (" ++ tgt.toString ++ ", " ++ dot.toString ++ ")")

              cardinal.extrudeObject(tgt, dot) match {
                case None => println("Object extrusion failed")
                case Some(c) => {
                  println("Object extrusion successful")
                  cardinal = c
                }
              }

              cells += (tgt, dot)

            }


          }
          case (SrcFacet(lstTgt, _), TgtFacet(tgt)) :: (TgtFacet(lstDot), TgtFacet(dot)) :: _ => {

            // println("  Loop: " ++ tgt.toString)  // Extrude a loop!
            println("Extruding loop with (" ++ tgt.toString ++ ", " ++ dot.toString ++ ")")

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
              case None => println("Loop extrusion failed.")
              case Some(c) => {
                println("Loop extrusion successful")
                cardinal = c
              }
            }

            cells += (tgt, dot)

          }
          case (TgtFacet(lstTgt), TgtFacet(tgt)) :: (TgtFacet(lstDot), SrcFacet(dot, _)) :: _ => {
            // println("  Push: " ++ tgt.toString)
            push(dim, tgt)
          }
          case (TgtFacet(lstTgt), SrcFacet(tgt, _)) :: (TgtFacet(lstDot), TgtFacet(dot)) :: _ => {
            // println("  Drop!  ")
          }
          case (TgtFacet(lstTgt), SrcFacet(tgt, _)) :: (SrcFacet(lstDot, _), SrcFacet(dot, _)) :: _ => {
            // println("  Pop: " ++ lstTgt.toString ++ " / Push: " ++ tgt.toString)

            pop(dim)
            push(dim, tgt)
          }
          case (SrcFacet(lstTgt, _), TgtFacet(tgt)) :: (SrcFacet(lstDot, _), SrcFacet(dot, _)) :: _ => {
            // println("  Pop: " ++ lstTgt.toString ++ " / Push: " ++ tgt.toString)
            pop(dim)
            push(dim, tgt)
          }
          case (SrcFacet(lstTgt, _), SrcFacet(tgt, _)) :: (TgtFacet(lstDot), SrcFacet(dot, _)) :: _ => {
            // println("  Push: " ++ tgt.toString)
            push(dim, tgt)
          }
          case (SrcFacet(lstTgt, _), SrcFacet(tgt, _)) :: (SrcFacet(lstDot, _), TgtFacet(dot)) :: _ => {
            // println("  Pop: " ++ lstTgt.toString)
            pop(dim)
          }
          case _ => println("Unknown!!!")
        }

        last = flg

      }

    }

    cardinal

  }

}
