/**
  * Main.scala - Main Module for OpetopicTT in Scala
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ott

import java.io._

import opetopic._
import mtl._

import OttSyntax._
import TypeChecker._

object Main {

  def main(args: Array[String]) : Unit = {

    if (args.length != 1) {

      println("Usage: opetopictt <filename>")

    } else {

      val fname = args(0)

      println("Going to parse file: " + fname)

      val reader = new FileReader(new File(fname))
      val lexer = new OttLexer(reader)

      // val action: Array[Int] = lexer.zzUnpackAction
      // println("Action array: " + action.map(_.toString).toList.toString)

      // val attr: Array[Int] = lexer.zzUnpackAttribute
      // println("Attribute array: " + attr.map(_.toString).toList.toString)

      val parser = new OttParser
      parser.lexer = lexer

      parser.parseAll match {
        case Right(Prog(dfs)) => {

          def abstractDef(d: DeclT) : (String, ExpT, ExpT) =
            d match {
              case Def(id, Nil, ty, tm) => (id, ty, tm)
              case Def(id, Tele(p, t) :: cs, ty, tm) => {
                val (tid, tty, ttm) = abstractDef(Def(id, cs, ty, tm))
                (tid, EPi(List(PTele(p, t)), tty), ELam(p, ttm))
              }
            }

          def checkDefs(defs: List[DeclT]): TCM[Unit] = {

            import tcmMonad._

            val test: MonadOps[TCM, Unit] = toMonadOps(pure(()))

            defs match {
              case Nil => pure(())
              case d :: ds => {

                val (id, ty, tm) = abstractDef(d)

                println("Checking definition: " + id)

                for {
                  ty0 <- check(ty, TypeD)
                  tyD <- tcEval(ty0)
                  tm0 <- check(tm, tyD)
                  _ <- local(withDef(id, ty0, tyD, tm0))(checkDefs(ds))
                } yield ()
               }
            }
          }

          checkDefs(dfs).run(TCEnv(Nil, RNil)) match {
            case Xor.Left(msg) => println("Typechecking error: " + msg)
            case Xor.Right(_) => println("Success!")
          }

        }
        case Right(_) => println("Unknown error")
        case Left(s) => println("Parse error: " + s)
      }

      reader.close

    }

  }

}

