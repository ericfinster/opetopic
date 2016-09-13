/**
  * Main.scala - Main Module for OpetopicTT in Scala
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ott

import java.io._

// import MinittSyntax._
// import TypeChecker._
// import scalaz._

// import MR._
// import ME.{raiseError, handleError}

object Main {

  def main(args: Array[String]) : Unit = {

    if (args.length != 1) {

      println("Usage: opetopictt <filename>")

    } else {

      println("Not done yet ...")

      // val fname = args(0)

      // println("Going to parse file: " + fname)

      // val reader = new FileReader(new File(fname))
      // val lexer = new MinittLexer(reader)
      // val parser = new MinittParser
      // parser.lexer = lexer

      // parser.parseAll match {
      //   case Right(Prog(dfs)) => {

      //     def abstractDef(d: DeclT) : (String, ExpT, ExpT) =
      //       d match {
      //         case Def(id, Nil, ty, tm) => (id, ty, tm)
      //         case Def(id, Tele(p, t) :: cs, ty, tm) => {
      //           val (tid, tty, ttm) = abstractDef(Def(id, cs, ty, tm))
      //           (tid, EPi(List(PTele(p, t)), tty), ELam(p, ttm))
      //         }
      //       }

      //     def checkDefs(defs: List[DeclT]): TCM[Unit] =
      //       defs match {
      //         case Nil => pure(())
      //         case d :: ds => {

      //           val (id, ty, tm) = abstractDef(d)

      //           println("Checking definition: " + id)

      //           for {
      //             ty0 <- check(ty, TypeD)
      //             tyD <- tcEval(ty0)
      //             tm0 <- check(tm, tyD)
      //             _ <- local(withDef(id, ty0, tyD, tm0))(checkDefs(ds))
      //           } yield ()
      //         }


      //       }

      //     checkDefs(dfs).run(TcEnv(Nil, RNil)).run match {
      //       case -\/(msg) => println("Typechecking error: " + msg)
      //       case \/-(_) => println("Success!")
      //     }

      //   }
      //   case Right(_) => println("Unknown error")
      //   case Left(s) => println("Parse error: " + s)
      // }

      // reader.close

    }

  }

}
