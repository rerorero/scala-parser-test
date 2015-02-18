package mogi

import scala.util._
import scala.concurrent.duration.Duration
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

object main extends App{
  val sample1 = """
    x = 99
    trace x

    if (count "time shift shichouken" < 1000) {
      x = 100
      trace "that is true!"
    }

    issue "time shift shichouken"

    trace x
  """

  Parser.parse(sample1) match {
    case Parser.Success(ast, _) => {
      object resource extends EntitlementsDao with StdIoLogger

      val reader = EvalInterpreter(ast)

      val runner = new DefaultRunner
      val f = runner.run(resource, reader)
      f.onComplete {
        case Success(v) => println("complete:"+v)
        case Failure(e) => println("failure:"+e)
      }
      Await.result(f, Duration.Inf)
    }
    case Parser.NoSuccess(e) => println("parse error:" + e)
  }
}