package mogi

object main extends App{
  val sample1 = """
    x = 99
    trace x

    if (count "time shift shichouken" < 101) {
      x = 100
      trace "that is true!"
    }

    issue "time shift shichouken"

    trace x
  """

  val ast = Parser.parse(sample1)
  println(ast)

  val interpreter = new EvalInterpreter

  ast match {
    case Right(as) => interpreter.eval(as)
    case Left(e) => println("error:"+e)
  }
}