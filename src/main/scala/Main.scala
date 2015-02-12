package mogi

object main extends App{
  val sample1 = """
    x = 99
    trace x

    if (count "time shift shichouken" < 10) {
      x = 100
      trace "that is true!"
    }

    issue "time shift shichouken"

    trace x
  """

  val ast = Parser.parse(sample1)
  println(ast)
}
