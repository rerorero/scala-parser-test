import scala.util.parsing.combinator._
import util.parsing.combinator.syntactical.StandardTokenParsers

//////////////////////////////////////////
// Parser/Lexer Base
//////////////////////////////////////////
trait MyParserBase extends StandardTokenParsers 


//////////////////////////////////////////
// AST
//////////////////////////////////////////
object AST {
  sealed abstract class Expr

  case class Equal(e1: Expr, e2: Expr) extends Expr

}



//////////////////////////////////////////
// Parsing
//////////////////////////////////////////
object MyParser extends MyParserBase {
  import AST._

  def parse(input: String) : Either[String, Equal] = phrase(program)(new lexical.Scanner(input)) match {
    case Success(ast, _) => Right(ast)
    case e: NoSuccess => Left("parser error:" + e.msg)
  }

  lazy val program = "===" ^^^ Equal

}



//////////////////////////////////////////
// Interpreter 
//////////////////////////////////////////

object MyInterpreter {
}
