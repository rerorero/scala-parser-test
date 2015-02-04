
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.token._
import scalaz._
import Scalaz._

// utilities
trait PrettyPrinters {
  // TODO: escape
  protected def _q(s: String): String = "\"" + s + "\""
}

//////////////////////////////////////////
// AST
//////////////////////////////////////////
object AST {

  trait Node extends PrettyPrinters

  case class Assign(name: String, e: Node) extends Node
  case class Var(name: String) extends Node
  case class PrintLn(e: Node) extends Node
  case class Program(l: Seq[Node]) extends Node

  case class Lit(v: Any) extends Node {
    override def toString = "NODE :: " + v match {
      case _:String => "\"" + v + "\""
      case _ => v.toString
    }
  }

  case class IfExpr(e1: Node, e2: Node, e3: Node) extends Node

  case class Block(l : Seq[Node]) extends Node

  case class IfClause(condition: Node, block: Node) extends Node
  case class ElseClause(block: Node) extends Node
  case class IfElse(ifElem: IfClause, elElem: Option[ElseClause]) extends Node

}


//////////////////////////////////////////
// Parser Base
//////////////////////////////////////////
trait MyParserBase extends StandardTokenParsers
class MyLexical extends StdLexical

//////////////////////////////////////////
// Parsing
//////////////////////////////////////////
object MyParser extends MyParserBase {

  import AST._

  override val lexical = new MyLexical

  // parse
  def parse(input: String) : Either[String, Node] = phrase(program)(new lexical.Scanner(input)) match {
    case Success(ast, _) => Right(ast)
    case e: NoSuccess => Left("parser error:" + e.msg)
  }

  val functions = ("println" split ' ')
  val controlStatements = ("if true false" split ' ')

  lexical.reserved ++= functions
  lexical.reserved ++= controlStatements 
  lexical.delimiters ++=  ("+ - * / ( ) { } , == = < > <> <= >=" split ' ')

  // parser
  type P[+T] = Parser[T]

  lazy val program      = rep(statement) ^^ (Program(_))

  lazy val statement    = expr | ifElse

  lazy val expr:P[Node] = assign | println

  lazy val println      = "println" ~> simpleExpr         ^^ (PrintLn(_))

  lazy val assign       = ident ~ ("=" ~> simpleExpr)     ^^ { case i ~ e => Assign(i, e) }

  lazy val simpleExpr   = ( ident               ^^ Var
                          | numericLit          ^^ { x => Lit(x.toInt) }
                          | stringLit           ^^ Lit
                          | "true"              ^^ { _ => Lit(true) }
                          | "false"             ^^ { _ => Lit(false) }
                          | "(" ~> expr <~ ")"
                          | failure ("Expression expected")
                          )

  lazy val block        = "{" ~> rep(expr) <~ "}"  ^^ (Block(_))

  lazy val ifClause           = "if" ~> "(" ~ simpleExpr ~ ")" ~ (block | expr)  ^^ { case _ ~ ex ~ _ ~ bl => IfClause(ex, bl) }
  lazy val elseClause         = "else" ~ (block | expr)                   ^^ { case _ ~ elem => ElseClause(elem) }
  lazy val ifElse             = ifClause ~ opt(elseClause) ^^ { case e1 ~ e2 => IfElse(e1, e2) }
}



//////////////////////////////////////////
// Interpreter 
//////////////////////////////////////////
object MyInterpreter {
  import AST._

  type VarName = String
  type VarValue = Any
  type Environment = collection.immutable.Map[VarName, VarValue]

  val initEnv: Environment = Map()

  class Context(private var env: Environment = initEnv) {

    override def toString = "Env: " + env.mkString(", ")

    def eval(e: Node): Any = e match {
      case Program(nodes) => nodes foreach eval

      case Block(nodes) => nodes foreach eval

      case Assign(id, node) => env += (id -> eval(node))

      case Var(id) => env getOrElse(id, error("Undefined var " + id))

      case PrintLn(v) => eval(v) match {
        case value : String => { println("println :: " + value); "unko" }
        case value => { println("println :: [" + value.getClass.toString + "] " + value.toString); "unkooo" }
      }

      case Lit(v) => v

      case IfElse(ifElem: IfClause,  elElem: Option[ElseClause]) => {
        val condVal = eval(ifElem.condition)
        if (isInstanceOf[Boolean]) {
          throw new Exception("Boolean condition expected." + condVal)
        }else{
          if (condVal.asInstanceOf[Boolean]) {
            eval(ifElem.block)
          }else{
            elElem map { x => eval(x.block) }
          }
        }
      }
    }
  }

  def eval(ast: Node): Unit = {
    (new Context) eval ast
  }
}

//////////////////////////////////////////
// 
//////////////////////////////////////////
sealed trait Entity
case class Entitlement(attrs: Seq[String]) extends Entity
case class Query(someQuery: String) 

sealed trait SideEffect[A]
 
object Logging {
  type SideEffectFree[A] = Free[SideEffect, A]
 
  // functorを定義するだけでモナドが出来上がるらしい
  implicit def sideEffectFunctor[B]: Functor[SideEffect] = new Functor[SideEffect]{
    def map[A,B](fa: SideEffect[A])(f: A => B): SideEffect[B] = 
      fa match {
        case SelectEntitlements(q,a) => SelectEntitlements(f compose q,f(a))
        case Info(msg,a)  => Info(msg,f(a))
        case Warn(msg,a)  => Warn(msg,f(a))
        case Error(msg,a) => Error(msg,f(a))
      }
  }
 
  implicit def logFToFree[A](f: SideEffect[A]): Free[SideEffect,A] = Free.liftF(f)
 

  case class SelectEntitlements[A](q: Query => A, o: A) extends SideEffect[A]
  case class Info[A](msg: String, o: A) extends SideEffect[A]
  case class Warn[A](msg: String, o: A) extends SideEffect[A]
  case class Error[A](msg: String, o: A) extends SideEffect[A]
 
  object log {

    def selectEntitlements(q: Query): SideEffectFree[Either[String,String]] = logFToFree[Either[String,String]] ( SelectEntitlements(
      { q => Right("Success!!") }
      , Right("dummy1"))
    )
    def info(msg: String): SideEffectFree[Either[String,String]]  = logFToFree(Info(msg, Right("dummy2")))
    def warn(msg: String): SideEffectFree[Either[String,String]]  = logFToFree(Warn(msg, (Right("dummy3"))))
    def error(msg: String): SideEffectFree[Either[String,String]] = logFToFree(Error(msg, (Right("dummy4"))))
  }
}

object SLF4J {
  import Logging._
 
  private val exe: SideEffect ~> Id = new (SideEffect ~> Id) {
    def apply[B](l: SideEffect[B]): B = l match { 
      case SelectEntitlements(q,a) => { println("DEBUG ::: ");  println(q); a } 
      case Info(msg,a) => { println("INFO ::: " + msg); println(a); a } 
      case Warn(msg,a) => { println("WARN ::: " + msg); println(a); a } 
      case Error(msg,a) => { println("ERROR ::: " + msg); println(a); a } 
    }
  }
 
  def apply[A](log: SideEffectFree[A]): A = 
    log.runM(exe.apply[SideEffectFree[A]])
}

//////////////////////////////////////////
// Main
//////////////////////////////////////////
object Main extends App {

  val sample1 = """
    x = 99
    println x

    if (true) { 
      x = 100
      println "that is true!"
    }

    println x
  """

  val parser = MyParser
  val interpreter = MyInterpreter

  val result = parser.parse(sample1) match {
    case Right(ast) => interpreter.eval(ast)
    case Left(msg) => println(msg)
  }

  println(result)

  //////////////////////
  val program: Free[SideEffect, Either[String,String]] = 
    for {
      x <- Logging.log.selectEntitlements(Query("some query dayo"))
      a <- Logging.log.info("fooo")
      b <- Logging.log.error("OH NOES")
    } yield b
 
  SLF4J(program)
}

