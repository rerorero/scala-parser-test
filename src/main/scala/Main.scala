
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

  trait SideEffectNode extends Node // 副作用を発生するNode
  trait Logger extends SideEffectNode
  trait EntitlementDatabase extends SideEffectNode

  case class Assign(name: String, e: Node) extends Node
  case class Var(name: String) extends Node
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

  case class Trace(e: Node) extends Logger

  case class IssueEntitlement(e: Node) extends EntitlementDatabase
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

  val functions = ("trace issue" split ' ')
  val controlStatements = ("if true false" split ' ')

  lexical.reserved ++= functions
  lexical.reserved ++= controlStatements 
  lexical.delimiters ++=  ("+ - * / ( ) { } , == = < > <> <= >=" split ' ')

  // parser
  type P[+T] = Parser[T]

  lazy val program      = rep(statement) ^^ (Program(_))

  lazy val statement    = expr | ifElse

  lazy val expr:P[Node] = assign | trace | issue

  lazy val trace      = "trace" ~> simpleExpr         ^^ (Trace(_))

  lazy val issue      = "issue" ~> simpleExpr         ^^ (IssueEntitlement(_))

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
// Contextはミュータブル
class Context() {
  type VarName = String
  type VarValue = Any
  type Environment = collection.immutable.Map[VarName, VarValue]

  var env: Environment = Map()

  override def toString = "Env: " + env.mkString(", ")

  def putVar(name:VarName, value:VarValue) : Unit = env += (name -> value)
  def getVar(name:VarName) : Option[VarValue] = env.get(name) 
}


trait Interpreter {
  def eval(e: AST.Node) : Any
}

class MyInterpreter (implicit logger:StdIoLogger, entitlementDb:EntitlementDAO) extends Interpreter{ self =>
  import AST._

  val context = new Context()

  // todo implicit使って SideEffectBehavior[T] に変換できないか？
  def sideEffectEval[T <: SideEffectNode](e: T) : Any = e match {
    case ee : Logger => logger.eval(ee, self)
    case ee : EntitlementDatabase => entitlementDb.eval(ee, self)
  }

  def eval(e: Node): Any = e match {
    case Program(nodes) => nodes foreach eval

    case Block(nodes) => nodes foreach eval

    case Assign(id, node) => context.putVar(id, eval(node))

    case Var(id) => context.getVar(id).getOrElse(error("Undefined var " + id))

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

    case ee : SideEffectNode =>  sideEffectEval(ee)
  }

  def execute(ast: Node): Unit = eval(ast)
}

//////////////////////////////////////////
// 副作用部分 ASTに依存しNodeごと1対1対応にしているが、ASTと分離することも可能
// 分離するとAST Node と IO との関係がパーサーに入り込むことになる。
//////////////////////////////////////////
trait SideEffectBehavior[A <: AST.SideEffectNode] // implicit 使ってsideEffectEvalをもっとかっこ良くできないか・・ 

class StdIoLogger extends SideEffectBehavior[AST.Logger] {
  def eval(e: AST.Logger, i: Interpreter) : Any = e match{
    case AST.Trace(v) => i.eval(v) match {
      case value : String => { println("trace :: " + value); "unko" }
      case value => { println("trace :: [" + value.getClass.toString + "] " + value.toString); "unkooo" }
    }
  }
}

class EntitlementActions[T] extends SideEffectBehavior[AST.EntitlementDatabase] {
  def eval(e: AST.EntitlementActions, i: Interpreter) : Any = e match{
    case AST.IssueEntitlement(v) => i.eval(v) match {
      case value => println("権利を発行します :: " + value.toString)
    }
  }

  def mater = CountIssueEntitlement(v)
}

////////////////////////////////////////////////////////////////////////////////
// 以上がライブラリ側で用意するもの。以降が呼び出し側での実装
////////////////////////////////////////////////////////////////////////////////
class MysqlEntitlementActions extends EntitlementActions[MysqlDAO] {
  def countIssuedTicket(dao: T, ticketId: Int) : Either[Int, String] = dao.countIssuedTicket(ticketId)
  def issueTicket(dao: T, ticketId: Int) : Either[Int, String] = dao.countIssuedTicket(ticketId)
}

//////////////////////////////////////////
// ダミーのDAO
//////////////////////////////////////////
class MysqlDAO {
  def countIssuedTicket(ticketId: Int) : Either[Int, String] = {
    println("DAO ::: countIssuedTicket "+ticketId)
    Right(1000)
  }
  def issueTicket(ticketId: Int) : Either[Int, String] = {
    println("DAO ::: issueTicket "+ticketId)
    Right(2525)
  }
}

//////////////////////////////////////////
// Main
//////////////////////////////////////////
object Main extends App {

  val sample1 = """
    x = 99
    trace x
    
    if (true) { 
      x = 100
      trace "that is true!"
    }

    issue "タイムシフト視聴権"

    trace x
  """

  val parser = MyParser

  // 副作用をインタプリターに注入
  implicit val logger = new StdIoLogger  
  implicit val entitlementDb = new EntitlementDAO

  val interpreter = new MyInterpreter

  val result = parser.parse(sample1) match {
    case Right(ast) => interpreter.execute(ast)
    case Left(msg) => println(msg)
  }

}

