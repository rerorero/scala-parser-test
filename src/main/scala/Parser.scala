package mogi

import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._

object Parser extends StandardTokenParsers {

  override val lexical = new StdLexical

  def parse(input: String) : ParseResult[AST[Any]] = phrase(program)(new lexical.Scanner(input))

  val functions = ("trace issue count" split ' ')
  val controlStatements = ("if true false" split ' ')

  lexical.reserved ++= functions
  lexical.reserved ++= controlStatements
  lexical.delimiters ++=  ("+ - * / ( ) { } , == = < > <> <= >=" split ' ')

  // parser
  type P[T] = Parser[AST[T]]

  def program      = rep(statement) ^^ (Program(_))

  def statement    = expr | ifElse

  // 式
  def expr:P[Any] = assign | trace | issue | count | compare

  def trace      = "trace" ~> simpleExpr         ^^ (Trace(_))

  def issue      = "issue" ~> simpleExpr         ^^ (IssueTicket(_))

  def count:P[Double]= "count" ~> simpleExpr         ^^ (CountTicket(_))

  def assign       = ident ~ ("=" ~> simpleExpr)     ^^ { case i ~ e => Assign(i, e) }

  def comparable = intVal | count
  def compare:P[Boolean]= comparable ~ ("<" | ">") ~ comparable ^^ { case l ~ op ~ r => Compare(l, r, op) }

  // 項目
  def simpleExpr   =  ( ident   ^^ Var
                      | intVal
                      | boolVal
                      | stringLit           ^^ Lit
                      | "(" ~> expr <~ ")"
                      | failure ("Expression expected")
                      )

  def intVal : P[Double] = numericLit          ^^ { x => Number(x.toDouble) }

  def boolVal : P[Boolean] = ("true" ^^^ BoolVal(true) | "false" ^^^ BoolVal(false))

  def block        = "{" ~> rep(expr) <~ "}"  ^^ (Block(_))

  def lhs : P[Boolean]   = boolVal | compare
  def ifClause           = "if" ~> "(" ~ lhs ~ ")" ~ (block | expr)  ^^ { case _ ~ ex ~ _ ~ bl => IfClause(ex, bl) }
  def elseClause         = "else" ~ (block | expr)                   ^^ { case _ ~ elem => ElseClause(elem) }
  def ifElse             = ifClause ~ opt(elseClause) ^^ { case e1 ~ e2 => IfElse(e1, e2) }
}
